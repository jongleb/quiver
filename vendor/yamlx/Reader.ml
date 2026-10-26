(** UTF-8 input reader for the YAML scanner. Decodes the input string into an
    array of Unicode codepoints (int values), skips an optional byte-order mark
    (BOM, U+FEFF) at the start, and normalizes all line endings to LF (U+000A):
    * CR+LF (\r\n) → LF * CR (\r) → LF * NEL (\x85) → LF * LS (\u2028) → LF * PS
    (\u2029) → LF Tracks the current position (codepoint index, line, column)
    and exposes a small lookahead interface used by the Scanner. *)

open Types

(** End-of-input sentinel. Returned by [peek] when past the end. *)
let eof = Char_class.eof

(* ------------------------------------------------------------------ *)
(* UTF-8 decoding                                                       *)
(* ------------------------------------------------------------------ *)

(** Decode a UTF-8 string to an array of normalized Unicode codepoints in a
    single pass. Combines UTF-8 decoding, BOM stripping, and line-ending
    normalization (CR+LF → LF, bare CR → LF, NEL/LS/PS → LF). Raises
    [Types.Error (Types.Scan_error _)] on invalid UTF-8. *)
let decode_utf8 (s : string) : int array =
  let n = String.length s in
  (* Upper bound: one codepoint per byte; CR+LF pairs collapse to one *)
  let buf = Array.make n 0 in
  let j = ref 0 in
  let i = ref 0 in
  let pos () = { Types.zero_pos with offset_bytes = !i } in
  let first = ref true in
  while !i < n do
    let b0 = Char.code s.[!i] in
    let cp, width =
      if b0 land 0x80 = 0 then
        (* 0xxxxxxx – 1-byte (ASCII) *)
        (b0, 1)
      else if b0 land 0xE0 = 0xC0 then begin
        (* 110xxxxx 10xxxxxx – 2-byte *)
        if !i + 1 >= n then
          Types.scan_error (pos ()) "truncated UTF-8 sequence at byte offset %d"
            !i;
        let b1 = Char.code s.[!i + 1] in
        (((b0 land 0x1F) lsl 6) lor (b1 land 0x3F), 2)
      end
      else if b0 land 0xF0 = 0xE0 then begin
        (* 1110xxxx 10xxxxxx 10xxxxxx – 3-byte *)
        if !i + 2 >= n then
          Types.scan_error (pos ()) "truncated UTF-8 sequence at byte offset %d"
            !i;
        let b1 = Char.code s.[!i + 1] in
        let b2 = Char.code s.[!i + 2] in
        ( ((b0 land 0x0F) lsl 12) lor ((b1 land 0x3F) lsl 6) lor (b2 land 0x3F),
          3 )
      end
      else if b0 land 0xF8 = 0xF0 then begin
        (* 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx – 4-byte *)
        if !i + 3 >= n then
          Types.scan_error (pos ()) "truncated UTF-8 sequence at byte offset %d"
            !i;
        let b1 = Char.code s.[!i + 1] in
        let b2 = Char.code s.[!i + 2] in
        let b3 = Char.code s.[!i + 3] in
        ( ((b0 land 0x07) lsl 18)
          lor ((b1 land 0x3F) lsl 12)
          lor ((b2 land 0x3F) lsl 6)
          lor (b3 land 0x3F),
          4 )
      end
      else
        Types.scan_error (pos ()) "invalid UTF-8 byte 0x%02X at byte offset %d"
          b0 !i
    in
    i := !i + width;
    (* Strip leading BOM (U+FEFF) *)
    if !first && cp = 0xFEFF then first := false
    else begin
      first := false;
      (* Normalize line endings to LF *)
      let norm =
        if cp = 0x0D then begin
          (* CR: consume a following LF if present (LF is ASCII, always 1 byte) *)
          if !i < n && Char.code s.[!i] = 0x0A then i := !i + 1;
          0x0A
        end
        else if cp = 0x85 || cp = 0x2028 || cp = 0x2029 then 0x0A
        else cp
      in
      buf.(!j) <- norm;
      incr j
    end
  done;
  Array.sub buf 0 !j

(** Encode a Unicode codepoint as UTF-8 bytes appended to [buf]. *)
let encode_utf8 (buf : Buffer.t) (cp : int) : unit =
  if cp <= 0x7F then Buffer.add_char buf (Char.chr cp)
  else if cp <= 0x7FF then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else if cp <= 0xFFFF then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end

(* ------------------------------------------------------------------ *)
(* Reader type                                                           *)
(* ------------------------------------------------------------------ *)

type t = {
  buf : int array;  (** normalized Unicode codepoints *)
  mutable idx : int;  (** current codepoint index in [buf] *)
  mutable byte_idx : int;  (** corresponding UTF-8 byte offset *)
  mutable line : int;  (** 1-based line number *)
  mutable column : int;  (** 0-based codepoint column (since last LF) *)
  mutable column_bytes : int;  (** 0-based UTF-8 byte column (since last LF) *)
}

(** Number of UTF-8 bytes needed to encode codepoint [cp]. *)
let utf8_length cp =
  if cp <= 0x7F then 1
  else if cp <= 0x7FF then 2
  else if cp <= 0xFFFF then 3
  else 4

(** Reject non-UTF-8 BOMs before attempting to decode. Called on the raw input
    bytes before [decode_utf8] so the error message is clear rather than a
    cryptic "invalid byte" from the UTF-8 decoder. *)
let check_encoding (s : string) : unit =
  let b n = if n < String.length s then Char.code s.[n] else -1 in
  (* Check 4-byte BOMs first (UTF-32 LE starts with the same two bytes as
     UTF-16 LE, so the longer pattern must win). *)
  if b 0 = 0x00 && b 1 = 0x00 && b 2 = 0xFE && b 3 = 0xFF then
    Types.scan_error Types.zero_pos
      "input appears to be UTF-32 BE (BOM detected); only UTF-8 is supported"
  else if b 0 = 0xFF && b 1 = 0xFE && b 2 = 0x00 && b 3 = 0x00 then
    Types.scan_error Types.zero_pos
      "input appears to be UTF-32 LE (BOM detected); only UTF-8 is supported"
  else if b 0 = 0xFE && b 1 = 0xFF then
    Types.scan_error Types.zero_pos
      "input appears to be UTF-16 BE (BOM detected); only UTF-8 is supported"
  else if b 0 = 0xFF && b 1 = 0xFE then
    Types.scan_error Types.zero_pos
      "input appears to be UTF-16 LE (BOM detected); only UTF-8 is supported"

(** Create a Reader from a UTF-8 string. *)
let of_string (s : string) : t =
  check_encoding s;
  let buf = decode_utf8 s in
  { buf; idx = 0; byte_idx = 0; line = 1; column = 0; column_bytes = 0 }

(** Total number of codepoints in the input. *)
let length (r : t) : int = Array.length r.buf

(** True when there are no more characters to read. *)
let at_end (r : t) : bool = r.idx >= Array.length r.buf

(** The current position as a [Types.pos]. *)
let pos (r : t) : pos =
  {
    line = r.line;
    column = r.column;
    column_bytes = r.column_bytes;
    offset = r.idx;
    offset_bytes = r.byte_idx;
  }

(** Look ahead without consuming. [peek r 0] is the current character. Returns
    [eof] past the end of input. *)
let peek (r : t) (ahead : int) : int =
  let i = r.idx + ahead in
  if i >= Array.length r.buf then eof else r.buf.(i)

(** Advance by [n] codepoints, updating all position fields. *)
let advance (r : t) (n : int) : unit =
  let limit = Array.length r.buf in
  for _ = 1 to n do
    if r.idx < limit then begin
      let cp = r.buf.(r.idx) in
      let blen = utf8_length cp in
      r.idx <- r.idx + 1;
      r.byte_idx <- r.byte_idx + blen;
      if cp = 0x0A then begin
        (* LF resets both column counters *)
        r.line <- r.line + 1;
        r.column <- 0;
        r.column_bytes <- 0
      end
      else begin
        r.column <- r.column + 1;
        r.column_bytes <- r.column_bytes + blen
      end
    end
  done

(** Consume and return the current codepoint. Returns [eof] at end. *)
let read (r : t) : int =
  let cp = peek r 0 in
  if cp <> eof then advance r 1;
  cp

(** Return the next [n] codepoints as a UTF-8 string without consuming them. *)
let peek_string (r : t) (n : int) : string =
  let buf = Buffer.create (n * 2) in
  let limit = min (r.idx + n) (Array.length r.buf) in
  let i = ref r.idx in
  while !i < limit do
    encode_utf8 buf r.buf.(!i);
    incr i
  done;
  Buffer.contents buf

(** Consume [n] codepoints and return them as a UTF-8 string. *)
let read_string (r : t) (n : int) : string =
  let s = peek_string r n in
  advance r n;
  s

(** True if the next [n] codepoints equal the given string (ASCII fast path). *)
let prefix_is (r : t) (s : string) : bool =
  let n = String.length s in
  let limit = Array.length r.buf in
  let ok = ref (r.idx + n <= limit) in
  let i = ref 0 in
  while !ok && !i < n do
    let cp = r.buf.(r.idx + !i) in
    if cp <> Char.code s.[!i] then ok := false;
    incr i
  done;
  !ok

(** [encode_utf8] exposed for use by other modules when building strings. *)
let encode_utf8_to = encode_utf8
