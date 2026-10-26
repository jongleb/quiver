(** YAML 1.2 character classification. Functions that answer questions about
    Unicode codepoints used throughout the scanner. All codepoint values come
    from YAML 1.2.2 §5.

    The Reader uses [-1] as the end-of-input sentinel; many predicates treat it
    as false (not in any character class). *)

(** End-of-input sentinel returned by the Reader at end of stream. *)
let eof = -1

(** True for the ASCII null character (U+0000). Note: null is allowed inside
    double-quoted scalars but forbidden in bare content. *)
let is_null cp = cp = 0x00

(** True for any YAML line-break character (post-normalization these are all
    U+000A, but we test for the originals too for safety). *)
let is_linebreak cp =
  cp = 0x0A (* LF / newline *)
  || cp = 0x0D (* CR *) || cp = 0x85
  (* NEL *) || cp = 0x2028 (* LS *)
  || cp = 0x2029 (* PS *)

(** True for space or tab (ASCII white space). *)
let is_space cp = cp = 0x20 (* SPC *) || cp = 0x09 (* TAB *)

(** True for a non-line-break white space character (SPC or TAB). *)
let is_blank cp = cp = 0x20 || cp = 0x09

(** True for white space or a line break. *)
let is_white cp = is_blank cp || is_linebreak cp

(** True if the character may begin an anchor or alias name. Per YAML 1.2
    §7.2.2: everything except flow indicators, whitespace and the special
    characters '\[', '\]', [,], [#]. *)
let is_anchor_char cp =
  cp > 0x20 && cp <> 0x2C (* , *) && cp <> 0x5B
  (* [ *) && cp <> 0x5D (* ] *)
  && cp <> 0x7B (* { *) && cp <> 0x7D
  (* } *) && cp <> eof

(** True for characters that may appear in a YAML tag (URI characters plus [!]).
    Covers both tag handles and tag suffixes. *)
let is_tag_char cp =
  cp > 0x20 && cp <> 0x2C (* , *) && cp <> 0x5B
  (* [ *) && cp <> 0x5D (* ] *)
  && cp <> 0x7B (* { *) && cp <> 0x7D
  (* } *) && cp <> eof

(** True for characters that may appear in a YAML directive parameter. *)
let is_directive_char cp = cp <> 0x0A && cp <> 0x0D && cp <> eof

(** True if [cp] is an ASCII letter [a-zA-Z]. *)
let is_letter cp = (cp >= 0x41 && cp <= 0x5A) || (cp >= 0x61 && cp <= 0x7A)

(** True if [cp] is an ASCII decimal digit [0-9]. *)
let is_digit cp = cp >= 0x30 && cp <= 0x39

(** True if [cp] is a hexadecimal digit [0-9a-fA-F]. *)
let is_hex_digit cp =
  is_digit cp || (cp >= 0x41 && cp <= 0x46) || (cp >= 0x61 && cp <= 0x66)

(** Decode a hex digit to its integer value (0–15). *)
let hex_value cp =
  if cp >= 0x30 && cp <= 0x39 then cp - 0x30
  else if cp >= 0x41 && cp <= 0x46 then cp - 0x41 + 10
  else cp - 0x61 + 10

(** True if [cp] is a flow indicator character: [[, ], {, }]. *)
let is_flow_indicator cp =
  cp = 0x5B (* [ *) || cp = 0x5D
  (* ] *) || cp = 0x7B (* { *)
  || cp = 0x7D (* } *)

(** True if [cp] can start a plain (unquoted) scalar in any context. *)
let can_start_plain_block cp =
  cp <> eof && cp <> 0x2C (* , *) && cp <> 0x23
  (* # *) && cp <> 0x26 (* & *)
  && cp <> 0x2A (* * *) && cp <> 0x3F
  (* ? *) && cp <> 0x3A (* : *)
  && cp <> 0x2D (* - *) && cp <> 0x3E
  (* > *) && cp <> 0x21
  (* ! *) && cp <> 0x25 (* % *)
  && cp <> 0x40 (* @ *) && cp <> 0x60 (* ` *)
  && (not (is_flow_indicator cp))
  && not (is_white cp)

(** True if [cp] can start a plain scalar in flow context (excludes [,] and flow
    indicators). *)
let can_start_plain_flow cp = can_start_plain_block cp && cp <> 0x2C (* , *)

(** True if [cp] is safe to continue a plain scalar in block context.
    (Continuation rules differ slightly from start rules; for example, [-], [?],
    [:] are allowed when not followed by whitespace.) *)
let can_continue_plain_block cp =
  cp <> eof
  && (not (is_white cp))
  && cp <> 0x2C (* , *) && cp <> 0x23 (* # *)
  && not (is_flow_indicator cp)

(** True if [cp] is safe to continue a plain scalar in flow context. *)
let can_continue_plain_flow cp =
  can_continue_plain_block cp && cp <> 0x3A (* : *)

(** Return the printable representation of a codepoint for error messages. *)
let show cp =
  if cp = eof then "<end-of-input>"
  else if cp < 0x20 then Printf.sprintf "U+%04X" cp
  else if cp < 0x80 then Printf.sprintf "%c" (Char.chr cp)
  else Printf.sprintf "U+%04X" cp
