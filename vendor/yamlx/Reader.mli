(** UTF-8 input reader for the YAML scanner. Decodes the input string into an
    array of Unicode codepoints (int values), skips an optional byte-order mark
    (BOM, U+FEFF) at the start, and normalizes all line endings to LF (U+000A):
    * CR+LF (\r\n) → LF * CR (\r) → LF * NEL (\x85) → LF * LS (\u2028) → LF * PS
    (\u2029) → LF Tracks the current position (codepoint index, line, column)
    and exposes a small lookahead interface used by the Scanner. *)

val eof : int
(** End-of-input sentinel. Returned by [peek] when past the end. *)

type t

val of_string : string -> t
(** Create a Reader from a UTF-8 string. *)

val length : t -> int
(** Total number of codepoints in the input. *)

val at_end : t -> bool
(** True when there are no more characters to read. *)

val pos : t -> Types.pos
(** The current position as a [Types.pos]. *)

val peek : t -> int -> int
(** Look ahead without consuming. [peek r 0] is the current character. Returns
    [eof] past the end of input. *)

val advance : t -> int -> unit
(** Advance by [n] codepoints, updating line and column. *)

val read : t -> int
(** Consume and return the current codepoint. Returns [eof] at end. *)

val peek_string : t -> int -> string
(** Return the next [n] codepoints as a UTF-8 string without consuming them. *)

val read_string : t -> int -> string
(** Consume [n] codepoints and return them as a UTF-8 string. *)

val prefix_is : t -> string -> bool
(** True if the next codepoints match the given ASCII string exactly. *)

val encode_utf8_to : Buffer.t -> int -> unit
(** Encode a Unicode codepoint as UTF-8 bytes appended to [buf]. *)
