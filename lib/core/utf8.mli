@@ portable

(** UTF-8 text read code point by code point, a broken byte counting as one point. *)

val decode : string -> int -> int * int
(** The bytes and the code point at a byte position; [-1] for a broken byte. *)

val points : string -> int
(** How many code points the text has. *)

val offset : string -> int -> int
(** The byte position of a code point, or the text's length past its last point. *)
