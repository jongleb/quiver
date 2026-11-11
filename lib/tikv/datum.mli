@@ portable

(** A value as TiDB writes it in a row of its first format ([codec.EncodeValue], not comparable):
    a flag, then its bytes. *)

val length : string -> int -> int option
(** The whole length of the value at this position, flag included; [None] when no value of the
    format starts there or it runs past the end. *)

val id : string -> int -> (int * int) option
(** The column id at this position, a signed varint, and where the value after it starts. *)

val of_id : int -> string
(** A column id as a row of the first format holds it. *)

val of_cell : Info.kind -> string -> string option
(** A value of the second format, as {!Cell.of_default} gives one, in the first; [None] for a
    kind this does not write. *)
