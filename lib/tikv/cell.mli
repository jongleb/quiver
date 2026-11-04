@@ portable

(** A column's value as TiDB writes it in a row of its second format, made from the text TiDB keeps
    a default in. *)

val of_default : Info.kind -> string -> string option
(** The bytes of the column's default in a row; [None] for a kind or a text this does not write. *)
