@@ portable

(** The placement policies in a table's definition as TiDB's [SHOW CREATE TABLE] gives it: its
    comments [/*T![placement] PLACEMENT POLICY=`name` */], on the table and on partitions, read
    outside quoted text. *)

type error = Unreadable of string  (** A placement comment not naming one policy. *)
[@@deriving sexp_of ~portable]

type t
(** A definition, its placement comments found. *)

val read : string -> (t, [> `Placement of error ]) result

val policies : t -> string list
(** Each policy named, once, in the order first named. *)

val without : t -> string
(** The definition without its placement comments, nor the space before each. *)
