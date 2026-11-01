(** Collations of the source a target lacks, each created as another of the same character set
    ([--collation FROM=TO]): the data stays as it is, only definitions name the other collation,
    so text compares and sorts by it on the target. *)
type t : immutable_data

type error =
  | Malformed of string  (** Not [FROM=TO] of two collation names. *)
  | Repeated of string  (** A collation mapped twice. *)
  | Other_charset of { from : string; into : string }  (** The two character sets differ. *)
[@@deriving sexp_of ~portable]

val of_rules : string list -> (t, [> `Collations of error ]) result @@ portable

val rewrite : t -> string -> string @@ portable
(** Every word naming a mapped collation, in any case, outside quotes and plain comments
    whether or not a backslash escapes inside quotes, becomes its target. *)

val definition : t -> Quiver_core.Definition.t -> Quiver_core.Definition.t @@ portable
(** Its text and the collation it was created under. *)
