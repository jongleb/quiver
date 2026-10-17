@@ portable

(** One thing to include in a dump. *)
type rule = Database of Ident.t | Table of Schema.Name.t
[@@deriving sexp_of, equal]

(** Which tables a dump covers. *)
type t = Everything | Only of rule * rule list
