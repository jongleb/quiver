type rule = Database of Ident.t | Table of Schema.Name.t
[@@deriving sexp_of, equal]

type t = Everything | Only of rule * rule list
