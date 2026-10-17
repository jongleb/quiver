@@ portable

(** What a provider shows the creating statement of. *)
type t = Database of Ident.t | Table of Schema.Name.t [@@deriving sexp_of]
