@@ portable

(** One thing to include in a dump. *)
type rule = Database of Ident.t | Table of Schema.Name.t
[@@deriving sexp_of, equal]

(** Which tables a dump covers. *)
type t = Everything | Only of rule * rule list

(** Which of the dump's tables give their rows; the others give only their
    schema. *)
type rows = All_rows | No_rows | Rows_of of rule * rule list

(** A kind of a database's objects. *)
type kind = Views | Triggers | Routines  (** Procedures and functions. *) | Events
[@@deriving sexp_of]

(** Which kinds of its databases' objects a dump takes. *)
type objects = Without_objects | With_objects of (kind * kind list) [@@deriving sexp_of]

val every_kind : kind * kind list

val kinds : objects -> kind list

val taken : objects -> (Ident.t * Definition.kind) list -> (Ident.t * Definition.kind) list
(** Those of a database's catalog the dump takes, in order. *)

type error = Unmatched_rows of rule  (** Names no table of the dump. *)
[@@deriving sexp_of]

type split = {
  with_rows : Schema.Table.t list;
  schema_only : Schema.Table.t list;
}
(** The dump's tables by what they give, each in its original order. *)

val split : rows -> Schema.Table.t list -> (split, [> `Filter of error ]) result
