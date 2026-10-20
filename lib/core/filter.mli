@@ portable

(** One thing to include in a dump. *)
type rule = Database of Ident.t | Table of Schema.Name.t
[@@deriving sexp_of, equal]

(** Which tables a dump covers. *)
type t = Everything | Only of rule * rule list

(** Which of the dump's tables give their rows; the others give only their
    schema. *)
type rows = All_rows | No_rows | Rows_of of rule * rule list

(** Whether a dump takes its databases' views, triggers, routines and events. *)
type objects = With_objects | Without_objects

type error = Unmatched_rows of rule  (** Names no table of the dump. *)
[@@deriving sexp_of]

type split = {
  with_rows : Schema.Table.t list;
  schema_only : Schema.Table.t list;
}
(** The dump's tables by what they give, each in its original order. *)

val split : rows -> Schema.Table.t list -> (split, [> `Filter of error ]) result
