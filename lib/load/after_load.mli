@@ portable

(** Statements the target runs once every table's rows and keys are in, before foreign
    keys, views and triggers: the place for a user's own [UPDATE]s. *)

type error =
  | Malformed of string  (** Not [DATABASE.TABLE=STATEMENT]. *)
  | Unused of string  (** Names no table of the dump. *)
[@@deriving sexp_of]

type rule : value mod contended portable [@@deriving sexp_of]

val rule : string -> (rule, [> `After_load of error | `Pattern of Quiver_core.Pattern.error ]) result
(** [DATABASE.TABLE=STATEMENT]; [{table}] in the statement stands for the table as the target
    names it, and the statement runs in the target's database of the table. *)

val statements :
  rule list ->
  Quiver_core.Schema.Name.t list ->
  into:(Quiver_core.Schema.Name.t -> Quiver_core.Schema.Name.t) ->
  ((Quiver_core.Schema.Name.t * (string * string list)) list, [> `After_load of error ]) result
(** Each table some rule names, as the target names it, with its statements in rule order;
    once every rule names a table. *)
