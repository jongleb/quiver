@@ portable

(** What a rule puts in place of a column's values. A NULL stays NULL under every
    action but [Null] and [Source]. *)
type action =
  | Null  (** NULL in every row. *)
  | Text of string  (** This value in every row: text, binary, an [ENUM] member or [SET] members. *)
  | Shape
      (** Each letter and digit replaced by a keyed one of the same kind (Latin, Cyrillic, digit),
          any other letter by a Latin one, the rest kept: as many characters, never more bytes. *)
  | Email  (** [Shape] before the last [@], the domain kept. *)
  | Hash
      (** A keyed digest: as many hex digits as the text has characters, as many bytes as the
          binary value, any integer of the column's type. *)
  | Source of string  (** This SQL expression, evaluated by the source server for each row. *)
  | Expression of Mask_script.expression
      (** This expression of the masking language, evaluated here for each row. *)
  | Keep  (** The values as read: taken, so that a later rule and [strict] see it handled. *)
  | Custom of { name : string; text : string -> string @@ portable }
      (** For a library: this function of each text or binary value. *)

type error =
  | Malformed of string @@ aliased  (** Not [DATABASE.TABLE.COLUMN=ACTION]. *)
  | Unknown_action of string @@ aliased
  | Unused of string @@ aliased
      (** No column of the dump takes the rule: it names none, or earlier rules take them all. *)
  | Key_column of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
      (** A column of the key the table is read and ordered by. *)
  | Generated_column of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
      (** Computed by the target, never written back. *)
  | Generated_values of Schema.Name.t @@ aliased
      (** The table's [STORED] generated columns are read from the source, and would keep the
          values of the columns masked. *)
  | Not_nullable of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
  | Unfit of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      action : string @@ aliased;
      ltype : Ltype.t @@ aliased;
    }
      (** The action gives no value of the column's type. *)
  | Not_member of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      value : string @@ aliased;
    }
      (** A [Text] that is no member of the column's [ENUM] or [SET]. *)
  | Scripted of {
      table : Schema.Name.t @@ aliased;
      column : Ident.t @@ aliased;
      error : Mask_script.error @@ aliased;
    }
      (** The column's expression does not type for it. *)
  | Null_from_source of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
      (** The source's expression gave NULL for a NOT NULL column. *)
  | Null_from_expression of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
      (** The column's expression gave NULL for a NOT NULL column. *)
  | Mismatched_column of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
      (** A batch column of another type than the table's. *)
  | Rewritten of Column.error @@ aliased
  | Unhandled of { table : Schema.Name.t @@ aliased; columns : Ident.t list @@ aliased }
      (** Columns of a strict table no rule takes, neither masked nor kept. *)
  | Finer_than_column of { table : Schema.Name.t @@ aliased; column : Ident.t @@ aliased }
      (** An expression's time has more fractional digits than the column keeps. *)
[@@deriving sexp_of]

type rule [@@deriving sexp_of]
(** The columns a pattern names and the action they take. *)

val rule :
  string -> (rule, [> `Mask of error | `Mask_script of Mask_script.error | `Pattern of Pattern.error ]) result
(** [DATABASE.TABLE.COLUMN=ACTION], where [ACTION] is [null], [text:VALUE], [shape], [email],
    [hash], [keep], [sql:EXPRESSION] or [expr:EXPRESSION] of {!Mask_script}. *)

val scripted : Mask_script.t -> (rule list, [> `Pattern of Pattern.error ]) result
(** The script's [(mask …)] and [(keep …)] forms, in order; a [where] selector takes only
    columns of its types. *)

val custom :
  string ->
  name:string ->
  (string -> string) @ portable ->
  (rule, [> `Pattern of Pattern.error ]) result
(** The columns [DATABASE.TABLE.COLUMN] names, each text or binary value through the function. *)

type t
(** Rules in order: a column takes the first that names it. *)

val make : Keyed.t -> script:Mask_script.t -> strict:(string * Pattern.table) list -> rule list -> t
(** Expressions call the definitions of [script]; every column of a [strict] table, named as
    written, must be taken by a rule. *)
val none : t

(** The masking of one table's batches. *)
module Table : sig
  type t : value mod contended portable

  val none : t

  val sources : t -> (Ident.t * string) list
  (** The fields the source evaluates, with their expressions. *)

  val masked : t -> (Ident.t * string) list
  (** Each masked field and the name of its action. *)

  val apply : t -> Batch.sealed @ local -> (unit, [> `Mask of error ]) result @ unique
  (** Rewrites the masked columns in place. *)
end

val resolve : t -> Schema.Table.t list -> (Table.t list, [> `Mask of error ]) result
(** Each table's masking, in order, once every rule takes a column. *)
