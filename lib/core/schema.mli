(** Fully qualified table name. *)
module Name : sig
  type t = { database : Ident.t; table : Ident.t }
  [@@deriving sexp ~portable, equal ~portable, compare ~portable, hash ~portable, yojson]

  val pp : Format.formatter -> t -> unit @@ portable
end

(** A column of a table. *)
module Field : sig
  type nullability = Nullable | Not_null [@@deriving sexp ~portable, equal ~portable, yojson]

  type t = { name : Ident.t; ltype : Ltype.t; nullability : nullability }
  [@@deriving sexp ~portable, equal ~portable, yojson]
end

type error =
  | No_fields of Name.t
  | Duplicate_field of { table : Name.t; field : Ident.t }
  | Unknown_key_field of { table : Name.t; field : Ident.t }
  | Nullable_key_field of { table : Name.t; field : Ident.t }
  | Duplicate_table of Name.t
[@@deriving sexp_of ~portable]

(** A table: its fields, the key it can be chunked by, and size estimates. *)
module Table : sig
  (** A key over fields ['f], never empty. *)
  type 'f key =
    | Primary of ('f * 'f list)
    | Unique of { index : Ident.t; fields : 'f * 'f list }
        (** A unique index over NOT NULL fields, used when there is no primary
            key. *)
    | No_key
  [@@deriving sexp ~portable, yojson]

  type estimate = { rows : int; bytes : int } [@@deriving sexp ~portable, yojson]
  (** Server-side estimates, not exact counts. *)

  type t = private {
    name : Name.t;
    fields : Field.t * Field.t list;  (** Written back to a target. *)
    generated : Field.t list;
        (** [STORED] generated columns read after [fields], never written back. *)
    key : Field.t key;
    estimate : estimate;
  }
  [@@deriving sexp_of ~portable, yojson_of]

  val columns : t -> Field.t list @@ portable
  (** A batch's columns: [fields], then [generated]. *)

  val t_of_yojson : Yojson.Safe.t -> t
  (** Checked as {!make} checks; raises as derived converters do. *)

  val t_of_sexp : Sexplib0.Sexp.t -> t @@ portable
  (** As [t_of_yojson]. *)

  val make :
    name:Name.t ->
    fields:Field.t list ->
    generated:Field.t list ->
    key:Ident.t key ->
    estimate:estimate ->
    (t, [> `Schema of error ]) result @@ portable
end

type t = private { tables : Table.t list } [@@deriving sexp_of ~portable, yojson_of]

val make : Table.t list -> (t, [> `Schema of error ]) result @@ portable
