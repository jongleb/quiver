@@ portable

(** What ties a schema's columns together beyond their tables, and what their types allow beyond
    the [Ltype]: the constraints a mask must keep. *)

type foreign = {
  child : Schema.Name.t;
  parent : Schema.Name.t;
  columns : (Ident.t * Ident.t) * (Ident.t * Ident.t) list;
      (** Each column of the child with the parent's column it refers to, in key order. *)
}
[@@deriving sexp_of]
(** A foreign key, or a reference the user declares. *)

type unique = { table : Schema.Name.t; columns : Ident.t * Ident.t list } [@@deriving sexp_of]
(** The columns of a primary key or a unique index. *)

type range = { table : Schema.Name.t; column : Ident.t; low : int64; high : int64 } [@@deriving sexp_of]
(** An integer column that holds fewer values than its [Ltype]: [MEDIUMINT], [BIT]. *)

type t = { foreign : foreign list; unique : unique list; ranges : range list } [@@deriving sexp_of]

val none : t
