open Sexplib0.Sexp_conv

type foreign = {
  child : Schema.Name.t;
  parent : Schema.Name.t;
  columns : (Ident.t * Ident.t) * (Ident.t * Ident.t) list;
}
[@@deriving sexp_of]

type unique = { table : Schema.Name.t; columns : Ident.t * Ident.t list } [@@deriving sexp_of]
type range = { table : Schema.Name.t; column : Ident.t; low : int64; high : int64 } [@@deriving sexp_of]
type t = { foreign : foreign list; unique : unique list; ranges : range list } [@@deriving sexp_of]

let none = { foreign = []; unique = []; ranges = [] }
