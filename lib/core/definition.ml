type context = {
  charset : Ident.t;
  collation : Ident.t;
  sql_mode : string option;
  time_zone : string option;
}
[@@deriving sexp ~portable, yojson]

type kind =
  | View of { columns : Ident.t * Ident.t list }
  | Trigger of { table : Ident.t }
  | Procedure
  | Function
  | Event
[@@deriving sexp ~portable, yojson]

type t = {
  database : Ident.t;
  name : Ident.t;
  kind : kind;
  context : context;
  text : string;
}
[@@deriving sexp ~portable, yojson]
