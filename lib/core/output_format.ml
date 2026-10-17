type dialect = Rfc4180 | Mysql_load_data [@@deriving sexp ~portable, equal ~portable, yojson]
type t = Sql | Csv of dialect [@@deriving sexp ~portable, equal ~portable, yojson]
