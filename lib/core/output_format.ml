type dialect = Rfc4180 | Mysql_load_data
[@@deriving
  sexp ~portable,
  equal ~portable,
  yojson,
  to_string ~capitalize:"kebab-case" ~portable,
  enumerate]
type t = Sql | Csv of dialect [@@deriving sexp ~portable, equal ~portable, yojson]
