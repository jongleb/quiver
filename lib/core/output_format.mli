(** How a dump writes its data files. *)

(** Dialect of CSV data files. *)
type dialect =
  | Rfc4180  (** Header, CRLF, quotes doubled, bare [\N] for NULL. *)
  | Mysql_load_data
      (** For [LOAD DATA … ENCLOSED BY '"' ESCAPED BY '\\']: no header, LF. *)
[@@deriving sexp ~portable, equal ~portable, yojson]

type t = Sql | Csv of dialect [@@deriving sexp ~portable, equal ~portable, yojson]
