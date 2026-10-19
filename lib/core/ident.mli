type error = Empty | Contains_nul of string | Invalid_utf8 of string
[@@deriving sexp_of ~portable]

type t = private string [@@deriving sexp_of ~portable, equal ~portable, compare ~portable, hash ~portable]
(** A database, table, column or index name: non-empty UTF-8 without NUL.
    Provider-specific limits are checked by the provider. *)

val of_string : string -> (t, [> `Ident of error ]) result @@ portable
val backquoted : t -> string @@ portable
val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
val t_of_sexp : Sexplib0.Sexp.t -> t @@ portable
