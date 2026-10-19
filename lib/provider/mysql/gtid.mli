type error =
  | Malformed of string  (** Not a GTID set in the server's syntax. *)
  | Tag_unsupported of Server.t  (** Tagged GTIDs exist from MySQL 8.4 on. *)
[@@deriving sexp_of ~portable]

(** A server UUID in canonical lowercase form. *)
module Uuid : sig
  type t = private string [@@deriving sexp_of ~portable]
end

(** A MySQL GTID tag, lowercase, matching [[a-z_][a-z0-9_]{0,31}]. *)
module Tag : sig
  type t = private string [@@deriving sexp_of ~portable]
end

(** Transaction numbers [first..last], [1 <= first <= last]. *)
module Interval : sig
  type t = private { first : int64; last : int64 } [@@deriving sexp_of ~portable]
end

type mysql_set = {
  uuid : Uuid.t;
  tag : Tag.t option;
  intervals : Interval.t list;
}
[@@deriving sexp_of ~portable]
(** One source's transactions, optionally under a tag. *)

type mariadb_position = { domain : int; server : int; sequence : int64 }
[@@deriving sexp_of ~portable]
(** The last transaction of one replication domain. *)

(** Executed transactions as reported by the server; empty when none. *)
type t = private Mysql of mysql_set list | Mariadb of mariadb_position list
[@@deriving sexp_of ~portable]

val of_server_string : Server.t -> string -> (t, [> `Gtid of error ]) result
val to_string : t -> string
val yojson_of_t : t -> Yojson.Safe.t
val t_of_yojson : Yojson.Safe.t -> t
