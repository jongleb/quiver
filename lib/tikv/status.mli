(** The HTTP status service of a TiDB server, for what it says a table is: the ids its columns and
    indexes have, which TiKV's keys and values carry. *)

type error =
  | Refused of { code : int; body : string }  (** An HTTP status other than 200. *)
  | Malformed of string  (** A response, or a table's description, not as TiDB gives them. *)
  | Address of string  (** Not [host:port]. *)
[@@deriving sexp_of ~portable]

(** What a table's rows are keyed by. *)
type handle =
  | Integer_key  (** Its integer primary key. *)
  | Common_key  (** Its clustered primary key of other columns. *)
  | Row_number  (** The hidden [_tidb_rowid]. *)
[@@deriving sexp_of ~portable]

type ids = { columns : (string * int) list; indexes : (string * int) list; handle : handle } [@@deriving sexp_of ~portable]
(** Each column's and index's id by its name in lower case, in TiDB's order. *)

val body : string -> (string, [> `Status of error ]) result
(** The body of a whole HTTP/1.1 response: by its length, in chunks, or up to the end. *)

val ids : string -> (ids, [> `Status of error ]) result
(** From a table's description in JSON. *)

val path : Quiver_core.Schema.Name.t -> string
(** Where the service describes the table. *)

val table :
  timeout:Core.Time_ns.Span.t ->
  string ->
  Quiver_core.Schema.Name.t ->
  (ids, [> `Status of error | `Transport of Quiver_mysql.Transport.error ]) result
(** The table's ids, from the service at [host:port] ([\[host\]:port] for IPv6). *)
