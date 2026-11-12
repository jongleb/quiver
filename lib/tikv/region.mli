@@ portable

(** A region of TiKV: the keys it holds and where its leader and replicas are. *)

type replica = { peer : Kv.peer; address : string  (** [host:port] of the peer's store, its proxy's for TiFlash. *) }
[@@deriving sexp_of ~portable]

type t = {
  context : Kv.region;
  start : string;  (** The first key it holds. *)
  stop : Kv.bound;
  address : string;  (** [host:port] of the leader's store. *)
  replicas : replica list;  (** Every peer, the leader too, by peer id. *)
}
[@@deriving sexp_of ~portable]

type error =
  | Malformed of string option list  (** A row of another shape than asked for. *)
  | No_table of Quiver_core.Schema.Name.t
[@@deriving sexp_of ~portable]

val find : t list -> string -> t option
(** The region that holds the key. *)

val table_id :
  Quiver_mysql.Link.t ->
  Quiver_core.Schema.Name.t ->
  ( int64,
    [> `Region of error
    | `Link of Quiver_mysql.Link.error
    | `Wire of Quiver_mysql.Wire.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
