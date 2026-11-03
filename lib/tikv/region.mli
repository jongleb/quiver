@@ portable

(** A region of TiKV as TiDB reports it: the keys it holds and where its leader is. *)

type replica = { peer : Kv.peer; address : string  (** [host:port] of the peer's store. *) }
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
  | Undecodable of { region : int64; key : string }  (** A bound that is not hex of an order-keeping form. *)
  | No_leader of int64  (** A region without a leader now, as during an election. *)
  | No_table of Quiver_core.Schema.Name.t
[@@deriving sexp_of ~portable]

val find : t list -> string -> t option
(** The region that holds the key. *)

val of_rows :
  regions:string option array list ->
  peers:string option array list ->
  (t list, [> `Region of error ]) result
(** From the rows of [Table_regions] and [Region_peers]: each region once, by its first key. *)

val table_id :
  Quiver_mysql.Link.t ->
  Quiver_core.Schema.Name.t ->
  ( int64,
    [> `Region of error
    | `Link of Quiver_mysql.Link.error
    | `Wire of Quiver_mysql.Wire.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result

val of_table :
  Quiver_mysql.Link.t ->
  Quiver_core.Schema.Name.t ->
  ( int64 * t list,
    [> `Region of error
    | `Link of Quiver_mysql.Link.error
    | `Wire of Quiver_mysql.Wire.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
(** The table's id and the regions holding its rows: TiDB asks PD for the range of rows only, so
    a region of the table's indexes alone is not among them. *)
