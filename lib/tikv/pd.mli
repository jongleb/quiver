@@ portable

(** A client of PD, the cluster's placement driver, over gRPC: where every region of a range of
    keys is, with its epoch, leader and every replica. *)

type error =
  | Refused of { kind : int; message : string }  (** PD's error in a response header, by its type. *)
  | No_leader  (** PD named no leader of itself. *)
  | Url of string  (** A client URL of PD that is not [http://host:port]. *)
  | Unknown_store of int64  (** A peer on a store PD did not list. *)
  | Undecodable of { region : int64; key : string }  (** A bound that is not an order-keeping form. *)
  | Leaderless of int64  (** A region without a leader now, as during an election. *)
  | Gap of string  (** No region starts at this key, where the last one ended. *)
[@@deriving sexp_of ~portable]

type t

val connect :
  timeout:Core.Time_ns.Span.t ->
  string ->
  ( t,
    [> `Pd of error
    | `Region of Region.error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
(** To PD at [host:port], then to PD's leader, with the stores it lists. *)

val regions :
  t ->
  start:string ->
  stop:Kv.bound ->
  ( Region.t list,
    [> `Pd of error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
(** The regions holding keys from [start] up to [stop], in key order, each from its first key. *)

val scatter :
  t ->
  int64 list ->
  ( int64 list,
    [> `Pd of error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
(** Asks PD to spread these regions over the stores as one group; the regions it could not. *)

val stores : t -> string list
(** The addresses of the TiKV stores PD listed, in its order, without TiFlash's. *)

val close : t -> unit
