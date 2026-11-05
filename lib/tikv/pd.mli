@@ portable

(** A client of PD, the cluster's placement driver, over gRPC: where every region of a range of
    keys is, with its epoch, leader and every replica. *)

type error =
  | Refused of { kind : int; message : string }  (** PD's error in a response header, by its type. *)
  | No_leader  (** PD named no leader of itself. *)
  | Url of string  (** A client URL of PD that is not [http://host:port] or [https://host:port]. *)
  | Unknown_store of int64  (** A peer on a store PD did not list. *)
  | Undecodable of { region : int64; key : string }  (** A bound that is not an order-keeping form. *)
  | Leaderless of int64  (** A region without a leader now, as during an election. *)
  | Gap of string  (** No region starts at this key, where the last one ended. *)
  | Gc_passed of { version : int64; least : int64 }
      (** GC may already have passed the version: the least version the cluster keeps is later. *)
[@@deriving sexp_of ~portable]

(** What GC is asked about a service's version. *)
type hold =
  | Hold_for of Core.Time_ns.Span.t  (** Keep what a read at it needs, for this long more. *)
  | Let_go  (** No longer. *)

type t

val connect :
  Http2.dial ->
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

val keep :
  t ->
  service:string ->
  hold ->
  int64 ->
  ( unit,
    [> `Pd of error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
(** Asks GC to keep, under the service's name, what a read at this version needs ([Gc_passed]
    when it is no longer sure to be there), or to let it go. *)

val held :
  every:Core.Time_ns.Span.t ->
  (hold -> (unit, 'e) result) @ portable ->
  (unit -> ('a, 'e) result) @ local once ->
  ('a, 'e) result
(** The work run with its version held from before it, asked again [every] while it runs, asked
    once more after it, which fails the work when refused (a hold that lapsed meanwhile shows
    there: GC never keeps below what it passed), and let go at the end; held for [3 * every] at a
    time. *)

val stores : t -> string list
(** The addresses of the TiKV stores PD listed, in its order, without TiFlash's. *)

val close : t -> unit
