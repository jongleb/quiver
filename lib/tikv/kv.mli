@@ portable

(** TiKV's transactional read of raw keys and values ([tikvpb.Tikv/KvScan]). *)

type region = {
  id : int64;
  conf_ver : int64;  (** The region's epoch: changes with its peers. *)
  version : int64;  (** The region's epoch: changes with splits and merges. *)
  peer : int64;  (** The peer asked, its leader. *)
  store : int64;  (** The store the peer lives on. *)
}
[@@deriving sexp_of ~portable]
(** Where a request goes, as the caller last learned it. *)

type peer = { id : int64; store : int64 } [@@deriving sexp_of ~portable]

(** Where a range of keys ends. *)
type bound = Before of string | Endless [@@deriving sexp_of ~portable]

(** Which way keys are scanned: [Backward] gives the last key of the range first. *)
type direction = Forward | Backward

(** Why the region could not answer; the caller learns the region again and retries. *)
type region_error =
  | Not_leader of peer option  (** The leader, when the peer knows it. *)
  | Epoch_not_match  (** The region split or merged since. *)
  | Region_not_found
  | Server_is_busy of { reason : string; backoff_ms : int64 }
  | Other of { kind : int; message : string }
      (** Any other kind, by its field number in [errorpb.Error]. *)
[@@deriving sexp_of ~portable]

type lock = { primary : string; version : int64; key : string; ttl : int64 }
[@@deriving sexp_of ~portable]

(** Why keys could not be read at the version. *)
type key_error =
  | Locked of lock  (** A transaction not yet resolved holds a key. *)
  | Retryable of string
  | Abort of string
  | Other_key_error of int  (** By its field number in [kvrpcpb.KeyError]. *)
[@@deriving sexp_of ~portable]

type pair = { key : string; value : string } [@@deriving sexp_of ~portable]

type scanned =
  | Pairs of pair list  (** In key order. *)
  | Region_error of region_error
  | Key_error of key_error
[@@deriving sexp_of ~portable]

val message : (Buffer.t -> unit) -> string
(** The encoded protobuf the fields written by the function make. *)

val context : Buffer.t -> region -> unit
(** A [kvrpcpb.Context]'s fields for a request to the region: its id, epoch and peer, and
    [request_source]. *)

val region_error : string -> pos:int -> len:int -> (region_error, [> `Proto of Proto.error ]) result
(** An [errorpb.Error] in [len] bytes at [pos]. *)

val scan_request : region -> direction:direction -> start:string -> stop:bound -> version:int64 -> limit:int -> string
(** Keys from [start] up to [stop], as of [version], at most [limit] of them; low priority,
    not kept in TiKV's block cache. *)

val scanned : string -> (scanned, [> `Proto of Proto.error ]) result

val scan :
  Http2.t ->
  region ->
  direction:direction ->
  start:string ->
  stop:bound ->
  version:int64 ->
  limit:int ->
  ( scanned,
    [> `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result

(** What a region's leader said to splitting it. *)
type split =
  | Split of int64 list  (** The ids of every region it became, the first one too. *)
  | Split_refused of region_error

val split_request : region -> string list -> string
(** At these keys, raw as TiDB wrote them, in order, all inside the region. *)

val split_response : string -> (split, [> `Proto of Proto.error ]) result

val split :
  Http2.t ->
  region ->
  string list ->
  ( split,
    [> `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
(** The region cut at the keys, through its leader. *)
