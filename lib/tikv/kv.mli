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

(** How a transaction commits; an async-commit primary lock lists the other locks' keys. *)
type protocol = Two_phase | Async_commit of string list [@@deriving sexp_of ~portable]

type lock = {
  primary : string;  (** The key of the transaction's primary lock, which decides it. *)
  version : int64;  (** The transaction's start. *)
  key : string;
  ttl : int64;  (** Milliseconds after its start the lock may be held. *)
  min_commit_ts : int64;
  protocol : protocol;
}
[@@deriving sexp_of ~portable]

(** Why keys could not be read or a transaction checked. *)
type key_error =
  | Locked of lock  (** A transaction not yet resolved holds a key. *)
  | Retryable of string
  | Abort of string
  | Txn_not_found  (** Neither a lock nor a record of the transaction at its primary key. *)
  | Other_key_error of int  (** By its field number in [kvrpcpb.KeyError]. *)
[@@deriving sexp_of ~portable]

type pair = { key : string; value : string } [@@deriving sexp_of ~portable]

(** A response: its answer, or why it has none. *)
type 'a reply = Reply of 'a | Region_error of region_error | Key_error of key_error [@@deriving sexp_of ~portable]

type scan =
  | Pairs of pair list  (** In key order. *)
  | Locks of lock * lock list  (** Every key locked among those scanned, in key order. *)
[@@deriving sexp_of ~portable]

(** Transactions whose locks a read passes: [ignored] unseen by it, [committed] read from the lock. *)
type settled = { ignored : int64 list; committed : int64 list }

val nothing_settled : settled

(** The current time a transaction's locks are judged expired by. *)
type clock = At of int64 | Past_every_ttl

(** What [CheckTxnStatus] does when the primary key has neither a lock nor a record. *)
type missing = Rollback_if_missing | Leave_if_missing

(** [Sync_commit_forced] judges an async-commit transaction as a two-phase one. *)
type sync = As_locked | Sync_commit_forced

type check = {
  primary : string;
  version : int64;  (** The transaction's start. *)
  caller : int64;  (** The version read at: a transaction alive is pushed to commit after it. *)
  current : clock;
  missing : missing;
  sync : sync;
}

(** Whether the transaction can no longer commit at or before the caller's version. *)
type pushed = Pushed | Not_pushed [@@deriving sexp_of ~portable]

type status =
  | Committed of int64
  | Rolled_back
  | Uncommitted of { ttl : int64; pushed : pushed; min_commit_ts : int64; protocol : protocol }
      (** Its primary lock, still there. *)
  | Unsettled of int  (** Neither, by the action TiKV took. *)
[@@deriving sexp_of ~portable]

(** What the locks of the keys of an async-commit transaction show. *)
type secondaries =
  | Committed_secondary of int64  (** One key is committed, at this version. *)
  | Locks_found of lock list  (** The transaction's locks found; one missing means rolled back. *)
[@@deriving sexp_of ~portable]

val message : (Buffer.t -> unit) -> string
(** The encoded protobuf the fields written by the function make. *)

val context : Buffer.t -> region -> unit
(** A [kvrpcpb.Context]'s fields for a request to the region: its id, epoch and peer, and
    [request_source]. *)

val region_error : string -> pos:int -> len:int -> (region_error, [> `Proto of Proto.error ]) result
(** An [errorpb.Error] in [len] bytes at [pos]. *)

val scan_request :
  region -> settled -> direction:direction -> start:string -> stop:bound -> version:int64 -> limit:int -> string
(** Keys from [start] up to [stop], as of [version], at most [limit] of them, past the settled
    transactions' locks; low priority, not kept in TiKV's block cache. *)

val scanned : string -> (scan reply, [> `Proto of Proto.error ]) result

val scan :
  Http2.t ->
  region ->
  settled ->
  direction:direction ->
  start:string ->
  stop:bound ->
  version:int64 ->
  limit:int ->
  ( scan reply,
    [> `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result

val check_request : region -> check -> string
(** [CheckTxnStatus] of the transaction at its primary key, in the region holding it. *)

val checked : string -> (status reply, [> `Proto of Proto.error ]) result

val check :
  Http2.t ->
  region ->
  check ->
  ( status reply,
    [> `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result

val secondaries_request : region -> version:int64 -> string list -> string
(** [CheckSecondaryLocks] of the transaction started at [version], at these keys of the region. *)

val secondaries_checked : string -> (secondaries reply, [> `Proto of Proto.error ]) result

val check_secondaries :
  Http2.t ->
  region ->
  version:int64 ->
  string list ->
  ( secondaries reply,
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
