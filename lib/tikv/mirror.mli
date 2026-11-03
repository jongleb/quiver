@@ portable

(** A table's keys copied as they are into another table of the same TiDB cluster: rows and
    indexes read from TiKV at one version and imported under the other table's id. The tables
    must agree on the ids of their columns and indexes, as [CREATE TABLE … LIKE] makes them. *)

type sizes = {
  scan : int;  (** Keys a read asks TiKV for at a time. *)
  job : int;  (** Bytes of keys and values gathered before they are imported. *)
  batch : int;  (** Bytes of keys and values in one message of an import. *)
}

(** What the stores do while keys are imported into them. *)
type stores =
  | Imported  (** In TiKV's import mode: compaction never stalls writes, no region is split by size. *)
  | Untouched  (** As they are, for a cluster serving other work meanwhile. *)

type settings = {
  timeout : Core.Time_ns.Span.t;  (** Of connecting to PD and the stores, and of their silence. *)
  sizes : sizes;
  workers : int;  (** Threads copying a table at once, each with connections of its own. *)
  stores : stores;
}

type copied = { keys : int; bytes : int; checksum : int64 } [@@deriving sexp_of ~portable]
(** What was copied: keys, their bytes with the values', and the CRC64 XOR of them as TiKV's
    checksum of a table sums them. *)

type error =
  | Partitioned of Quiver_core.Schema.Name.t  (** Its partitions would get new ids in the copy. *)
  | Mismatch of { copied : copied; target : copied }  (** TiDB's checksum of the target differs. *)
  | Allocator of string  (** An id allocator of a kind this side does not know. *)
[@@deriving sexp_of ~portable]

(** Keys of the source from [start] up to [stop], and the same keys in the target. *)
type range = { start : string; stop : string; target_start : string; target_stop : string }

val ranges : source:int64 -> into:int64 -> Region.t list -> range list
(** The source table's keys split at the bounds of its regions, each part also as the target
    table's keys. *)

val points : first:string -> last:string -> parts:int -> string list
(** Up to [parts - 1] keys after [first] and up to [last], evenly apart in the eight bytes after
    their common prefix: a row's handle, when they are keys of rows. *)

val divide : into:int64 -> range -> string list -> range list
(** The range cut at these keys, in order, all inside it. *)

val cuts : Region.t list -> string list -> (Region.t * string list) list
(** Each region with the keys strictly inside it, the keys sorted: where it is to be split. *)

val table :
  Quiver_mysql.Link.t ->
  source:Quiver_core.Schema.Name.t ->
  target:Quiver_core.Schema.Name.t ->
  version:int64 ->
  commit_ts:int64 ->
  settings ->
  ( copied,
    [> `Read of Read.error
    | `Pd of Pd.error
    | `Ingest of Ingest.error
    | `Region of Region.error
    | `Link of Quiver_mysql.Link.error
    | `Wire of Quiver_mysql.Wire.error
    | `Transport of Quiver_mysql.Transport.error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error ] )
  result
(** [source]'s keys as of [version] into [target] at [commit_ts], which must be later than any
    version [target] holds; PD found through the link to TiDB, the regions of both learned from it,
    each region of [source], cut at keys evenly apart, copied by one of the workers; [target] split
    at the same keys and its regions scattered first. *)

type 'e session = {
  query : Quiver_mysql.Query.t -> (string option array list, 'e) result;
  execute : Quiver_mysql.Statement.t -> (unit, 'e) result;
  server : Quiver_mysql.Server.t;
  name : unit -> string;  (** A table name no table has, for the copy while it is made. *)
  keys : Quiver_core.Schema.Name.t -> version:int64 -> commit_ts:int64 -> (copied, 'e) result;
      (** The source's keys as of [version] into this table at [commit_ts]. *)
}
(** How a copy reaches TiDB and TiKV. *)

val through :
  ([> `Mirror of error | `Region of Region.error ] as 'e) session ->
  source:Quiver_core.Schema.Name.t ->
  target:Quiver_core.Schema.Name.t ->
  (copied, 'e) result
(** What {!copy} does, through the session. *)

val copy :
  Quiver_mysql.Link.t ->
  source:Quiver_core.Schema.Name.t ->
  target:Quiver_core.Schema.Name.t ->
  settings ->
  ( copied,
    [> `Mirror of error
    | `Read of Read.error
    | `Pd of Pd.error
    | `Ingest of Ingest.error
    | `Region of Region.error
    | `Link of Quiver_mysql.Link.error
    | `Wire of Quiver_mysql.Wire.error
    | `Transport of Quiver_mysql.Transport.error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error ] )
  result
(** [source] as of now into a new table [target]: made [LIKE] it under a name of its own, its keys
    copied by {!table} at a later version, checked against TiDB's checksum, its id allocators moved
    past the source's, renamed; dropped on any failure. *)
