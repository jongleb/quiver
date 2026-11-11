@@ portable

(** TiKV's import of sorted keys ([import_sstpb.ImportSST]): each peer of a region writes them into
    its own files, the leader then ingests its files through raft. *)

type mode = Normal | Import  (** [Import] tunes the whole store for bulk writes, for a while. *)

val switch_mode :
  Http2.t ->
  mode ->
  ( unit,
    [> `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result

type sst
(** A file a store wrote, as it describes it. *)

type job = {
  uuid : string;  (** Sixteen bytes naming the files on every store. *)
  region : Kv.region;  (** The region the keys go to, as last learned; its id and epoch count. *)
  commit_ts : int64;  (** The version every key gets. *)
  pairs : Pairs.span list;  (** In key order, all inside the region, none empty; a batch a message. *)
}

(** What one store said once the job's keys were all sent. *)
type 'sst written =
  | Wrote of 'sst list  (** The files it wrote, one for each column family used. *)
  | Write_failed of string  (** The store's reason, in its response or as the call's status. *)

val write_requests : job -> string list
(** The messages of a job's write: the [SSTMeta], whose range is the first and last key inclusive,
    then each batch. *)

val written : string -> (sst written, [> `Proto of Proto.error ]) result
(** A [WriteResponse]. *)

val write :
  Http2.t list ->
  job ->
  ( sst written list,
    [> `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
(** The job to every peer of its region at once, a connection to each peer's store, each batch to
    all of them before the next; what each said, in the same order. *)

(** What the leader said to an ingest. *)
type ingested = Ingested | Ingest_failed of Kv.region_error

val ingest_request : Kv.region -> sst list -> string

val ingested : string -> (ingested, [> `Proto of Proto.error ]) result
(** An [IngestResponse]. *)

val ingest :
  Http2.t ->
  Kv.region ->
  sst list ->
  ( ingested,
    [> `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error
    | `Transport of Quiver_mysql.Transport.error ] )
  result
(** The leader's files of a job, through the region's leader named in [Kv.region]. *)
