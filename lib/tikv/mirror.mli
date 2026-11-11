@@ portable

(** A table's keys copied into another table, of the same TiDB cluster or another: rows and
    indexes read from TiKV at one version and imported under the other table's ids
    ({!Renumber}). *)

type sizes = {
  scan : int;  (** Keys a read asks TiKV for at a time. *)
  job : int;  (** Bytes of keys and values gathered before they are imported. *)
  batch : int;  (** Bytes of keys and values in one message of an import. *)
}

(** What the stores do while keys are imported into them. *)
type stores =
  | Imported  (** In TiKV's import mode: compaction never stalls writes, no region is split by size. *)
  | Untouched  (** As they are, for a cluster serving other work meanwhile. *)

(** What PD does with the regions of the copy while keys are imported into them. *)
type regions =
  | Held  (** Nothing: no leader or replica moved, no merge (the label [schedule=deny]). *)
  | Scheduled  (** As with any other region. *)

(** Where a copy into another cluster puts its regions. *)
type placement =
  | Kept_placement  (** Under the source's placement policies, which the target must have. *)
  | Default_placement  (** Without them, as the target places any table. *)
[@@deriving sexp_of ~portable]

type settings = {
  timeout : Core.Time_ns.Span.t;  (** Of connecting to PD and the stores, and of their silence. *)
  sizes : sizes;
  workers : int;  (** Threads copying a table at once, each with connections of its own. *)
  stores : stores;
  regions : regions;
  placement : placement;
}

type reached = {
  link : Quiver_mysql.Link.t;  (** Its TiDB. *)
  security : Http2.security;  (** Of its PD and stores. *)
}
(** A cluster as this side reaches it. *)

type copied = { keys : int; bytes : int; checksum : int64 } [@@deriving sexp_of ~portable]
(** What was copied: keys, their bytes with the values', and the CRC64 XOR of them as TiKV's
    checksum of a table sums them. *)

type error =
  | Mismatch of { copied : copied; target : copied }  (** TiDB's checksum of the target differs. *)
  | Allocator of string  (** An id allocator of a kind this side does not know. *)
  | Collations_differ of { source : string; target : string }
      (** The clusters' [new_collation_enabled]: an index's keys would be encoded otherwise. *)
  | Definition of Quiver_core.Schema.Name.t  (** Its [SHOW CREATE TABLE] does not start with its name. *)
  | Undescribed of Quiver_core.Schema.Name.t  (** No description of the table under TiKV's meta keys. *)
  | Unknown_policies of string * string list  (** Placement policies of the source the target lacks, by name in any case. *)
[@@deriving sexp_of ~portable]

(** Keys of the source from [start] up to [stop]. *)
type range = { start : string; stop : string }

val ranges : source:int64 -> Region.t list -> range list
(** The source table's keys split at the bounds of its regions. *)

val points : first:string -> last:string -> parts:int -> string list
(** Up to [parts - 1] keys after [first] and up to [last], evenly apart in the eight bytes after
    their common prefix: a row's handle, when they are keys of rows. *)

val divide : range -> string list -> range list
(** The range cut at these keys, in order, all inside it. *)

val cuts : Region.t list -> string list -> (Region.t * string list) list
(** Each region with the keys strictly inside it, the keys sorted: where it is to be split. *)

val table :
  from:reached ->
  into:reached ->
  plan:Renumber.t ->
  version:int64 ->
  commit_ts:int64 ->
  settings ->
  ( copied,
    [> `Read of Read.error
    | `Renumber of Renumber.error
    | `Pd of Pd.error
    | `Ingest of Ingest.error
    | `Region of Region.error
    | `Link of Quiver_mysql.Link.error
    | `Wire of Quiver_mysql.Wire.error
    | `Transport of Quiver_mysql.Transport.error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http1 of Http1.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error ] )
  result
(** The keys of the plan's source table, or of each of its partitions, as of [version] into the
    copy's at [commit_ts], which must be later than any version the copy holds; the TiDB of each,
    [from] and [into], the same or of two clusters, names its cluster's PD, which the regions of
    their tables are learned from; each region of the source, cut at keys evenly apart, copied by
    one of the workers; the target split at the same keys and its regions scattered first. *)

val described :
  Quiver_mysql.Link.t ->
  Quiver_core.Schema.Name.t ->
  at:int64 ->
  dial:Http2.dial ->
  ( Info.t,
    [> `Mirror of error
    | `Info of Info.error
    | `Read of Read.error
    | `Pd of Pd.error
    | `Region of Region.error
    | `Link of Quiver_mysql.Link.error
    | `Wire of Quiver_mysql.Wire.error
    | `Transport of Quiver_mysql.Transport.error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http1 of Http1.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error ] )
  result
@@ nonportable
(** The table as its TiKV keeps its description at this version. *)

(** Where the copy goes. *)
type 'link cluster = Same_cluster | Other_cluster of 'link  (** Another cluster, through its TiDB. *)

type 'e side = {
  query : Quiver_mysql.Query.t -> (string option array list, 'e) result;
  execute : Quiver_mysql.Statement.t -> (unit, 'e) result;
  server : Quiver_mysql.Server.t;
  described : Quiver_core.Schema.Name.t -> at:int64 -> (Info.t, 'e) result;  (** The table at this version. *)
}
(** How a copy reaches one cluster's TiDB. *)

type 'e session = {
  source_side : 'e side;
  target_side : 'e side;
  cluster : unit cluster;
  placement : placement;  (** Of a copy into another cluster. *)
  name : unit -> string;  (** A table name no table has, for the copy while it is made. *)
  keys : Renumber.t -> version:int64 -> commit_ts:int64 -> (copied, 'e) result;
      (** The plan's keys as of [version] into the copy at [commit_ts]. *)
  kept : version:int64 -> (unit -> (copied, 'e) result) -> (copied, 'e) result;
      (** The work, which reads the source at [version], with GC of the source held from passing it. *)
}
(** How a copy reaches TiDB and TiKV. *)

val through :
  ([> `Mirror of error | `Placement of Placement.error | `Region of Region.error | `Renumber of Renumber.error ] as 'e) session ->
  source:Quiver_core.Schema.Name.t ->
  target:Quiver_core.Schema.Name.t ->
  (copied, 'e) result
(** What {!copy} does, through the session. *)

val copy :
  reached ->
  into:reached cluster ->
  source:Quiver_core.Schema.Name.t ->
  target:Quiver_core.Schema.Name.t ->
  settings ->
  ( copied,
    [> `Mirror of error
    | `Placement of Placement.error
    | `Info of Info.error
    | `Renumber of Renumber.error
    | `Read of Read.error
    | `Pd of Pd.error
    | `Ingest of Ingest.error
    | `Region of Region.error
    | `Link of Quiver_mysql.Link.error
    | `Wire of Quiver_mysql.Wire.error
    | `Transport of Quiver_mysql.Transport.error
    | `Proto of Proto.error
    | `Grpc of Grpc.error
    | `Http1 of Http1.error
    | `Http2 of Http2.error
    | `Hpack of Hpack.error ] )
  result
@@ nonportable
(** [source] as of now into a new table [target], in its cluster or another: made under a name of
    its own ([LIKE] it, or by its definition in the other cluster, where the two clusters must agree
    on collations), its keys copied by {!table}, with the source's GC held at their version by
    {!Pd.held}, at a later version of the target's cluster under the
    copy's ids, as both tables are described at those versions, checked against TiDB's checksum,
    its id allocators moved past the source's, renamed; dropped on any failure. *)
