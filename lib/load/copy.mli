(** A sink that writes a dump straight into a MySQL or MariaDB server, with no
    files: databases and tables are created as the dump reaches them, each chunk
    streams through [LOAD DATA LOCAL] in one transaction on a loading session of
    its own, and views, triggers and routines follow the data. *)

type t : value mod contended portable
type part
type renderer : value mod contended portable
(** What rendering a part's batches needs. *)

type rendered : value mod portable

(** What [Tablespaces] needs of the target. *)
type need =
  | Mysql_8_4  (** The only line whose files this writer builds. *)
  | Skipped_binlog
      (** [--binlog skip]: imported rows never reach the binlog, so replicas
          would miss them without a word. *)
  | Local_datadir of string  (** The data directory, here on this machine. *)
  | Sort_directory of string  (** A directory here for sorted secondary records. *)
[@@deriving sexp_of ~portable]

(** How tables are built as tablespaces. *)
type tablespaces = {
  datadir : string option;
      (** The server's data directory as this machine sees it, the server's own path when not given. *)
  builder : Physical.builder;
  sorting : string option;  (** Where sorted secondary records wait: beside each built file when not given. *)
  threads : int option;  (** Threads merging a table's secondary indexes: half the fast cores when not given. *)
}

(** How the data reaches the target. *)
type ingest =
  | Load_data  (** [LOAD DATA LOCAL], a transaction per chunk. *)
  | Tablespaces of tablespaces
      (** Each table this writer can build is built as a tablespace beside the
          server and imported (§5.9); the others take [LOAD DATA]. *)

type error =
  | Unreachable of Load.cause  (** A loading session could not be set up. *)
  | Rejected of { statement : string; cause : Load.cause }
      (** The target failed a statement; [statement] is cut short. *)
  | Several_databases of Quiver_core.Ident.t list
      (** [Into] asks for one target, the dump reached these. *)
  | Unrepresentable_date of {
      table : Quiver_core.Schema.Name.t @@ aliased;
      column : Quiver_core.Ident.t @@ aliased;
    }  (** A date or time value outside the proleptic Gregorian years 0–9999. *)
  | Unsplittable_definition of Quiver_core.Schema.Name.t
      (** A routine, trigger or view with [";\n"] inside a string literal. *)
  | Tablespaces_need of need
  | Built of { table : Quiver_core.Schema.Name.t @@ aliased; cause : Physical.cause @@ aliased }
      (** Building, checking or importing the table's tablespace failed. *)
  | Lost_rows of { table : Quiver_core.Schema.Name.t; sent : int; built : int }
      (** The imported tablespace holds another number of rows than were read. *)
  | Mismatched_render  (** A batch rendered for the other kind of part. *)
  | After_load of After_load.error
[@@deriving sexp_of ~portable]

val create :
  Quiver_mysql.Provider.conn_config ->
  Load.target ->
  keys:Load.keys ->
  after:After_load.rule list ->
  binlog:Load.binlog ->
  ingest:ingest ->
  sessions:int ->
  (t, [> `Sink of error ]) result
(** Opens [sessions] loading sessions, each taken by one part at a time from
    [part] to [close]: as many as parts open at once. [after] runs once every table's
    rows and keys are in, before foreign keys and objects. *)

val shutdown : t -> unit
(** Ends the loading sessions, after the dump succeeded or failed. *)

type imported = {
  preparing : Core.Time_ns.Span.t;
      (** Creating its empty table and exporting the template, a free session included. *)
  queued : Core.Time_ns.Span.t;  (** From its last part to a free session. *)
  checking : Core.Time_ns.Span.t;  (** Building its upper levels and checking the whole file, here. *)
  importing : Core.Time_ns.Span.t;  (** [DISCARD], [IMPORT] and [RENAME] on the server. *)
  keys : Core.Time_ns.Span.t;  (** Adding its secondary keys. *)
}
(** Where a table built beside the server spent its time on the target. *)

(** How a table with rows reached the target under [Tablespaces]. *)
type way =
  | Imported of imported
  | Loaded of Physical.unsuitable  (** [LOAD DATA], since the writer cannot build it. *)

val ways : t -> (Quiver_core.Schema.Name.t * way) list
(** By the table's name in the dump. *)

val advice : t -> Quiver_core.Advice.t list
(** What the target's durability settings cost the copy. *)

(** How long [finish] took, part by part. *)
type finishing =
  | Unfinished
  | Finished of {
      built : Core.Time_ns.Span.t;  (** Waiting for imports and keys still running. *)
      after_load : Core.Time_ns.Span.t;  (** The statements of [after]. *)
      foreign_keys : Core.Time_ns.Span.t;
      objects : Core.Time_ns.Span.t;
    }

val finishing : t -> finishing

include
  Quiver_core.Sink_intf.S
    with type t := t
     and type part := part
     and type renderer := renderer
     and type rendered := rendered
     and type error := error
