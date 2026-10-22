(** Loads a quiver dump into MySQL or MariaDB: databases, then all tables, then
    all data files in parallel sessions, each file
    streamed statement by statement in one transaction and checked against the
    manifest's checksum; views, triggers and routines last. *)

type cause : value mod contended portable =
  [ `Link of Quiver_mysql.Link.error
  | `Wire of Quiver_mysql.Wire.error
  | `Transport of Quiver_mysql.Transport.error ]
[@@deriving sexp_of ~portable]
(** A connection's failure, as the protocol client reports it. *)

type error =
  | Unreadable of { file : File_path.Part.t; message : string }
      (** The file could not be read or decompressed. *)
  | Corrupt of File_path.Part.t
      (** Its bytes differ from the manifest's checksum. *)
  | Missing of File_path.Part.t  (** A file the dump needs is not there. *)
  | Rejected of { file : File_path.Part.t; statement : string; cause : cause }
      (** The server failed a statement; [statement] is cut short. *)
  | Several_databases of Quiver_core.Ident.t list
      (** [Into] asks for one target, the dump holds these. *)
  | Non_positive_threads of int
  | Unstarted of Quiver_core.Concurrent.error  (** No loading thread could start. *)
  | Unreadable_columns of Quiver_core.Schema.Name.t
      (** The server's description of a loaded table's columns. *)
[@@deriving sexp_of ~portable]

(** Where the dumped databases go. *)
type target =
  | Same_names
  | Into of Quiver_core.Ident.t  (** The dump's only database, renamed. *)

(** When secondary and foreign keys are built: [After_data] adds a table's back
    on the session that commits its last data file, while the others keep
    loading, so InnoDB builds each index by sorting; [With_table] keeps the
    table definition whole. *)
type keys = After_data | With_table
[@@deriving to_string ~portable, enumerate ~portable]

(** Whether the load is written to the server's binlog; [Skip_binlog] needs the
    right to set [sql_log_bin], and replicas then miss the data. *)
type binlog = Write_binlog [@rename "write"] | Skip_binlog [@rename "skip"]
[@@deriving to_string ~portable, enumerate ~portable]

(** Whether loading sessions check unique keys. *)
type uniqueness =
  | Unchecked
  | Checked_on_mariadb
      (** On MariaDB, where without them the first transaction inserting into
          an empty table locks it until commit: parts of one table loading at
          once would wait for each other. *)

val session :
  Quiver_mysql.Provider.conn_config ->
  binlog ->
  uniqueness ->
  (Quiver_mysql.Link.t, [> cause ]) result @@ portable
(** A connection set up for loading: no foreign key checks, unique checks as
    asked, UTC, the [sql_mode] dumps are written for, local data allowed. *)

val sessions :
  Quiver_mysql.Provider.conn_config ->
  binlog ->
  uniqueness ->
  count:int ->
  control:(unit -> ('c, cause) result) @ once portable ->
  close:('c -> unit) ->
  ('c * Quiver_mysql.Link.t Await.Capsule.Await.With_mutex.t list, [> cause ]) result
  @@ portable
(** [control] and [count] loading sessions, all opened at once; whatever
    opened is closed again if any fails. *)

val advice : Quiver_mysql.Link.t -> binlog -> Quiver_core.Advice.t list @@ portable
(** What the target's durability settings cost a load; none when it does not
    tell. *)

val retried :
  cause:('e -> cause option) -> (unit -> ('a, 'e) result) -> ('a, 'e) result @@ portable
(** Runs again, up to 5 times in all, while the server reports a deadlock or a
    lock wait timeout. *)

val renamed : target -> Quiver_core.Ident.t -> Quiver_core.Ident.t @@ portable

val create_database : target -> Quiver_core.Ident.t -> string -> string @@ portable
(** A [CREATE DATABASE] statement for the database's new name. *)

type summary = {
  files : int;
  rows : int;
  bytes : int;
  skipped : int;  (** Files an interrupted earlier load had committed. *)
  advice : Quiver_core.Advice.t list;  (** What the target's settings cost. *)
}
(** Data files loaded now, their rows and uncompressed bytes. *)

val run :
  Quiver_mysql.Provider.conn_config ->
  dir:File_path.t ->
  target ->
  keys:keys ->
  binlog:binlog ->
  threads:int ->
  ( summary,
    [> `Load of error
    | `Manifest of Quiver_core.Manifest.error
    | `Statements of Statements.error
    | `Ident of Quiver_core.Ident.error
    | cause ] )
  result
