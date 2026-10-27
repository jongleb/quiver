(** MySQL and MariaDB. *)

(** What a zero or calendar-invalid date ([0000-00-00], [2020-02-30]) does:
    [Reject] fails the dump; [Keep] reads every [DATE], [DATETIME] and
    [TIMESTAMP] as [Unchecked_date]/[Unchecked_datetime] and writes it as
    stored. *)
type zero_dates = Reject | Keep
[@@deriving to_string ~portable, enumerate ~portable]

(** Whether the source's [STORED] generated columns are read: [Left_out] leaves
    them for the target to compute; [Read] reads their stored values into
    {!Quiver_core.Schema.Table.generated}, for a writer that stores whole records. *)
type generated = Left_out | Read

(** How a table without a primary key or a usable unique one is read: [Scan] whole, in one
    pass over its clustered index; [Ranges] in ranges of another index's leading column,
    through that index, looking each row up in the clustered index. *)
type keyless_read = Scan | Ranges
[@@deriving to_string ~portable, enumerate ~portable]

type conn_config = {
  route : Link.route;
  user : string;
  password : string option;
  timeouts : Link.timeouts;
  zero_dates : zero_dates;
  generated : generated;
  keyless_read : keyless_read;
}
(** Where and as whom to connect. *)

(** How the group of sessions is opened: [Ftwrl] and [Backup_stage] (MariaDB
    only, blocks commits but not writes) give one point in time; [Trx_only]
    takes no lock and gives each session its own snapshot. *)
type snapshot_mode = Ftwrl | Backup_stage | Trx_only
[@@deriving sexp_of ~portable, to_string ~portable, enumerate ~portable]

type error =
  | Client of Client_error.t
  | Unknown_type of {
      table : Quiver_core.Schema.Name.t;
      column : Quiver_core.Ident.t;
      column_type : string;
          (** [COLUMN_TYPE] exactly as the server reports it. *)
    }
  | Schema_changed of Quiver_core.Schema.Name.t
      (** A result no longer matches what introspection saw. *)
  | Invalid_date of {
      table : Quiver_core.Schema.Name.t;
      column : Quiver_core.Ident.t;
    }
      (** A zero or calendar-invalid date, which has no exact value;
          [zero_dates = Keep] dumps it as stored. *)
  | Non_positive_sessions of int
  | Gtid of Gtid.error
  | Malformed_binlog_file of string
  | Unmatched_filter of Quiver_core.Filter.rule
      (** A requested database or table does not exist. *)
  | Unsupported_snapshot_mode of { mode : snapshot_mode; server : Server.t }
  | Unparsable_key of {
      table : Quiver_core.Schema.Name.t;
      column : Quiver_core.Ident.t;
      text : string;  (** As the server sent it. *)
    }  (** A key value that cannot bound a chunk, such as a zero date. *)
[@@deriving sexp_of ~portable]

type binlog_file = { base : string; index : int } [@@deriving sexp_of ~portable]
(** A binlog file name, [base.index]. *)

type binlog = { file : binlog_file; offset : int64; gtid : Gtid.t }
[@@deriving sexp_of ~portable]

(** What held the server still while the sessions opened their snapshots. *)
type lock = Global_read_lock | Backup_stage_block_commit
[@@deriving sexp_of ~portable]

(** Where the sessions' common snapshot sits. *)
type position =
  | Locked of {
      lock : lock;
      binlog : binlog option;
      lock_held : Core.Time_ns.Span.t;
    }  (** [binlog] is [None] when the binary log is disabled. *)
  | Unlocked  (** [Trx_only]: there is no common point. *)

include
  Quiver_core.Provider_intf.S
    with type conn_config := conn_config
     and type error := error
     and type position := position
     and type snapshot_mode := snapshot_mode

(** One column of an index, as [information_schema.STATISTICS] lists it. *)
type index_column = {
  index : Quiver_core.Ident.t;
  column : Quiver_core.Ident.t option;  (** [None] for an expression. *)
  uniqueness : [ `Unique | `Repeating ];
  part : [ `Whole | `Prefix ];
  cardinality : int;
  reach : [ `Forceable | `Unforceable ];  (** A visible BTREE index that [FORCE INDEX] can name. *)
}

val key :
  keyless_read ->
  fields:Quiver_core.Schema.Field.t list ->
  keyable:Quiver_core.Ident.t list ->
  index_column list ->
  Quiver_core.Ident.t Quiver_core.Schema.Table.key
(** The key to chunk by; a table with a primary key is read in its order, by it or whole. *)
