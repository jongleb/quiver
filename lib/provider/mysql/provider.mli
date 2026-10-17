(** MySQL and MariaDB. *)

(** What a zero or calendar-invalid date ([0000-00-00], [2020-02-30]) does:
    [Reject] fails the dump; [Keep] reads every [DATE], [DATETIME] and
    [TIMESTAMP] as [Unchecked_date]/[Unchecked_datetime] and writes it as
    stored. *)
type zero_dates = Reject | Keep

type conn_config = {
  tls : Link.tls;
  host : string;
  port : int;
  user : string;
  password : string option;
  timeout : Core.Time_ns.Span.t;
      (** Longest silence of a connection before the dump fails. *)
  zero_dates : zero_dates;
}
(** Where and as whom to connect. *)

(** How the group of sessions is opened: [Ftwrl] and [Backup_stage] (MariaDB
    only, blocks commits but not writes) give one point in time; [Trx_only]
    takes no lock and gives each session its own snapshot. *)
type snapshot_mode = Ftwrl | Backup_stage | Trx_only
[@@deriving sexp_of ~portable, yojson]

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

type binlog_file = { base : string; index : int } [@@deriving sexp_of ~portable, yojson]
(** A binlog file name, [base.index]. *)

type binlog = { file : binlog_file; offset : int64; gtid : Gtid.t }
[@@deriving sexp_of ~portable, yojson]

(** What held the server still while the sessions opened their snapshots. *)
type lock = Global_read_lock | Backup_stage_block_commit
[@@deriving sexp_of ~portable, yojson]

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
