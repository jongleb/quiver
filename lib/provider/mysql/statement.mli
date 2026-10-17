(** A command without a result set. *)
type t =
  | Configure_session of Server.t
      (** UTC time zone, empty [sql_mode], long network timeouts, [REPEATABLE
          READ] and, on MySQL, fresh [information_schema] stats (§5.4). *)
  | Flush_tables_with_read_lock
  | Start_consistent_snapshot
  | Unlock_tables
  | Backup_stage_start  (** MariaDB only. *)
  | Backup_stage_block_commit  (** MariaDB only: blocks commits, not reads. *)
  | Backup_stage_end  (** MariaDB only. *)
  | Use of Quiver_core.Ident.t
      (** Default database, so [SHOW CREATE VIEW] leaves names unqualified. *)
  | Prepare_load
      (** A loading session: no foreign key or unique checks, UTC time zone, the
          [sql_mode] dump files are written for. *)
  | Skip_binlog
      (** The session's writes stay out of the binlog, so replicas miss them. *)
  | Create_progress_schema
  | Create_progress_table
      (** [quiver_load.progress]: data files loaded, per dump and target. *)
  | Record_progress of { dump : string; target : string; file : string }
      (** In the transaction that loads [file], so both commit or neither. *)
  | Forget_progress of { dump : string; target : string }
  | Begin
  | Commit
  | Rollback
  | Load_local of {
      table : Quiver_core.Ident.t;
      dialect : Quiver_core.Output_format.dialect;
      columns : (Quiver_core.Ident.t * conversion) list;
    }
      (** [LOAD DATA LOCAL INFILE] of a CSV data file into [table] of the
          current database. *)

(** How a CSV field becomes a column's value. *)
and conversion =
  | Text_value
  | Hex_value  (** Bytes and geometry, written as hex. *)
  | Number_value  (** [BIT], written as a decimal number. *)
[@@deriving sexp_of ~portable]

val sql : t -> string @@ portable
