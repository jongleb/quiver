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
          [sql_mode] dump files are written for, warnings in English and kept. *)
  | Check_unique
      (** Unique checks back on: MariaDB 10.6+ locks an empty table until commit
          for the first transaction inserting into it without them. *)
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
  | Export of Quiver_core.Schema.Name.t
      (** [FLUSH TABLES … FOR EXPORT]: the table's files are consistent and its
          [.cfg] written until [UNLOCK TABLES]. *)
  | Discard_tablespace of Quiver_core.Schema.Name.t
  | Import_tablespace of Quiver_core.Schema.Name.t
  | Rename_table of { from : Quiver_core.Schema.Name.t; into : Quiver_core.Schema.Name.t }
  | Drop_table of Quiver_core.Schema.Name.t  (** If it exists. *)
  | Drop_indexes of { table : Quiver_core.Schema.Name.t; indexes : Quiver_core.Ident.t list }
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
  | Enum_value
      (** [ENUM]: its error value [''] is stored only with a warning. *)
[@@deriving sexp_of ~portable]

val sql : t -> string @@ portable
