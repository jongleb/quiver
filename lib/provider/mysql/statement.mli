(** A command without a result set. *)
(** A TiDB id allocator: [AUTO_INCREMENT] also gives [_tidb_rowid] unless the table caches ids
    apart. *)
type allocator = Auto_increment | Auto_random [@@deriving sexp_of ~portable]

type renaming = { from : Quiver_core.Schema.Name.t; into : Quiver_core.Schema.Name.t } [@@deriving sexp_of ~portable]
(** A table and its new name. *)

type t =
  | Configure_session of Server.t
      (** UTC time zone, empty [sql_mode], long network timeouts, [REPEATABLE
          READ] and, on MySQL, fresh [information_schema] stats (§5.4); on TiDB
          reads from TiKV only and no time limit. *)
  | Flush_tables_with_read_lock
  | Start_consistent_snapshot
  | Pin_snapshot of int64
      (** TiDB: every later statement of the session reads at this TSO, the schema too. *)
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
  | Rename_tables of renaming * renaming list  (** All renamed, or none. *)
  | Drop_table of Quiver_core.Schema.Name.t  (** If it exists. *)
  | Create_database of Quiver_core.Ident.t  (** If it does not exist, in the server's defaults. *)
  | Create_like of { table : Quiver_core.Schema.Name.t; like : Quiver_core.Schema.Name.t }
  | Create_table of { table : Quiver_core.Schema.Name.t; body : string }
      (** The table under this name, the rest of its definition as [SHOW CREATE TABLE] gives it after the name. *)
  | Rebase of { table : Quiver_core.Schema.Name.t; allocator : allocator; next : int64 }
      (** TiDB: the allocator hands out ids from [next] on. *)
  | Analyze_table of Quiver_core.Schema.Name.t  (** Its statistics gathered anew. *)
  | Set_tiflash_replica of { table : Quiver_core.Schema.Name.t; count : int; labels : string list }
      (** TiDB: the table's TiFlash replicas, none at 0, placed apart by these location labels. *)
  | Drop_indexes of { table : Quiver_core.Schema.Name.t; indexes : Quiver_core.Ident.t list }
  | Scatter_regions
      (** TiDB: regions the session splits are spread over the stores before the split returns. *)
  | Split_table of { table : Quiver_core.Ident.t; at : Quiver_core.Key.t * Quiver_core.Key.t list }
      (** TiDB: [table] of the current database cut into regions at each key, of its clustered
          primary key. *)
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
