type collation = {
  charset : Quiver_core.Ident.t;
  collation : Quiver_core.Ident.t;
}
[@@deriving sexp_of ~portable]
(** A column's character set and collation, as the server names them. *)

(** A read the provider sends to the server. Constructors whose SQL differs
    between release lines carry the {!Server.t} they are rendered for. *)
type t =
  | Tables of Server.t
      (** Base tables outside the system schemas, with estimates and, on TiDB, whether rows are
          numbered by [_tidb_rowid]. *)
  | Columns
  | Indexes of Server.t
  | Foreign_keys  (** Columns of foreign keys outside the system schemas, with the columns they refer to. *)
  | Key_bounds of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Ident.t;
    }
  | Read_threads of [ `Innodb | `Thread_pool ]
      (** The server's sizing hint for parallel reads: InnoDB read I/O
          threads on MySQL, the thread pool's groups on MariaDB. *)
  | Binlog_status of Server.t  (** On TiDB, the TSO of a fresh transaction. *)
  | Gtid_executed of [ `Mysql | `Mariadb ]
  | Show_create of Quiver_core.Ddl_target.t
  | View_columns of Quiver_core.Ident.t
  | Triggers of Quiver_core.Ident.t
  | Routines of Quiver_core.Ident.t
  | Events of Quiver_core.Ident.t
  | Sequences of Quiver_core.Ident.t  (** The database's sequences (MariaDB, TiDB). *)
  | Show_create_sequence of Quiver_core.Schema.Name.t
  | Sequence_next of { server : Server.t; sequence : Quiver_core.Schema.Name.t }
      (** Where the sequence stands: MariaDB's first value neither handed out nor cached and
          the increment, TiDB's one more than the last value handed out or cached. *)
  | Show_definition of {
      database : Quiver_core.Ident.t;
      name : Quiver_core.Ident.t;
      kind : Quiver_core.Definition.kind;
    }
  | Chunk of { chunk : Quiver_core.Chunk.t; sources : (Quiver_core.Ident.t * string) list }
      (** The chunk's rows, in primary key order when the table has one; each field in
          [sources] is its expression, cast to the field's type. *)
  | Key_span of { chunk : Quiver_core.Chunk.t; key : Quiver_core.Ident.t }
      (** The smallest and the largest [key] of the chunk's rows. *)
  | Collation of {
      table : Quiver_core.Schema.Name.t;
      column : Quiver_core.Ident.t;
    }
  | Load_columns of Quiver_core.Schema.Name.t
      (** Name and [DATA_TYPE] of each stored column, in table order. *)
  | Durability
      (** [innodb_flush_log_at_trx_commit], [sync_binlog] and [log_bin]. *)
  | Autoinc_lock_mode  (** [innodb_autoinc_lock_mode]. *)
  | Datadir  (** [datadir]: where the server keeps its files. *)
  | Text_collations  (** Ids of the server's [utf8mb4], [utf8mb3] and [latin1] collations, with the set's name. *)
  | Auto_increment of Quiver_core.Schema.Name.t
      (** The table's [AUTO_INCREMENT] column, if it has one. *)
  | Clustering of Quiver_core.Schema.Name.t
  | Table_id of Quiver_core.Schema.Name.t  (** TiDB: the table's [TIDB_TABLE_ID]. *)
  | Table_regions of int64
      (** TiDB: id, bounds in hex of their order-keeping form, and epoch of each region holding
          some of the table's keys; a region once for each of the table's ranges it holds. *)
  | Region_peers of int64 * int64 list
      (** TiDB: each region's peers, their stores, whether each leads (1 or 0) and the store's
          address. *)
  | Pd_instances  (** TiDB: [host:port] of each PD server. *)
  | Tidb_status  (** TiDB: [host:port] of each TiDB server's HTTP status service. *)
  | New_collations  (** TiDB: whether the cluster was made with the new collation framework, as [True] or [False]. *)
  | Next_row_ids of Quiver_core.Schema.Name.t
      (** TiDB: each id allocator of the table, the next id it hands out and its kind. *)
  | Checksum of Quiver_core.Schema.Name.t
      (** TiDB: the CRC64 XOR of every key and value of the table, their count and bytes. *)
  | Partitioned of Quiver_core.Schema.Name.t  (** The number of the table's partitions. *)
  | Base_tables of Quiver_core.Ident.t  (** The database's tables, not views or sequences, by name. *)
      (** TiDB: the table's [TIDB_PK_TYPE], [CLUSTERED] when its rows are kept by its primary key. *)
  | Warnings  (** Level, code and message of the last statement's warnings. *)
  | Loaded_files of { dump : string; target : string }
      (** Data files an earlier [quiver load] of this dump committed. *)
  | Probe of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Schema.Field.t * Quiver_core.Schema.Field.t list;
      from : Quiver_core.Key.value;
      collation : collation option;
          (** Of the first field when it is text: [from] is converted to it, so
              a guess outside the column's repertoire cannot fail. *)
    }  (** The smallest key whose first field is at least [from]. *)
  | Estimate of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Ident.t;
      from : Quiver_core.Key.value option;
      below : Quiver_core.Key.value option;
    }
      (** [EXPLAIN] of the rows with [from <= key < below]: the server's
          estimate from index dives, in a column that depends on the line. *)
  | Ordered_keys of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Schema.Field.t * Quiver_core.Schema.Field.t list;
      candidates : Quiver_core.Key.t * Quiver_core.Key.t list;
    }
  | Region_starts of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Schema.Field.t * Quiver_core.Schema.Field.t list;
    }
      (** TiDB: the key of the first row of each region of the table, in key order. *)  (** The existing [candidates], distinct and in the server's key order. *)
[@@deriving sexp_of ~portable]

val sql : t -> string @@ portable

val literal : Quiver_core.Key.value -> string @@ portable
(** As a key's field compares with it, in a session at UTC. *)

val bytes : string -> string @@ portable
(** A binary string literal: [X'…']. *)

val text : string -> string @@ portable

val qualified : Quiver_core.Schema.Name.t -> string @@ portable
(** [`database`.`table`]. *)
(** A UTF-8 string literal in hex, so no byte is ever escaped: [_utf8mb4 X'…']. *)
