type collation = {
  charset : Quiver_core.Ident.t;
  collation : Quiver_core.Ident.t;
}
[@@deriving sexp_of ~portable]
(** A column's character set and collation, as the server names them. *)

(** A read the provider sends to the server. Constructors whose SQL differs
    between release lines carry the {!Server.t} they are rendered for. *)
type t =
  | Tables  (** Base tables outside the system schemas, with estimates. *)
  | Columns
  | Unique_indexes
  | Key_bounds of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Ident.t;
    }
  | Read_threads of Server.t
      (** The server's sizing hint for parallel reads: InnoDB read I/O
          threads on MySQL, the thread pool's groups on MariaDB. *)
  | Binlog_status of Server.t
  | Gtid_executed of Server.t
  | Show_create of Quiver_core.Ddl_target.t
  | View_columns of Quiver_core.Ident.t
  | Triggers of Quiver_core.Ident.t
  | Routines of Quiver_core.Ident.t
  | Events of Quiver_core.Ident.t
  | Show_definition of {
      database : Quiver_core.Ident.t;
      name : Quiver_core.Ident.t;
      kind : Quiver_core.Definition.kind;
    }
  | Chunk of Quiver_core.Chunk.t
      (** The chunk's rows, in primary key order when the table has one. *)
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
    }  (** The existing [candidates], distinct and in the server's key order. *)
[@@deriving sexp_of ~portable]

val sql : t -> string @@ portable

val bytes : string -> string @@ portable
(** A binary string literal: [X'…']. *)

val text : string -> string @@ portable

val qualified : Quiver_core.Schema.Name.t -> string @@ portable
(** [`database`.`table`]. *)
(** A UTF-8 string literal in hex, so no byte is ever escaped: [_utf8mb4 X'…']. *)
