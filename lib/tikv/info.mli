(** A table as TiDB describes it in its schema (its [TableInfo] JSON, as TiKV keeps it under the
    meta keys), in what the table's keys and values depend on. *)

type error = Malformed of string  (** Not a description as TiDB writes one. *) [@@deriving sexp_of ~portable]

(** What a table's rows are keyed by. *)
type handle =
  | Integer_key  (** Its integer primary key. *)
  | Common_key  (** Its clustered primary key of other columns. *)
  | Row_number  (** The hidden [_tidb_rowid]. *)
[@@deriving sexp_of ~portable]

(** Where a column or an index is in TiDB's online schema change. *)
type state = Public | Changing [@@deriving sexp_of ~portable]

(** What TiDB reads for a column a row was written without. *)
type missing =
  | Unset  (** NULL or the zero value, as in a table made with the column. *)
  | Origin of string
      (** The default the column had when it was added, which a table made with it lacks: as TiDB
          keeps it, text, or the bytes of a [BIT]. *)
[@@deriving sexp_of ~portable]

(** Whether an integer column is signed. *)
type sign = Signed | Unsigned [@@deriving sexp_of ~portable]

(** Which zone a [TIMESTAMP] column's default is written in. *)
type clock =
  | Utc
  | Server  (** A column of TiDB before 3.0: the zone of the server that added it. *)
[@@deriving sexp_of ~portable]

(** What a column holds, as far as writing its default in a row goes. *)
type kind =
  | Integer of sign
  | Year
  | Float
  | Double
  | Decimal of { precision : int; scale : int }
  | Char  (** Text of a fixed length, whose trailing spaces TiDB drops. *)
  | Bytes  (** Text or bytes kept as written: [VARCHAR], [TEXT], [BINARY], [BLOB]. *)
  | Date
  | Datetime of { fsp : int }
  | Timestamp of { clock : clock; fsp : int }
  | Time of { fsp : int }
  | Enum of string list
  | Set of string list
  | Bit
  | Json
  | Unknown  (** A type or character set whose values this does not write. *)
[@@deriving sexp_of ~portable]

type column = { name : string; id : int; state : state; missing : missing; kind : kind } [@@deriving sexp_of ~portable]
(** By its name in lower case; a virtual column, never stored, is [Unset]. *)

(** Which keys an index of a partitioned table is under. *)
type scope =
  | Local  (** Each partition's, under its id. *)
  | Global  (** One over every partition, under the table's id. *)
[@@deriving sexp_of ~portable]

type index = { name : string; id : int64; state : state; scope : scope } [@@deriving sexp_of ~portable]

type partition = { name : string; id : int64 } [@@deriving sexp_of ~portable]
(** By its name in lower case. *)

(** Which ids the table's keys are under. *)
type layout =
  | Whole  (** The table's own. *)
  | Partitioned of partition * partition list  (** Each partition's, in TiDB's order. *)
  | Rearranging  (** Partitions being added, dropped, truncated, reorganized or exchanged. *)
[@@deriving sexp_of ~portable]

type t = { id : int64; columns : column list; indexes : index list; handle : handle; layout : layout }
[@@deriving sexp_of ~portable]
(** The table's id, its columns and indexes in TiDB's order. *)

val of_json : string -> (t, [> `Info of error ]) result

val database : string -> (string * int64, [> `Info of error ]) result
(** The name in lower case and the id of a database, from its description. *)
