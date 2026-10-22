(** The [.cfg] file [FLUSH TABLES … FOR EXPORT] writes beside a tablespace
    (format 7, MySQL 8.0.29 and later): the table's columns and indexes as
    InnoDB stores them. *)

type error =
  | Truncated of int  (** The file ends before this offset's field. *)
  | Unsupported_version of int
  | Page_size of int  (** Pages are not 16 KiB. *)
  | Row_versions  (** Columns were added or dropped instantly. *)
  | No_clustered_index
[@@deriving sexp_of ~portable]

(** A column as InnoDB stores it. *)
module Column : sig
  type t : immutable_data = {
    name : string;
    mtype : int;  (** Main type: [DATA_INT], [DATA_VARCHAR], … *)
    prtype : int;  (** Precise type: the MySQL type and flags. *)
    len : int;  (** Longest value in bytes. *)
    mbminmaxlen : int;  (** Shortest and longest character, for text. *)
  }
  [@@deriving sexp_of ~portable]
end

(** An index as InnoDB stores it. *)
module Index : sig
  (** The direction a field is sorted in. *)
  type order = Ascending | Descending [@@deriving sexp_of ~portable]

  type field : immutable_data = {
    column : string;
    prefix : int;  (** Bytes of the column indexed, 0 for all of it. *)
    fixed : int;  (** Stored length when fixed, otherwise 0. *)
    order : order;
  }
  [@@deriving sexp_of ~portable]

  type t : immutable_data = {
    id : int;
    root : int;  (** Page number of the root. *)
    name : string;
    n_uniq : int;  (** Fields that identify a record. *)
    n_nullable : int;
    fields : field list;  (** In record order. *)
  }
  [@@deriving sexp_of ~portable]
end

type t : immutable_data = private {
  table : string;  (** [database/table]. *)
  autoinc : int;  (** Next [AUTO_INCREMENT] value, 0 to take the column's largest. *)
  table_flags : int;
  space_flags : int;
  columns : Column.t list;  (** User columns, then the system ones. *)
  clustered : Index.t;
  secondary : Index.t list;
}
[@@deriving sexp_of ~portable]

val parse : string -> (t, [> `Cfg of error ]) result @@ portable
