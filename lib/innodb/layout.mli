(** How a dumped table's rows become records of its clustered index: the
    index's fields in record order, each tied to a column of the dump's batches
    and stored the way InnoDB stores its type. *)

type error =
  | Missing_column of string  (** The index stores a column the dump lacks. *)
  | Unsupported of { column : string; mtype : int; prtype : int }
      (** A stored type this writer does not encode yet. *)
  | Mismatch of { column : string; ltype : Quiver_core.Ltype.t }
      (** The dump's type cannot be the one stored. *)
  | Prefix_key of string  (** The key indexes a column's prefix. *)
  | Secondary_indexes  (** Only a table with its primary key alone is built. *)
  | Hidden_key  (** No primary key: InnoDB orders rows by a hidden row id. *)
  | Row_format of { table_flags : int; space_flags : int }
      (** Not [DYNAMIC] in a plain 16 KiB tablespace: compressed, encrypted, … *)
  | Key_type of string
      (** A key column whose order is not its bytes': text by a collation, floats. *)
  | Descending_key of string  (** Rows of such a key are stored backwards. *)
  | Charset of { column : string; collation : int }
      (** Text in a character set other than [utf8mb4] and [utf8mb3]. *)
[@@deriving sexp_of ~portable]

(** Whether a stored integer has a sign, kept sortable by flipping its top bit. *)
type sign : immutable_data = Signed | Unsigned [@@deriving sexp_of ~portable]

(** What the batch's value stands for, turned into the stored integer. *)
type number : immutable_data =
  | Plain  (** The value itself, [Unchecked_date]'s packed fields included. *)
  | Year  (** [YEAR]: year − 1900, the zero year 0. *)
  | Days  (** [DATE] from days since 1970: year × 512 + month × 32 + day. *)
  | Member of string list  (** [ENUM]: the member's place from 1, 0 for [''] . *)
  | Members of string list  (** [SET]: a bit per member. *)
[@@deriving sexp_of ~portable]

(** Where a [DATETIME] value comes from. *)
type clock : immutable_data =
  | Microseconds  (** Since 1970. *)
  | Fields  (** [Unchecked_datetime]'s packed fields. *)
[@@deriving sexp_of ~portable]

(** The character set of a text field; the dump reads text in [utf8mb4]. *)
type charset : immutable_data =
  | Utf8mb4  (** Stored as read. *)
  | Utf8mb3  (** Stored as read, with no character of four bytes. *)
[@@deriving sexp_of ~portable]

(** How one field is stored. *)
type storage : immutable_data =
  | Integer of { bytes : int; sign : sign; number : number }  (** Big-endian. *)
  | Float  (** Four bytes, little-endian. *)
  | Double  (** Eight bytes, little-endian. *)
  | Decimal of { precision : int; scale : int }  (** Nine digits per four bytes. *)
  | Datetime of { fsp : int; clock : clock }
  | Timestamp of int  (** Seconds since 1970 and [fsp] fractional digits. *)
  | Time of int  (** A signed duration with this many fractional digits. *)
  | Binary of int  (** [BINARY]: raw bytes padded with zeros. *)
  | Char of { least : int; big : bool; charset : charset }
      (** [CHAR]: text padded with spaces to [least] bytes. *)
  | Text of { big : bool; charset : charset }
      (** [VARCHAR] or [TEXT]; a length of 128 or more takes two bytes when [big]. *)
  | Variable of { big : bool }
      (** Raw bytes; a length of 128 or more takes two bytes when [big]. *)
  | Json  (** The text's binary form, like a [BLOB]. *)
  | Trx_id  (** Six bytes the import overwrites. *)
  | Roll_ptr  (** Seven bytes the import overwrites. *)
[@@deriving sexp_of ~portable]

(** A stored value's length. *)
type length : immutable_data =
  | Fixed of int
  | Varying of { big : bool }  (** Given before the record, in one byte or two when [big]. *)

(** Whether a field can hold NULL, and which bit of the record says so. *)
type nullability : immutable_data = Not_null | Null_bit of int [@@deriving sexp_of ~portable]

type field : immutable_data = {
  source : int;  (** The batch column, or -1 for a system field. *)
  storage : storage;
  nullability : nullability;
}
[@@deriving sexp_of ~portable]

type t : immutable_data = private {
  fields : field list;  (** In record order: the key first. *)
  key : int;  (** How many leading fields make the key. *)
  null_bytes : int;  (** Of every record, node pointers included. *)
  index : int;  (** The clustered index's id. *)
  root : int;  (** Its root page. *)
}
[@@deriving sexp_of ~portable]

val of_cfg :
  Cfg.t ->
  collations:(int * charset) list ->
  (string * Quiver_core.Ltype.t) list ->
  (t, [> `Layout of error ]) result
  @@ portable
(** For the dump's columns, by name and type, in batch order; [collations]
    are the server's collations of the character sets stored, by id. *)

val length : storage -> length @@ portable

val key_length : t -> int @@ portable
(** Bytes of a record's key, which is fixed. *)
