(** A built tablespace read back the way [IMPORT TABLESPACE] and the server's
    later work read it. Several of the server's own checks stop mysqld instead
    of failing the import, and any access to a table whose import failed stops
    it too, so only a checked file reaches the server. *)

type error =
  | Size of int  (** The file's length in bytes is not what its header says. *)
  | Checksum of int  (** Of this page. *)
  | Number of int  (** The page's header names another page. *)
  | Not_index of int  (** A page of the tree is not an index page of this index. *)
  | Level of int  (** The page sits at another level than its parent says. *)
  | Empty of int  (** A page other than an empty table's root holds no record. *)
  | Links of int  (** Its previous or next page is not its neighbour on the level. *)
  | Records of int  (** Its record list, heap or record headers are malformed. *)
  | Directory of int  (** Its directory slots or record ownership are wrong. *)
  | Leftmost of int  (** The minimum-record flag is missing or misplaced. *)
  | Order of int  (** A key is not above the one before it. *)
  | Pointer of int  (** A node pointer disagrees with the child it names. *)
  | Lob of int  (** A column stored off the record, on this page, is malformed. *)
  | Space of string  (** Extent and segment bookkeeping disagrees with the pages used. *)
  | Duplicate of int  (** A [UNIQUE] key repeats on this page. *)
  | Entries of int  (** The index with this id holds another number of records than the table's rows. *)
[@@deriving sexp_of ~portable]

type t = private { pages : int; rows : int }
(** A file that passed: its length in pages and the rows of its clustered index. *)

val tablespace :
  Layout.t ->
  secondary:Layout.t list ->
  pages:int ->
  read:(int -> Bytes.t) @ portable ->
  (t, [> `Check of error ]) result
  @@ portable
(** The clustered index and the [secondary] ones, each with a record per row. *)
(** [read n] is page [n] of the file, zeros if never written. *)
