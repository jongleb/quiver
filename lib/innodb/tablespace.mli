(** A table's clustered index built bottom-up into a copy of the empty
    tablespace the server created for it, for [ALTER TABLE … IMPORT
    TABLESPACE]. Batches become record images on any thread; a run takes the
    images of one key range in order and fills leaf pages in extents of its
    own, so runs go on at once; [finish] links the runs in key order, builds
    the levels above, moves the top into the server's root page and writes the
    space's bookkeeping. *)

type error =
  | Template of string  (** The empty tablespace is not what this writer expects. *)
  | Shape of int  (** The batch's column has another type than the layout's field. *)
  | Row_too_long of int
      (** A record of this many bytes needs off-page columns, not written yet. *)
  | Value_too_long of int  (** A value longer than its column holds. *)
  | Unstorable of int
      (** The batch column holds a value its field cannot: no such [ENUM] or
          [SET] member, a decimal past the column's digits. *)
  | Out_of_order  (** A run's keys do not ascend. *)
  | Overlapping_runs  (** Two runs share a key range. *)
  | Too_many_extents  (** Past the 4 GiB a 32-bit page number reaches. *)
  | Too_many_rows  (** More rows than six-byte row ids number, for a table without a primary key. *)
[@@deriving sexp_of ~portable]

type t : value mod contended portable

val create :
  template:string -> Layout.t -> File.t -> (t, [> `Tablespace of error ]) result @@ portable
(** [template] is the empty table's [.ibd]; the pages go to the file. *)

type records : immutable_data
(** A batch's rows as record images. *)

val encode :
  t ->
  Quiver_core.Column.any Iarray.t @ local ->
  rows:int ->
  (records, [> `Tablespace of error ]) result
  @@ portable
(** The batch's first [rows] rows. *)

val image : records -> string @@ portable
(** The images' bytes, for a checksum. *)

type run
(** Rows of one key range, in key order, on one thread at a time. *)

val run : t -> rank:int -> run @@ portable
(** [rank] orders the table's runs when their keys cannot be compared by bytes
    ([Layout.Source_order]): the runs of chunks in key order, by chunk. *)

val add : run -> records -> (unit, [> `Tablespace of error | `File of File.error ]) result @@ portable
(** After the run's earlier rows. *)

type finished : immutable_data
(** A run's pages, waiting for its neighbours. *)

val close : run -> (finished, [> `File of File.error ]) result @@ portable

val finish :
  t -> finished list -> (int, [> `Tablespace of error | `File of File.error ]) result @@ portable
(** The runs in any order: the file's length in pages. *)
