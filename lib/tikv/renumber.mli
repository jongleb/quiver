@@ portable

(** A table's keys and values as those of its copy, whose columns, indexes and partitions may have
    other ids: the table, partition and index ids of keys, the column ids of rows and of the columns indexes keep to
    restore. Keys of an index the source does not have public are left behind: an index dropped,
    and not yet collected, or one being built. *)

type error =
  | Changing of string  (** A column or index of either table not public: its schema is being changed. *)
  | Handles_differ of { source : Info.handle; target : Info.handle }
  | Columns_differ of { source : string list; target : string list }
  | Indexes_differ of { source : string list; target : string list }
  | Partitions_differ of { source : string list; target : string list }  (** None when not partitioned. *)
  | Rearranging  (** Partitions of either table being added, dropped, truncated, reorganized or exchanged. *)
  | Global_index of string
      (** A global index this does not move: of a table without partitions, or naming the partition in its keys in one table only. *)
  | Missing of string
      (** A row written before this column was added, whose default then this does not write: the
          source reads the row with it, the copy would read it without. *)
  | Malformed of string  (** A value, in hex, not as TiDB writes one. *)
[@@deriving sexp_of ~portable]

type t : value mod contended portable

val make : source:Info.t -> target:Info.t -> (t, [> `Renumber of error ]) result
(** From the two tables at the versions the keys are read at and written at. *)

val tables : t -> (int64 * int64) list
(** The ids keys are under, each of the source with the copy's: the table's, or its partitions' and,
    when it has global indexes, its own. *)

val key : t -> string -> (string option, [> `Renumber of error ]) result
(** The key in the copy, any key of the ranges of {!tables}; [None] for a key of an index the source
    does not have public, of another table, or under a partitioned table's own id but of none of its
    global indexes. A global index's key naming its partition names the copy's. *)

val value : t -> key:string -> string -> (string, [> `Renumber of error ]) result
(** The value of this source key in the copy, the same string when no column id in it moves and
    no default is written into it; a global index's value names the copy's partition; a row of TiDB's first format stays in it, its columns in the
    copy's order. A row written before a column was added gets the default the
    column had then, which the copy, made with the column, lacks. A rewritten row has no checksum:
    TiDB writes one only when asked and reads none. *)
