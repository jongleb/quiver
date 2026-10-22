(** A 16 KiB page of a B-tree index in the COMPACT format, filled with records
    in key order the way InnoDB's bulk loader fills it: a directory slot every
    4 records, 1/16 of the page kept free, at least two records a page. *)

val size : int @@ portable

val null : int @@ portable
(** [FIL_NULL]: no page. *)

type t

(** Whether one more record of a size goes on the page. *)
type room = Fits | Full

(** A record's place in the tree. *)
type status = Ordinary | Node_pointer

(** [Leftmost] marks the first record of a level's leftmost non-leaf page. *)
type flag = Plain | Leftmost

val create : level:int -> index:int -> t @@ portable
val level : t -> int @@ portable
val count : t -> int @@ portable
val bytes : t -> Bytes.t @@ portable

val room : t -> int -> room @@ portable
(** For a record of this many bytes, its extra bytes and header included. *)

val append : t -> extra:int -> data:int -> status -> flag -> int @@ portable
(** Links a record after the last and returns its origin: [extra] bytes go
    just below the 5-byte header at [origin - 5], [data] bytes from [origin].
    The caller writes both. *)

val place : t -> string -> pos:int -> length:int -> origin:int -> int @@ portable
(** Copies an ordinary record's image of [length] bytes at [pos], its origin
    [origin] bytes in, after the last record; returns its origin on the page. *)

val first : t -> int @@ portable
(** The origin of the first record. *)

val close : t -> unit @@ portable
(** Writes the directory and the page header. *)

(** What a page holds, as its file header names it. *)
type kind =
  | Index
  | Lob_first  (** A column's first page off the record: its pieces' index and data. *)
  | Lob_data
  | Lob_index  (** More of the pieces' index. *)

val seal : Bytes.t -> kind:kind -> number:int -> prev:int -> next:int -> space:int -> unit @@ portable
(** Writes the file header and the checksum of a closed page. *)

val checksum : Bytes.t -> pos:int -> int @@ portable
(** InnoDB's CRC-32C checksum of the page at [pos]. *)
