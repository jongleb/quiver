@@ portable

type error =
  | Ragged of { column : int; length : int; rows : int }
      (** A column does not hold one value per committed row. *)
  | Malformed  (** Bytes no batch of these columns encodes to. *)
[@@deriving sexp_of, equal]

type t : value mod many portable
(** Columnar rows sharing one set of column buffers. Its columns are only lent
    out, so whoever owns the batch owns every buffer in it. *)

val create : Ltype.t list -> t @ unique
val columns : t @ local -> Column.any Iarray.t @ local
val rows : t @ local -> int
val byte_size : t @ local -> int
val commit_row : t @ local -> unit

type sealed : value mod many portable
(** A batch whose columns all hold one value per row. *)

val seal : t @ unique -> (sealed, [> `Batch of error ]) result @ unique

val unseal : sealed @ once unique -> t @ unique
(** The batch again, to be refilled. *)

val sealed_columns : sealed @ local -> Column.any Iarray.t @ local @@ portable
val sealed_rows : sealed @ local -> int @@ portable
val sealed_byte_size : sealed @ local -> int @@ portable
val reshape : t @ local -> Ltype.t list -> unit

val encode : sealed @ local -> Byte_buffer.t @ local -> unit @@ portable
(** Appends the row count and every column's buffers, in native byte order. *)

val decode :
  t @ local -> Bytes.t -> stop:int -> (sealed, [> `Batch of error ]) result @ local
(** Replaces the batch's rows with those {!encode} wrote into the bytes before
    [stop], for a batch of the same logical types, and lends it out sealed; the
    batch is left empty when they are malformed. *)
