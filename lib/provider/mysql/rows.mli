(** Bytes per value of a binary-protocol integer. *)
type width = W1 | W2 | W4 | W8

type sign = Signed | Unsigned

(** How a result column arrives in a binary row. *)
type kind =
  | Integer of { width : width; sign : sign }
  | Float32
  | Float64
  | Date
  | Datetime  (** [DATETIME] and [TIMESTAMP]. *)
  | Time
  | Bits  (** Big-endian bytes of a [BIT] value. *)
  | Text  (** Length-encoded bytes: strings, [DECIMAL], [JSON], blobs. *)
[@@deriving sexp_of ~portable]

type t
(** A prepared statement streaming binary rows straight from the socket. *)

val open_ :
  Link.t ->
  string ->
  ( t,
    [> `Link of Link.error
    | `Wire of Wire.error
    | `Transport of Transport.error ] )
  result @@ portable

val width : t -> int @@ portable

val warnings : t -> int @@ portable
(** The warnings the server counted for the result, once its last row is read. *)
val kind : t -> int -> kind @@ portable

val next :
  t ->
  row:
    (Wire.Cursor.reader ->
    bitmap:int ->
    ( unit,
      ([> `Link of Link.error
       | `Wire of Wire.error
       | `Transport of Transport.error ]
       as
       'e) )
    result)
  @ local ->
  (Quiver_core.Batch_writer.next, 'e) result
(** Reads the next row of the result into [row], or learns it has ended. *) @@ portable

val close :
  t ->
  ( unit,
    [> `Link of Link.error
    | `Wire of Wire.error
    | `Transport of Transport.error ] )
  result

(** {2 Cells of the current row, read in column order} *) @@ portable

val[@zero_alloc] presence :
  Wire.Cursor.reader -> bitmap:int -> int -> Quiver_core.Column.presence @@ portable

val[@zero_alloc] add_integer :
  Wire.Cursor.reader ->
  width ->
  sign ->
  int Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error ]) result @@ portable

val[@zero_alloc] add_int64 :
  Wire.Cursor.reader ->
  int64 Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error ]) result @@ portable

val[@zero_alloc] add_bits :
  Wire.Cursor.reader ->
  int64 Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error ]) result @@ portable

val[@zero_alloc] add_float32 :
  Wire.Cursor.reader ->
  float Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error ]) result @@ portable

val[@zero_alloc] add_float64 :
  Wire.Cursor.reader ->
  float Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error ]) result @@ portable

val[@zero_alloc] add_date :
  Wire.Cursor.reader ->
  int Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error | `Invalid_date ]) result @@ portable

val[@zero_alloc] add_datetime :
  Wire.Cursor.reader ->
  int64 Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error | `Invalid_date ]) result @@ portable

val[@zero_alloc] add_unchecked_date :
  Wire.Cursor.reader ->
  int Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error | `Invalid_date ]) result @@ portable

val[@zero_alloc] add_unchecked_datetime :
  Wire.Cursor.reader ->
  int64 Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error | `Invalid_date ]) result @@ portable

val[@zero_alloc] add_time :
  Wire.Cursor.reader ->
  int64 Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error ]) result @@ portable

val[@zero_alloc] add_text :
  Wire.Cursor.reader ->
  string Quiver_core.Column.t @ local ->
  (unit, [> `Column of Quiver_core.Column.error ]) result @@ portable
