@@ portable

type error =
  | Out_of_range of { ltype : Ltype.t; value : int }
  | Inexact_float32 of float
      (** The value has no exact 32-bit representation. *)
  | Value_too_large of int
      (** Variable-width data would exceed 2^31 - 1 bytes. *)
[@@deriving sexp_of, equal]

(** The OCaml value a column holds, by logical type. *)
type _ repr =
  | Bool : bool repr
  | Int : int repr
      (** [Int8]..[Int32], [Uint8]..[Uint32]; [Date] as days since 1970-01-01.
      *)
  | Int64 : int64 repr
      (** [Int64]; [Uint64] as a bit pattern; [Time] in µs; [Datetime] and
          [Timestamp] in µs since 1970-01-01. *)
  | Float : float repr  (** [Float32], [Float64]. *)
  | Text : string repr
      (** [Decimal] in canonical text, [String], [Bytes], [Json], [Enum] member,
          [Set] members joined by commas. *)

(** Whether a slot holds a value. A [Null] slot reads as zero or [""]. *)
type presence = Null | Present [@@deriving sexp_of, equal]

type 'a t : value mod many portable
(** An Arrow-layout column buffer: validity bitmap plus fixed-width data, or
    int32 offsets plus bytes. *)

(** A column whose value type is known only after matching on {!repr}. *)
type any : value mod many portable = Any : 'a t -> any

val create : Ltype.t -> any
val recycle : any -> Ltype.t -> any
val[@zero_alloc] repr : 'a t @ local -> 'a repr @@ portable
val ltype : _ t @ local -> Ltype.t @@ portable
val[@zero_alloc] length : _ t @ local -> int @@ portable
val byte_size : _ t @ local -> int
val[@zero_alloc] add : 'a t @ local -> 'a -> (unit, [> `Column of error ]) result
val[@zero_alloc] add_int64 :
  int64 t @ local -> int64# -> (unit, [> `Column of error ]) result

val[@zero_alloc] add_float :
  float t @ local -> float# -> (unit, [> `Column of error ]) result

val[@zero_alloc] add_bigstring :
  string t @ local ->
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  off:int ->
  len:int ->
  (unit, [> `Column of error ]) result

val[@zero_alloc] add_null : _ t @ local -> unit
val[@zero_alloc] presence : _ t @ local -> int -> presence @@ portable
val get : 'a t @ local -> int -> 'a @@ portable
val[@zero_alloc] get_int64 : int64 t @ local -> int -> int64# @@ portable
val[@zero_alloc] get_float : float t @ local -> int -> float# @@ portable
val[@zero_alloc] text_length : string t @ local -> int -> int @@ portable
val[@zero_alloc] text_bytes : string t @ local -> Bytes.t @@ portable
val[@zero_alloc] text_offset : string t @ local -> int -> int @@ portable
val clear : _ t @ local -> unit

val encode : _ t @ local -> Byte_buffer.t @ local -> unit @@ portable
(** Appends the column's buffers as they lie in memory, in native byte order:
    validity, data or offsets, then the bytes the offsets point into. *)

val decode : _ t @ local -> Bytes.t -> off:int -> stop:int -> rows:int -> int option
(** Replaces the column's values with [rows] encoded by {!encode} for the same
    logical type between [off] and [stop]; the offset after them, or [None] when
    they are cut short or the offsets are not ordered within them. *)
