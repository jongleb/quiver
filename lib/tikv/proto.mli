@@ portable

(** The protocol buffers wire format, as far as TiKV's messages need it. *)

type error =
  | Truncated  (** The message ends inside a field. *)
  | Overlong_varint  (** A varint of more than 64 bits. *)
  | Unknown_wire of int  (** A wire type that is not 0, 1, 2 or 5. *)
  | Field_zero  (** Field numbers start at 1. *)
  | Unexpected of { field : int; wire : int }
      (** A known field encoded otherwise than its message declares. *)
[@@deriving sexp_of ~portable]

val uint : Buffer.t -> int -> int64 -> unit
(** A varint field (an unsigned 64-bit pattern, an enum, a bool as 0 or 1). *)

val delimited : Buffer.t -> int -> int -> unit
(** The start of a length-delimited field of this many bytes, which the caller writes next. *)

val put_delimited : Bytes.t -> int -> int -> int -> int
(** {!delimited} written at this position of the bytes, a field then a length not negative; the
    position after it. *)

val put_varint : Bytes.t -> int -> int -> int
(** A varint of a number not negative at this position; the position after it. *)

val uint_size : int -> int64 -> int
(** The bytes {!uint} writes. *)

val delimited_size : int -> int -> int
(** The bytes of a length-delimited field with this many bytes in it. *)

val bytes : Buffer.t -> int -> string -> unit
(** A length-delimited field: bytes, a string or an encoded message. *)

(** One field's value as the wire carries it. *)
type value =
  | Varint of int64
  | Fixed64 of int64
  | Fixed32 of int32
  | Delimited of { pos : int; len : int }  (** Bytes of the text being read. *)

val fold :
  string ->
  pos:int ->
  len:int ->
  init:'a ->
  f:('a -> int -> value -> ('a, ([> `Proto of error ] as 'e)) result) ->
  ('a, 'e) result
(** Every field of the message in [len] bytes at [pos], in order, with its number. *)

val number : int -> value -> (int64, [> `Proto of error ]) result
(** A varint field's value; [Unexpected] for another wire type. *)

val slice : int -> value -> (int * int, [> `Proto of error ]) result
(** Where a length-delimited field's bytes lie: position and length. *)

val contents : string -> int -> value -> (string, [> `Proto of error ]) result
(** A length-delimited field's bytes, from the text the value was read in. *)

val numbers : string -> int -> value -> (int64 list, [> `Proto of error ]) result
(** A repeated varint field's values in one value: one, or every one packed in a length-delimited
    field, as proto3 writes them. *)

(** How far {!walk} went. *)
type walked =
  | Walked  (** Every field. *)
  | Unusual  (** It stopped at something {!fold} reads otherwise or refuses, or a callback stopped it. *)

val walk :
  string ->
  pos:int ->
  len:int ->
  delimited:(int -> int -> int -> walked) @ local ->
  scalar:(int -> walked) @ local ->
  walked
(** The fields of a message as {!fold} reads them, with nothing allocated: each length-delimited
    one to [delimited] with its number, where its bytes start and their length, any other to
    [scalar] with its number; it stops at a varint longer than eight bytes, as {!fold} reads all. *)
