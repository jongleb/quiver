(** A frame of quiver's wire protocol: a 32-byte header, then the payload. The
    header holds, little-endian: magic ["QW"], version, a zero byte, kind,
    codec, two zero bytes, the chunk, the frame's number within the chunk,
    payload length on the wire, its decoded length, the low 32 bits of xxh3-64
    of the payload on the wire, and those of xxh3-64 of the 28 bytes before. *)

(** What the payload is. *)
type kind = Message  (** A JSON message. *) | Batch  (** An encoded batch. *)

(** How the payload is encoded on the wire. *)
type codec = Plain | Zstd

type error =
  | Bad_magic
  | Unsupported_version of int
  | Damaged_header  (** The header's own checksum fails. *)
  | Damaged_payload  (** The payload's checksum fails. *)
  | Unknown_kind of int
  | Unknown_codec of int
  | Too_large of int  (** A payload longer than {!max_payload}. *)
[@@deriving sexp_of ~portable]

type header = private {
  kind : kind;
  codec : codec;
  chunk : int;  (** Zero for a message. *)
  seq : int;  (** Of a batch within its chunk, from zero. *)
  length : int;  (** Of the payload on the wire. *)
  raw : int;  (** Of the payload decoded. *)
  checksum : int32;  (** Low half of xxh3-64 of the payload on the wire. *)
}

val header_bytes : int
val max_payload : int

val start : Quiver_core.Byte_buffer.t @ local -> unit
(** Empties the buffer and reserves room for a header before the payload. *) @@ portable

val seal :
  Quiver_core.Byte_buffer.t @ local -> kind -> codec -> raw:int -> chunk:int -> unit
(** Writes the header for the payload appended after {!start}, numbered 0. *) @@ portable

val number : Bytes.t -> seq:int -> unit @@ portable
(** Renumbers a sealed frame. *)

val header : string -> (header, [> `Frame of error ]) result
(** Of the first {!header_bytes} bytes of a frame. *) @@ portable

val payload : header -> string -> (unit, [> `Frame of error ]) result
(** Checks the payload read after the header. *) @@ portable
