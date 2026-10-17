type error =
  | Closed  (** The connection ended before a whole packet arrived. *)
  | Out_of_order of { expected : int; got : int }
      (** A frame carried an unexpected sequence id. *)
  | Malformed  (** A packet ended before a field, or a length did not fit. *)
[@@deriving sexp_of ~portable]

type t
(** Packets of one connection, parsed in place from a growable buffer. *)

(** What the first byte of a server packet announces. *)
type marker =
  | Ok_packet  (** [0x00]: OK, a prepare answer or a binary row. *)
  | Auth_more_data  (** [0x01]. *)
  | Local_infile  (** [0xFB]; also a NULL cell of a text row. *)
  | Eof_packet  (** [0xFE]: end of rows, or an auth switch request. *)
  | Err_packet  (** [0xFF]. *)
  | Data  (** Anything else: a count or the first field. *)

(** A command the client sends. *)
type command = Query | Prepare | Execute | Close_statement

val marker : int -> marker @@ portable
val ok_byte : int
val eof_byte : int
val err_byte : int
val command : command -> string @@ portable
val frame_limit : int
val create : (Base_bigstring.t -> off:int -> len:int -> int) -> t @@ portable
val restart : t -> unit @@ portable
val frame : t -> string -> string

(** Fields of the current packet, read in order only inside {!Cursor.decode}. *) @@ portable
module Cursor : sig
  type t
  type reader

  val create : unit -> t @@ portable
  val sequence : t -> int @@ portable

  val decode :
    t ->
    (reader -> ('a, ([> `Wire of error ] as 'e)) result) @ local ->
    ('a, 'e) result @@ portable

  val[@zero_alloc] remaining : reader -> int @@ portable
  val[@zero_alloc] peek : reader -> int @@ portable
  val[@zero_alloc] uint8 : reader -> int @@ portable
  val[@zero_alloc] uint16 : reader -> int @@ portable
  val[@zero_alloc] uint24 : reader -> int
  val[@zero_alloc] uint32 : reader -> int @@ portable
  val[@zero_alloc] int64 : reader -> int64# @@ portable
  val[@zero_alloc] lenenc_int : reader -> int @@ portable
  val[@zero_alloc] skip : reader -> int -> unit @@ portable
  val[@zero_alloc] take : reader -> int -> int @@ portable
  val[@zero_alloc] buffer : reader -> Base_bigstring.t @@ portable
  val string : reader -> int -> string @@ portable
  val lenenc_string : reader -> string
  val lenenc_text : reader -> string option @@ portable
  val null_terminated : reader -> string @@ portable
  val rest : reader -> string @@ portable
end

val next : t -> Cursor.t -> (unit, [> `Wire of error ]) result @@ portable
