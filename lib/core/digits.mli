@@ portable

(** Decimal and hex text written straight into an output buffer: room is
    reserved once and digits are stored in place, without [Printf] (whose C
    formatting takes a process-wide locale lock) or intermediate strings. *)

val[@zero_alloc] int : Byte_buffer.t @ local -> int -> unit
val[@zero_alloc] padded : Byte_buffer.t @ local -> width:int -> int -> unit
val[@zero_alloc] two : Byte_buffer.t @ local -> int -> unit
val[@zero_alloc] clock : Byte_buffer.t @ local -> hours:int -> minutes:int -> seconds:int -> unit
val[@zero_alloc] int64 : Byte_buffer.t @ local -> int64# -> unit
val[@zero_alloc] uint64 : Byte_buffer.t @ local -> int64# -> unit
