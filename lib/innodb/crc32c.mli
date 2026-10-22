(** CRC-32C (Castagnoli), the checksum of InnoDB pages. *)

val digest : Bytes.t -> pos:int -> len:int -> int @@ portable
(** Of [len] bytes at [pos], as an unsigned 32-bit value. *)
