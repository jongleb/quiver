@@ portable

(** CRC-64/XZ (ECMA-182, reflected), as TiKV's checksum of a table computes it for each key and
    value. *)

val pair : string -> string -> int64
(** The CRC of the key followed by the value, without joining them. *)
