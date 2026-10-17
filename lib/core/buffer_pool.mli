@@ portable

(** Byte buffers shared by every domain of a dump: a lock-free free list, so
    taking one never blocks and work reuses buffers instead of allocating. A
    buffer is owned by one holder at a time, from [take] to [give]. *)

type t : value mod contended portable

val create : unit -> t

val take : t -> Byte_buffer.t @ unique
(** An empty buffer, reused when one was given back. *)

val give : t -> Byte_buffer.t @ once unique -> unit
