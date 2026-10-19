@@ portable

(** Holds writes back to at most so many bytes a second, all of them together:
    each write gets the next stretch of time its bytes need, so the rate holds
    over any stretch longer than one write, and time left idle is not saved up
    for a burst. *)

type t : value mod contended portable

val create : bytes_per_second:int -> t

val wait : t -> bytes:int -> int
(** Blocks this thread until so many bytes may go: the nanoseconds it waited. *)
