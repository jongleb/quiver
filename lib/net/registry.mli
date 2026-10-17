@@ portable

(** Values published under a number, awaited from any thread, possibly before
    they are published: the chunks of a session by their number. *)

type ('a : value mod contended portable) t : value mod contended portable

val create : unit -> 'a t
val publish : 'a t -> int -> 'a -> unit

val await : 'a t -> int -> 'a option
(** [None] once the registry is closed and nothing was published there. *)

val forget : 'a t -> int -> unit
val close : 'a t -> unit
