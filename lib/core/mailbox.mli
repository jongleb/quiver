@@ portable

type ('a : value mod contended portable) t : value mod contended portable
(** A queue between threads: [take] waits for a message, and [close] ends it. *)

(** Whether a message was queued. *)
type delivery = Delivered | Closed

val create : unit -> 'a t
val add : 'a t -> 'a -> delivery

val take : 'a t -> 'a option
(** [None] once the mailbox is closed and every message taken. *)

val close : 'a t -> unit
