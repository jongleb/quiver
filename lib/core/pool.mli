@@ portable

(** A few threads taking items in turn, for work that would otherwise start a thread per
    item: every thread takes part in each garbage collection, so hundreds of them cost all
    the others. *)

type ('a : value mod contended portable) t : value mod contended portable

(** Where an item went. *)
type submitted =
  | Queued  (** A thread of the pool will take it. *)
  | Unqueued  (** The pool has no thread: the caller does the work. *)

val start : ('a : value mod contended portable). workers:int -> ('a -> unit) @ portable -> 'a t
(** Up to [workers] threads, fewer when the system gives fewer. *)

val submit : ('a : value mod contended portable). 'a t -> 'a -> submitted

val stop : ('a : value mod contended portable). 'a t -> unit
(** Once every item submitted before is done, ends the threads. *)
