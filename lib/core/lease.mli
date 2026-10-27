@@ portable

(** Threads a whole run may add at once, lent to pieces of work as they start, so that
    pieces running together never add more than that between them. *)

type t : value mod contended portable

val create : int -> t

val take : t -> wanted:int -> int
(** Up to [wanted] of the threads not lent now, without waiting: none when all are. *)

val give : t -> int -> unit
(** Threads [take] lent, once their work is done. *)
