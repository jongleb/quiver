val await : Await.Await.t @@ portable
(** Waits by blocking the thread of the operating system, never terminated. *)

val sockets : unit Base.Portable_lazy.t @@ portable
(** Forced before a socket is first used: a write to a socket the peer closed
    fails with [EPIPE] instead of killing the process with [SIGPIPE]. *)

(** What a descriptor is waited for. *)
type direction = Readable | Writable

(** How waiting ended. *)
type readiness = Ready | Timed_out

val wait : ?seconds:float -> direction -> Unix.file_descr -> readiness @@ portable
(** Blocks this thread, not the others, until the descriptor is ready or so
    many seconds passed; a descriptor closed or reset counts as ready. *)
