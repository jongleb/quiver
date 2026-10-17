@@ portable

(** Work on threads of [Multicore], checked by the compiler to share nothing
    unprotected with other threads; each may block on its own I/O. *)

type error : immutable_data [@@deriving sexp_of ~portable]
(** Why the system did not start a thread. *)

type ('a : value mod contended portable) task : value mod contended portable
(** Work running on a thread of its own. *)

val start :
  ('a : value mod contended portable).
  (unit -> 'a) @ once portable unyielding ->
  ('a task, [> `Concurrent of error ]) result

val join : 'a task -> 'a
(** Waits for the work and returns what it returned. *)

val each :
  ('a : value mod contended portable). int -> (int -> 'a) @ portable -> 'a list
(** Runs the work for [0] to [count - 1], each on a thread of its own, or on
    the caller's thread when the system gives no thread, and returns what each
    returned, in order. *)

val both :
  ('a : value mod contended portable) 'b.
  (unit -> 'a) @ portable -> (unit -> 'b) @ local once -> 'a * 'b
(** Runs the first on a thread of its own, or after the second when the system
    gives no thread, and the second on the caller's thread. *)

val spread :
  ('a : value mod contended portable) ('b : value mod contended portable).
  workers:int -> 'a iarray -> ('a -> 'b) @ portable -> 'b list
(** Runs the work for every item on at most [workers] threads, each taking the
    next item no thread took yet, and returns what each returned, in item
    order. *)
