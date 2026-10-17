@@ portable

(** Values in capsules of their own, each behind a mutex that any thread may
    take: a thread holding the mutex owns the value. *)

val create :
  ('a : value) ('e : value mod contended portable).
  (unit -> ('a, 'e) result) @ local once portable ->
  ('a Await.Capsule.Await.With_mutex.t, 'e) result
(** Builds the value inside a fresh capsule, unless building it fails. *)

val hold :
  'a Await.Capsule.Await.With_mutex.t ->
  ('a -> 'b @ contended portable) @ local once portable ->
  'b @ contended portable
(** Runs the function on the value while holding its mutex. *)
