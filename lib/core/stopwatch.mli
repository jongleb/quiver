@@ portable

(** A moment on the monotonic clock, in nanoseconds, for timing work. *)

type t : immutable_data

val start : unit -> t
val elapsed : t -> Core.Time_ns.Span.t
