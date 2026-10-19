(** A lock-free stack of values with one owner each: [put] gives a value up,
    [take] hands it to exactly one taker, on any thread. *)

type 'a t : value mod contended portable

val create : unit -> 'a t @@ portable
val take : 'a t -> 'a or_null @ unique @@ portable
val put : 'a t -> 'a @ once portable unique -> unit @@ portable
