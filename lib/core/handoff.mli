@@ portable

(** Moving a unique value to another thread, which the type system cannot see
    is safe: the only unchecked cast of the threads. *)

external own : ('a : value_or_null). 'a @ contended once unique -> 'a @ unique
  @@ portable
  = "%identity"
(** A value taken from a queue or stack between threads: it was put there
    [unique], so the taker is its only holder and may use it as its own. Only
    for types whose interior never escapes their owner (buffers, batches,
    rendered output, messages). *)
