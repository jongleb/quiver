@@ portable

(** Where full batches go. [ship] takes a batch over; the consumer gives it
    back to the pool once it has been read, while the writer fills others. *)
module type Consumer = sig
  type error

  val ship : Batch.sealed @ unique -> Batch_pool.t -> (unit, error) result
end

type 'e t
(** Fills batches taken from a pool row by row and ships each to its consumer
    once it reaches [limit_bytes]. ['e] is the error type shared with the
    consumer. *)

(** What the source did for one call. *)
type next = Row  (** It wrote one more row into the columns. *) | End

val create :
  limit_bytes:int ->
  Batch_pool.t ->
  (module Consumer with type error = 'e) ->
  Ltype.t list ->
  'e t

val fill :
  ([> `Batch of Batch.error ] as 'e) t ->
  (Column.any Iarray.t @ local -> (next, 'e) result) @ local ->
  (unit, 'e) result
(** Asks the source for rows until it ends or fails, shipping every full batch
    and then the last one; on failure the batch being filled goes back to the
    pool. *)
