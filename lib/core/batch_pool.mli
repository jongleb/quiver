@@ portable

type t : value mod contended portable
(** A fixed set of batches reused across chunks of any table; taking from an
    empty pool blocks until a batch is given back (backpressure). A batch has
    one owner from [take] to [give]. *)

val create : batches:int -> t
val take : t -> Ltype.t list -> Batch.t @ unique
val give : t -> Batch.t @ once unique -> unit

val waited : t -> Core.Time_ns.Span.t
(** How long taking blocked on an empty pool, in all. *)
