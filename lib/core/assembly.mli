@@ portable

(** A part of a sink written by many threads. Its batches, rendered in any
    order on any thread, are appended in their order under the part's lock by
    whichever thread delivers the next one; the part closes once its last
    batch is appended, or is discarded at the first failure. *)

module Make (S : Sink_intf.S) : sig
  type t : value mod contended portable

  (** How a sealed part ended. *)
  type outcome =
    | Closed of { part : Manifest.part; writing : Manifest.writing }
        (** Every batch appended, the part durable. *)
    | Failed of S.error  (** Rendering, appending or closing failed. *)

  val create :
    S.t -> Schema.Table.t -> index:int -> (t, S.error) result @@ portable

  type rendered : value mod portable
  (** One batch rendered, or the sink's failure to, and how long it took. *)

  val render : t -> Batch.sealed @ local -> rendered @ unique @@ portable

  val deliver : t -> int -> rendered @ unique -> unit @@ portable
  (** The batch with this number. *)

  val seal :
    t -> batches:int -> (outcome -> unit) @ portable -> unit @@ portable
  (** No batch numbered [batches] or above follows; the outcome reaches the
      callback on whichever thread ends the part. *)

  val abandon : t -> unit @@ portable
  (** Discards the part; batches delivered later are dropped. *)
end
