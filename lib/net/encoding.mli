@@ portable

(** Which rung of a ladder of encodings, from the cheapest to the most
    compact, a session's batches use: a step up while the network holds the
    dump back, down while the network waits for rendering, kept only when the
    dump goes faster. Decided from one sample per interval, so the same samples
    give the same decisions. *)

type sample = {
  delivered : int;
      (** Bytes of batches, before encoding, the collector acknowledged during
          the interval. *)
  waiting : float;
      (** Share of the interval that rendering waited for room among the frames
          to send. *)
  idle : float;
      (** Share of the interval that the connections waited for frames. *)
}

type t

val start : rungs:int -> at:int -> t
(** A ladder of so many rungs, on rung [at]. *)

val rung : t -> int

(** Whether a trial is being measured. *)
type activity = Watching | Trying

val activity : t -> activity
val step : t -> sample -> t
