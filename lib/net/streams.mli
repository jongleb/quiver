@@ portable

(** How many data connections a session uses: more while the network is what
    holds the dump back and more connections carry more, back to as many as
    before when a trial did not pay. Decided from one sample per interval, so
    the same samples give the same decisions. *)

type sample = {
  delivered : int;  (** Bytes the collector acknowledged during the interval. *)
  waiting : float;
      (** Share of the interval that rendering waited for room among the frames
          to send. *)
}

type decision = Keep | Open of int | Close of int [@@deriving sexp_of ~portable]

type t

val start : streams:int -> limit:int -> t
(** [streams] open now, at most [limit]. *)

val streams : t -> int

(** Whether a trial is being measured. *)
type activity = Watching | Trying

val activity : t -> activity
val step : t -> sample -> t * decision
