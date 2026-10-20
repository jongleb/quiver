@@ portable

(** How many workers of one kind run: more while they are what holds the work
    back and more of them carry more, back to as many as before when a trial
    did not pay. Decided from one sample per interval, so the same samples give
    the same decisions. Data connections of an agent and readers of a dump use
    it. *)

type sample = {
  delivered : int;  (** Bytes the workers moved during the interval. *)
  waiting : float;
      (** Share of the interval the next stage waited on the workers. *)
}

type decision = Keep | Open of int | Close of int [@@deriving sexp_of ~portable]

type t

val start : width:int -> limit:int -> t
(** [width] running now, at most [limit]. *)

val width : t -> int

(** Whether a trial is being measured. *)
type activity = Watching | Trying

val activity : t -> activity
val step : t -> sample -> t * decision
