@@ portable

type t = private int [@@deriving sexp_of ~portable, equal ~portable]
(** Microseconds since 1970-01-01 00:00 UTC, from the start of year 0 to the
    end of year 9999. *)

val micros_per_second : int
val micros_per_day : int
val max_year : int
val[@zero_alloc] days_since_epoch : Core.Date.t -> int
val[@zero_alloc] date_of_days : int -> Core.Date.t
val of_micros : int -> t option
val of_date : Core.Date.t -> t
(** The date's midnight. *)

val date : t -> Core.Date.t
val micros_of_day : t -> int
