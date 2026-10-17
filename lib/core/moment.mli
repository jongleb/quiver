@@ portable

type t = private int [@@deriving sexp_of ~portable, equal ~portable]
(** Microseconds since 1970-01-01 00:00 UTC, from the start of year 0 to the
    end of year 9999. *)

val of_micros : int -> t option
val of_date : Core.Date.t -> t
(** The date's midnight. *)

val date : t -> Core.Date.t
val micros_of_day : t -> int
