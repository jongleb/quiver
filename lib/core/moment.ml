type t = int [@@deriving sexp_of ~portable, equal ~portable]

let micros_per_day = 86_400 * 1_000_000

let days_since_epoch date =
  Core.Date.Days.diff (Core.Date.Days.of_date date) Core.Date.Days.unix_epoch

let of_date date = days_since_epoch date * micros_per_day
let first = of_date (Core.Date.create_exn ~y:0 ~m:Jan ~d:1)
let last = of_date (Core.Date.create_exn ~y:9999 ~m:Dec ~d:31) + micros_per_day - 1

let of_micros micros =
  match micros >= first && micros <= last with
  | true -> Some micros
  | false -> None

let days t = Base.Int.(t /% micros_per_day)

let date t =
  Core.Date.Days.to_date (Core.Date.Days.add_days Core.Date.Days.unix_epoch (days t))

let micros_of_day t = t - (days t * micros_per_day)
