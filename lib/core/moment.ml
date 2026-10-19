type t = int [@@deriving sexp_of ~portable, equal ~portable]

let micros_per_second = 1_000_000
let micros_per_day = 86_400 * micros_per_second
let max_year = 9999

let[@inline] [@zero_alloc] days_since_epoch date =
  Core.Date.Days.diff (Core.Date.Days.of_date date) Core.Date.Days.unix_epoch

let[@inline] [@zero_alloc] date_of_days days =
  Core.Date.Days.to_date (Core.Date.Days.add_days Core.Date.Days.unix_epoch days)

let of_date date = days_since_epoch date * micros_per_day
let first = of_date (Core.Date.create_exn ~y:0 ~m:Jan ~d:1)
let last = of_date (Core.Date.create_exn ~y:max_year ~m:Dec ~d:31) + micros_per_day - 1
let of_micros micros = Base.Option.some_if (micros >= first && micros <= last) micros

let days t = Base.Int.(t /% micros_per_day)

let date t = date_of_days (days t)

let micros_of_day t = t - (days t * micros_per_day)
