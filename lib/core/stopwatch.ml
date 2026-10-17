type t = Core.Int63.t

let now =
  match Core_unix.Clock.gettime with
  | Ok gettime -> fun () -> gettime Monotonic
  | Error _ -> Time_now.nanosecond_counter_for_timing

let start () = now ()

let elapsed started =
  Core.Time_ns.Span.of_int63_ns (Core.Int63.( - ) (now ()) started)
