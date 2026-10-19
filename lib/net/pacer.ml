type t = {
  origin : Quiver_core.Stopwatch.t;
  next : int Portable.Atomic.t;
      (** Nanoseconds after [origin] when the next write may start. *)
  nanoseconds_per_byte : float;
}

let create ~bytes_per_second =
  {
    origin = Quiver_core.Stopwatch.start ();
    next = Portable.Atomic.make 0;
    nanoseconds_per_byte = 1e9 /. Float.of_int (Int.max 1 bytes_per_second);
  }

let wait t ~bytes =
  let cost = Float.to_int (Float.of_int bytes *. t.nanoseconds_per_byte) in
  let rec reserve () =
    let now =
      Core.Time_ns.Span.to_int_ns (Quiver_core.Stopwatch.elapsed t.origin)
    and next = Portable.Atomic.get t.next in
    let start = Int.max now next in
    match
      Portable.Atomic.compare_and_set t.next ~if_phys_equal_to:next
        ~replace_with:(start + cost)
    with
    | Set_here -> start - now
    | Compare_failed -> reserve ()
  in
  match reserve () with
  | 0 -> 0
  | ahead ->
      Unix.sleepf (Float.of_int ahead /. 1e9);
      ahead
