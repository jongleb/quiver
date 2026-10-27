type t = int Portable.Atomic.t

let create threads = Portable.Atomic.make (Int.max 0 threads)

let rec take t ~wanted =
  let free = Portable.Atomic.get t in
  let lent = Int.max 0 (Int.min wanted free) in
  match Portable.Atomic.compare_and_set t ~if_phys_equal_to:free ~replace_with:(free - lent) with
  | Set_here -> lent
  | Compare_failed -> take t ~wanted

let give t lent = Portable.Atomic.update t ~pure_f:(fun free -> free + lent)
