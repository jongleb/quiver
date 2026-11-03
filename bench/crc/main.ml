let () =
  let value = String.make 60 'v' and key = String.make 19 'k' in
  let n = 1_000_000 in
  let started = Unix.gettimeofday () in
  let acc = ref 0L in
  Base.Fn.apply_n_times ~n (fun () -> acc := Int64.logxor !acc (Quiver_tikv.Crc64.pair key value)) ();
  let seconds = Unix.gettimeofday () -. started in
  Printf.printf "%d pairs of 79 bytes: %.3f s, %.2f ns/byte (%Lx)\n" n seconds (seconds *. 1e9 /. float (n * 79)) !acc
