let () =
  let secret = Quiver_core.Keyed.of_secret "bench" in
  let n = 1_000_000 in
  let time name f =
    let start = Unix.gettimeofday () in
    let rec go i acc = if i > n then acc else go (i + 1) (acc lxor f i) in
    let sink = go 1 0 in
    let seconds = Unix.gettimeofday () -. start in
    Printf.printf "%-28s %6.0f ns per value, %5.2f M per second (%d)\n%!" name (seconds *. 1e9 /. float n)
      (float n /. seconds /. 1e6) (sink land 1)
  in
  let ints = Quiver_core.Keyed.permutation secret ~label:"int:1:1073741823" in
  time "permute INT" (fun i -> Int64.to_int (Quiver_core.Keyed.permute_integer ints ~low:1L ~high:1073741823L (Int64.of_int i)));
  let big = Quiver_core.Keyed.permutation secret ~label:"int:1:9007199254740991" in
  time "permute BIGINT (2^53)" (fun i ->
      Int64.to_int (Quiver_core.Keyed.permute_integer big ~low:1L ~high:9007199254740991L (Int64.of_int i)));
  let text = Quiver_core.Keyed.permutation secret ~label:"text" in
  time "permute email (24 chars)" (fun i ->
      String.length (Option.get (Quiver_core.Keyed.permute_text text (Printf.sprintf "ivan.petrov%07d@mail.ru" i))));
  time "shape email (for scale)" (fun i -> String.length (Quiver_core.Keyed.email secret (Printf.sprintf "ivan.petrov%07d@mail.ru" i)))
