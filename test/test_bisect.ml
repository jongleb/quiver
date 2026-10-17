let span = 10_000

let middle lo hi =
  let lo = Option.value lo ~default:0
  and top = Option.value hi ~default:span - 1 in
  Ok
    (match top > lo with
    | true -> Some (lo + ((top - lo + 1) / 2))
    | false -> None)

let count keys lo hi =
  List.length
    (List.filter
       (fun key ->
         Option.fold ~none:true ~some:(fun lo -> key >= lo) lo
         && Option.fold ~none:true ~some:(fun hi -> key < hi) hi)
       keys)

let keys =
  QCheck2.Gen.(
    let* clusters =
      list_size (int_range 1 4)
        (pair (int_range 0 (span - 1)) (int_range 1 400))
    in
    let+ scattered = list_size (int_range 0 200) (int_range 0 (span - 1)) in
    List.sort_uniq Int.compare
      (scattered
      @ List.concat_map
          (fun (start, length) ->
            List.init length (fun i -> start + i)
            |> List.filter (fun key -> key < span))
          clusters))

let balanced =
  QCheck2.Test.make ~count:500
    ~name:
      "bisected ranges are ordered, within the limit, never light in pairs and \
       never heavy unless the limit cut them short"
    ~print:(fun (keys, target, limit, workers) ->
      Printf.sprintf "%d keys, target %d, limit %d, %d workers"
        (List.length keys) target limit workers)
    QCheck2.Gen.(
      quad keys (int_range 1 300)
        (oneof [ int_range 0 200; pure span ])
        (int_range 1 4))
    (fun (keys, target, limit, workers) ->
      match
        Quiver_core.Bisect.boundaries ~workers
          ~weigh:(fun lo hi -> Ok (count keys lo hi))
          ~middle ~target ~limit
      with
      | Error () -> false
      | Ok bounds ->
          let edges = None :: List.map Option.some bounds in
          let ranges =
            List.combine edges (List.map Option.some bounds @ [ None ])
          in
          let weights = List.map (fun (lo, hi) -> count keys lo hi) ranges in
          let rec pairs = function
            | a :: (b :: _ as rest) ->
                float_of_int (a + b) > 1.1 *. float_of_int target && pairs rest
            | [ _ ] | [] -> true
          in
          List.length bounds <= limit
          && List.for_all (fun bound -> bound > 0 && bound < span) bounds
          && List.sort_uniq Int.compare bounds = bounds
          && pairs weights
          && (limit < span
             || List.for_all
                  (fun weight ->
                    float_of_int weight <= (1.5 *. float_of_int target) +. 1.)
                  weights))

let tests = List.map QCheck_alcotest.to_alcotest [ balanced ]
