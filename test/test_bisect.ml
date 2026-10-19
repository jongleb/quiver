open Sexplib0.Sexp_conv

let span = 10_000

let middle lo hi =
  let lo = Option.value lo ~default:0
  and top = Option.value hi ~default:span - 1 in
  Ok (if top > lo then Some (lo + ((top - lo + 1) / 2)) else None)

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

(** How the fake server estimates and cuts ranges. *)
type server = {
  keys : int list;
  widening : int;  (** Extra rows per this many keys of width: wide ranges overestimated. *)
  stuck : int;  (** A range whose bounds sum to a multiple of it cannot be cut; 0 never. *)
  broken : int option;  (** Estimating a range that starts here fails. *)
}
[@@deriving sexp_of]

let estimate server lo hi =
  let low = Option.value lo ~default:0 and high = Option.value hi ~default:span in
  if Option.equal Int.equal server.broken lo && Option.is_some lo then Error lo
  else
    Ok
      (count server.keys lo hi
      + (if server.widening = 0 then 0 else (high - low) / server.widening)
      + ((low * 31) + high) mod 7)

let cut server lo hi =
  let low = Option.value lo ~default:0 and high = Option.value hi ~default:span in
  if server.stuck > 0 && (low + high) mod server.stuck = 0 then Ok None
  else middle lo hi

let model server ~target ~limit =
  let ( let* ) = Result.bind in
  let rec split ranges =
    let room = limit - (Array.length ranges - 1) in
    let candidates =
      List.filter
        (fun i ->
          let _, _, weight, cuttable = ranges.(i) in
          cuttable && weight > target)
        (List.init (Array.length ranges) Fun.id)
    in
    let by_weight =
      List.stable_sort
        (fun a b ->
          let _, _, wa, _ = ranges.(a) and _, _, wb, _ = ranges.(b) in
          Int.compare wb wa)
        candidates
    in
    let chosen = List.filteri (fun rank _ -> rank < room) by_weight in
    if chosen = [] then Ok ranges
    else
      let* next =
        List.fold_left
          (fun acc i ->
            let* acc = acc in
            let lo, hi, weight, cuttable = ranges.(i) in
            if List.mem i chosen then
              let* key = cut server lo hi in
              match key with
              | None -> Ok ((lo, hi, weight, false) :: acc)
              | Some key ->
                  let* left = estimate server lo (Some key) in
                  let* right = estimate server (Some key) hi in
                  Ok ((Some key, hi, right, true) :: (lo, Some key, left, true) :: acc)
            else Ok ((lo, hi, weight, cuttable) :: acc))
          (Ok []) (List.init (Array.length ranges) Fun.id)
      in
      split (Array.of_list (List.rev next))
  in
  let* total = estimate server None None in
  let* ranges = split [| (None, None, total, true) |] in
  let estimated = Array.fold_left (fun sum (_, _, w, _) -> sum + w) 0 ranges in
  let target = float_of_int target in
  let share weight =
    if estimated = 0 then 0.
    else float_of_int weight *. float_of_int total /. float_of_int estimated
  in
  let groups = ref [||] in
  Array.iter
    (fun (_, hi, weight, _) ->
      let s = share weight and n = Array.length !groups in
      if n > 0 && snd !groups.(n - 1) +. s <= 1.1 *. target then
        !groups.(n - 1) <- (hi, snd !groups.(n - 1) +. s)
      else groups := Array.append !groups [| (hi, s) |])
    ranges;
  let rec absorb groups =
    let n = Array.length groups in
    let small = ref None in
    Array.iteri
      (fun i (_, s) -> if !small = None && s < 0.2 *. target && n > 1 then small := Some i)
      groups;
    match !small with
    | None -> groups
    | Some i ->
        let j =
          if i = 0 then 1
          else if i = n - 1 then n - 2
          else if snd groups.(i - 1) <= snd groups.(i + 1) then i - 1
          else i + 1
        in
        let first = min i j and second = max i j in
        let merged = (fst groups.(second), snd groups.(first) +. snd groups.(second)) in
        absorb
          (Array.of_list
             (List.concat
                (List.mapi
                   (fun k g -> if k = first then [ merged ] else if k = second then [] else [ g ])
                   (Array.to_list groups))))
  in
  Ok (List.filter_map fst (Array.to_list (absorb !groups)))

let against_model =
  QCheck2.Test.make ~count:2000
    ~name:
      "bisected boundaries match a simple sequential model under noisy and \
       widening estimates, uncuttable ranges and failing estimates"
    ~print:(fun (server, target, limit, workers) ->
      Printf.sprintf "%s, target %d, limit %d, %d workers"
        (Sexplib0.Sexp.to_string
           (sexp_of_server { server with keys = [] }))
        target limit workers)
    QCheck2.Gen.(
      let* keys = keys in
      let* widening = oneof [ pure 0; int_range 1 200 ] in
      let* stuck = oneof [ pure 0; int_range 2 9 ] in
      let* broken =
        oneof_weighted
          [ (4, pure None); (1, map Option.some (oneof_list (0 :: keys))) ]
      in
      quad (pure { keys; widening; stuck; broken }) (int_range 1 300)
        (oneof [ int_range 0 200; pure span ])
        (int_range 1 4))
    (fun (server, target, limit, workers) ->
      Quiver_core.Bisect.boundaries ~workers
        ~weigh:(fun lo hi -> estimate server lo hi)
        ~middle:(fun lo hi -> cut server lo hi)
        ~target ~limit
      = model server ~target ~limit)

let tests = List.map QCheck_alcotest.to_alcotest [ balanced; against_model ]
