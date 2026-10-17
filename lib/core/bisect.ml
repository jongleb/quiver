open Base.Result.Let_syntax
(** Whether a range may still be cut. *)

type state = Cuttable | Final
type 'k range = { lo : 'k option; hi : 'k option; weight : int; state : state }

let boundaries (type (k : value mod contended portable) (e : value mod contended portable))
    ~workers ~(weigh : k option -> k option -> (int, e) result) ~middle ~target
    ~limit =
  let rec grow ranges count =
    let heavy =
      Base.List.take
        (List.stable_sort
           (fun (_, a) (_, b) -> Int.compare b.weight a.weight)
           (List.filter
              (fun (_, range) ->
                match range.state with
                | Cuttable -> range.weight > target
                | Final -> false)
              (List.mapi (fun position range -> (position, range)) ranges)))
        (limit - count)
    in
    match heavy with
    | [] -> Ok ranges
    | _ ->
        let%bind cuts =
          Base.Result.all
            (List.map
               (fun (position, range) ->
                 let%map cut = middle range.lo range.hi in
                 (position, range, cut))
               heavy)
        in
        let (pieces : (k option * k option) list) =
          List.concat_map
            (fun (_, range, cut) ->
              Option.fold ~none:[]
                ~some:(fun key -> [ (range.lo, Some key); (Some key, range.hi) ])
                cut)
            cuts
        in
        let%bind weights =
          Base.Result.all
            (Concurrent.spread ~workers (Iarray.of_list pieces) (fun (lo, hi) ->
                 weigh lo hi))
        in
        let replaced = Hashtbl.create (List.length cuts) in
        let (_ : int list) =
          List.fold_left
            (fun weights (position, range, cut) ->
              match (cut, weights) with
              | Some key, left :: right :: weights ->
                  Hashtbl.replace replaced position
                    [
                      { lo = range.lo; hi = Some key; weight = left; state = Cuttable };
                      { lo = Some key; hi = range.hi; weight = right; state = Cuttable };
                    ];
                  weights
              | Some _, ([] | [ _ ]) | None, _ ->
                  Hashtbl.replace replaced position [ { range with state = Final } ];
                  weights)
            weights cuts
        in
        grow
          (List.concat
             (List.mapi
                (fun position range ->
                  Option.value (Hashtbl.find_opt replaced position)
                    ~default:[ range ])
                ranges))
          (count
          + List.length (List.filter_map (fun (_, _, cut) -> cut) cuts))
  in
  let%bind total = weigh None None in
  let%map ranges =
    grow [ { lo = None; hi = None; weight = total; state = Cuttable } ] 0
  in
  let estimated =
    List.fold_left (fun sum range -> sum + range.weight) 0 ranges
  in
  let scaled weight =
    match estimated with
    | 0 -> 0.
    | _ -> float_of_int weight *. float_of_int total /. float_of_int estimated
  in
  let target = float_of_int target in
  let full = 1.1 *. target and scrap = 0.2 *. target in
  let groups =
    List.rev
      (List.fold_left
         (fun groups range ->
           let weight = scaled range.weight in
           match groups with
           | (_, sum) :: rest when sum +. weight <= full ->
               (range.hi, sum +. weight) :: rest
           | _ -> (range.hi, weight) :: groups)
         [] ranges)
  in
  let rec fold groups =
    let all = Array.of_list groups in
    let count = Array.length all in
    match
      Array.find_index (fun (_, weight) -> weight < scrap && count > 1) all
    with
    | None -> groups
    | Some scraps ->
        let into =
          match scraps with
          | 0 -> 1
          | last when last = count - 1 -> last - 1
          | middle when snd all.(middle - 1) <= snd all.(middle + 1) ->
              middle - 1
          | middle -> middle + 1
        in
        let first = Int.min scraps into and second = Int.max scraps into in
        fold
          (List.concat
             (List.mapi
                (fun at group ->
                  if at = first then
                    [ (fst all.(second), snd all.(first) +. snd all.(second)) ]
                  else if at = second then []
                  else [ group ])
                groups))
  in
  List.filter_map fst (fold groups)

let integer_middle ~min ~max lo hi =
  let lo = Option.value lo ~default:min
  and top = Option.fold ~none:max ~some:Int64.pred hi in
  let gap = Int64.sub top lo in
  match Int64.equal gap 0L with
  | true -> None
  | false ->
      Some
        (Int64.add lo
           (Int64.add (Int64.shift_right_logical gap 1) (Int64.logand gap 1L)))
