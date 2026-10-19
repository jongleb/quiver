open Base.Result.Let_syntax

(** Whether a range may still be cut. *)
type state = Cuttable | Final

type 'k range = { lo : 'k option; hi : 'k option; weight : int; state : state }

(** A range after a cut: known, or a half still to be weighed. *)
type 'k piece = Known of 'k range | Half of 'k option * 'k option

(** Neighbouring ranges joined up to [last], with their scaled weight. *)
type 'k group = { last : 'k option; size : float }

let boundaries (type (k : value mod contended portable) (e : value mod contended portable))
    ~workers ~(weigh : k option -> k option -> (int, e) result) ~middle ~target
    ~limit =
  let rec split (l : k range list) =
    let n = limit - (List.length l - 1) in
    let sorted =
      List.stable_sort
        (fun (_, a) (_, b) -> Int.compare b a)
        (Base.List.filter_mapi l ~f:(fun i r ->
             match r.state with
             | Cuttable when r.weight > target -> Some (i, r.weight)
             | Cuttable | Final -> None))
    in
    let set =
      Base.Set.of_list (module Base.Int) (List.map fst (Base.List.take sorted n))
    in
    if Base.Set.is_empty set then Ok l
    else
      let%bind xs =
        Base.Result.all
          (Base.List.mapi l ~f:(fun i r ->
               if Base.Set.mem set i then
                 let%map key = middle r.lo r.hi in
                 Option.fold key
                   ~none:[ Known { r with state = Final } ]
                   ~some:(fun key -> [ Half (r.lo, Some key); Half (Some key, r.hi) ])
               else Ok [ Known r ]))
      in
      let xs = List.concat xs in
      let count = Base.List.count xs ~f:(function Half _ -> true | Known _ -> false) in
      let%bind l =
        Base.Result.all
          (Concurrent.spread ~workers:(Int.min workers count) (Iarray.of_list xs)
             (function
               | Half (lo, hi) ->
                   let%map weight = weigh lo hi in
                   { lo; hi; weight; state = Cuttable }
               | Known r -> Ok r))
      in
      split l
  in
  let%bind n = weigh None None in
  let%map l = split [ { lo = None; hi = None; weight = n; state = Cuttable } ] in
  let sum = Base.List.sum (module Base.Int) l ~f:(fun r -> r.weight)
  and target = float_of_int target in
  let max = 1.1 *. target and min = 0.2 *. target in
  let merge a b = { last = b.last; size = a.size +. b.size } in
  let join acc r =
    let size =
      if sum = 0 then 0. else float_of_int r.weight *. float_of_int n /. float_of_int sum
    in
    match acc with
    | x :: rest when x.size +. size <= max -> { last = r.hi; size = x.size +. size } :: rest
    | _ -> { last = r.hi; size } :: acc
  in
  let rec loop l =
    let rec aux before = function
      | [] -> l
      | x :: after when x.size >= min -> aux (x :: before) after
      | x :: after -> (
          match (before, after) with
          | [], [] -> l
          | prev :: before, [] -> loop (List.rev_append before [ merge prev x ])
          | [], next :: after -> loop (merge x next :: after)
          | prev :: before, next :: after ->
              if prev.size <= next.size then
                loop (List.rev_append before (merge prev x :: next :: after))
              else loop (List.rev_append (prev :: before) (merge x next :: after)))
    in
    aux [] l
  in
  List.filter_map (fun x -> x.last) (loop (List.rev (List.fold_left join [] l)))

let integer_middle ~min ~max lo hi =
  let lo = Option.value lo ~default:min
  and top = Option.fold ~none:max ~some:Int64.pred hi in
  let len = Int64.sub top lo in
  Base.Option.some_if
    (not (Int64.equal len 0L))
    (Int64.add lo (Int64.add (Int64.shift_right_logical len 1) (Int64.logand len 1L)))
