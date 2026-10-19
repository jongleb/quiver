let between (low : Key.value) (high : Key.value) ~count =
  let steps = Base.List.range 1 count in
  let along lo hi =
    let step = Int64.unsigned_div (Int64.sub hi lo) (Int64.of_int (Int.max 1 count)) in
    List.map (fun i -> Int64.add lo (Int64.mul step (Int64.of_int i))) steps
  in
  let moments lo hi =
    List.filter_map
      (fun micros -> Moment.of_micros (Int64.to_int micros))
      (along (Int64.of_int lo) (Int64.of_int hi))
  in
  let decimals lo hi =
    List.map
      (fun i ->
        Key.Exact
          (Printf.sprintf "%.17g" (lo +. ((hi -. lo) *. float_of_int i /. float_of_int count))))
      steps
  in
  let bytes lo hi =
    let shared = Base.String.common_prefix2_length lo hi in
    let tail s =
      let b = Bytes.make 8 '\000' in
      Bytes.blit_string s shared b 0 (Int.min 8 (String.length s - shared));
      Bytes.get_int64_be b 0
    in
    let spell v =
      let b = Bytes.create 8 in
      Bytes.set_int64_be b 0 v;
      Key.Binary (String.sub lo 0 shared ^ Bytes.to_string b)
    in
    let lo = tail lo and hi = tail hi in
    List.map spell (if Int64.unsigned_compare lo hi <= 0 then along lo hi else along hi lo)
  in
  let text lo hi =
    let points s =
      let rec decode i acc =
        if i >= String.length s then Array.of_list (List.rev acc)
        else
          let d = String.get_utf_8_uchar s i in
          decode (i + Uchar.utf_decode_length d) (Uchar.to_int (Uchar.utf_decode_uchar d) :: acc)
      in
      decode 0 []
    in
    let lo = points lo and hi = points hi in
    let shared =
      let rec go i =
        if i < Array.length lo && i < Array.length hi && lo.(i) = hi.(i) then go (i + 1) else i
      in
      go 0
    in
    let rest a = Array.sub a shared (Array.length a - shared) in
    let lo_rest = rest lo and hi_rest = rest hi in
    let alphabet =
      Array.of_list
        (List.sort_uniq Int.compare (Array.to_list lo_rest @ Array.to_list hi_rest))
    in
    let base = Array.length alphabet in
    if base < 2 then []
    else
      let width =
        let rec grow width span =
          if span >= 64 * count || span > max_int / base / base then width
          else grow (width + 1) (span * base)
        in
        grow 1 base
      in
      let number a =
        Base.List.fold (Base.List.range 0 width) ~init:0 ~f:(fun acc k ->
            let digit =
              if k < Array.length a then
                Option.value ~default:0 (Array.find_index (Int.equal a.(k)) alphabet)
              else 0
            in
            (acc * base) + digit)
      in
      let spell v =
        let rec digits k v acc =
          if k = 0 then acc else digits (k - 1) (v / base) (alphabet.(v mod base) :: acc)
        in
        let out = Buffer.create 16 in
        List.iter
          (fun point -> Buffer.add_utf_8_uchar out (Uchar.of_int point))
          (Array.to_list (Array.sub lo 0 shared) @ digits width v []);
        Key.Text (Buffer.contents out)
      in
      let low = number lo_rest and high = number hi_rest in
      List.map
        (fun v -> spell (Int64.to_int v))
        (along (Int64.of_int (Int.min low high)) (Int64.of_int (Int.max low high)))
  in
  match (low, high) with
  | Signed lo, Signed hi -> List.map (fun v -> Key.Signed v) (along lo hi)
  | Unsigned lo, Unsigned hi -> List.map (fun v -> Key.Unsigned v) (along lo hi)
  | Duration lo, Duration hi -> List.map (fun v -> Key.Duration v) (along lo hi)
  | Day lo, Day hi ->
      List.map
        (fun moment -> Key.Day (Moment.date moment))
        (moments (Moment.of_date lo :> int) (Moment.of_date hi :> int))
  | Instant lo, Instant hi ->
      List.map (fun moment -> Key.Instant moment) (moments (lo :> int) (hi :> int))
  | Exact lo, Exact hi ->
      Option.value ~default:[]
        (Base.Option.map2 ~f:decimals (float_of_string_opt lo) (float_of_string_opt hi))
  | Binary lo, Binary hi -> bytes lo hi
  | Text lo, Text hi -> text lo hi
  | ( ( Signed _ | Unsigned _ | Text _ | Binary _ | Exact _ | Day _ | Instant _
      | Duration _ ),
      _ ) ->
      []
