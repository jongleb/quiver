let between (low : Key.value) (high : Key.value) ~count =
  let steps = List.init (Int.max 0 (count - 1)) (fun i -> i + 1) in
  let along lo hi =
    let width = Int64.sub hi lo in
    let step = Int64.unsigned_div width (Int64.of_int (Int.max 1 count)) in
    List.map (fun i -> Int64.add lo (Int64.mul step (Int64.of_int i))) steps
  in
  let prefix a b =
    let rec go i =
      if i < String.length a && i < String.length b && a.[i] = b.[i] then
        go (i + 1)
      else i
    in
    go 0
  in
  match (low, high) with
  | Signed lo, Signed hi -> List.map (fun v -> Key.Signed v) (along lo hi)
  | Unsigned lo, Unsigned hi -> List.map (fun v -> Key.Unsigned v) (along lo hi)
  | Duration lo, Duration hi -> List.map (fun v -> Key.Duration v) (along lo hi)
  | Day lo, Day hi ->
      List.filter_map
        (fun micros ->
          Option.map
            (fun moment -> Key.Day (Moment.date moment))
            (Moment.of_micros (Int64.to_int micros)))
        (along
           (Int64.of_int (Moment.of_date lo :> int))
           (Int64.of_int (Moment.of_date hi :> int)))
  | Instant lo, Instant hi ->
      List.filter_map
        (fun micros ->
          Option.map
            (fun moment -> Key.Instant moment)
            (Moment.of_micros (Int64.to_int micros)))
        (along (Int64.of_int (lo :> int)) (Int64.of_int (hi :> int)))
  | Exact lo, Exact hi -> (
      match (float_of_string_opt lo, float_of_string_opt hi) with
      | Some lo, Some hi ->
          List.map
            (fun i ->
              Key.Exact
                (Printf.sprintf "%.17g"
                   (lo +. ((hi -. lo) *. float_of_int i /. float_of_int count))))
            steps
      | _ -> [])
  | Binary lo, Binary hi ->
      let shared = prefix lo hi in
      let tail text =
        List.fold_left
          (fun acc k ->
            Int64.logor (Int64.shift_left acc 8)
              (Int64.of_int
                 (if shared + k < String.length text then
                    Char.code text.[shared + k]
                  else 0)))
          0L (List.init 8 Fun.id)
      in
      List.map
        (fun v ->
          Key.Binary
            (String.sub lo 0 shared
            ^ String.init 8 (fun k ->
                Char.chr
                  (Int64.to_int
                     (Int64.logand
                        (Int64.shift_right_logical v (8 * (7 - k)))
                        0xFFL)))))
        (let lo = tail lo and hi = tail hi in
         if Int64.unsigned_compare lo hi <= 0 then along lo hi else along hi lo)
  | Text lo, Text hi -> (
      let points text =
        Array.of_seq
          (Seq.unfold
             (fun i ->
               if i >= String.length text then None
               else
                 let decoded = String.get_utf_8_uchar text i in
                 Some
                   ( Uchar.to_int (Uchar.utf_decode_uchar decoded),
                     i + Uchar.utf_decode_length decoded ))
             0)
      in
      let lo = points lo and hi = points hi in
      let shared =
        let rec go i =
          if i < Array.length lo && i < Array.length hi && lo.(i) = hi.(i) then
            go (i + 1)
          else i
        in
        go 0
      in
      let rest points =
        Array.sub points shared (Array.length points - shared)
      in
      let lo_rest = rest lo and hi_rest = rest hi in
      let alphabet =
        Array.of_list
          (List.sort_uniq Int.compare
             (Array.to_list lo_rest @ Array.to_list hi_rest))
      in
      let base = Array.length alphabet in
      match base with
      | 0 | 1 -> []
      | _ ->
          let width =
            let rec grow width span =
              if span >= 64 * count || span > max_int / base / base then width
              else grow (width + 1) (span * base)
            in
            grow 1 base
          in
          let number rest =
            Seq.fold_left
              (fun acc k ->
                (acc * base)
                +
                if k < Array.length rest then
                  Option.value ~default:0
                    (Array.find_index (Int.equal rest.(k)) alphabet)
                else 0)
              0 (Seq.init width Fun.id)
          in
          let utf8 points =
            let out = Buffer.create 16 in
            let first_surrogate = 0xD800 and last_surrogate = 0xDFFF in
            Array.iter
              (fun point ->
                Buffer.add_utf_8_uchar out
                  (Uchar.of_int
                     (if point >= first_surrogate && point <= last_surrogate
                      then last_surrogate + 1
                      else point)))
              points;
            Buffer.contents out
          in
          let spell v =
            let digits = Array.make width alphabet.(0) in
            let remaining = ref v in
            for k = width - 1 downto 0 do
              digits.(k) <- alphabet.(!remaining mod base);
              remaining := !remaining / base
            done;
            digits
          in
          let low = number lo_rest and high = number hi_rest in
          List.map
            (fun v ->
              Key.Text
                (utf8
                   (Array.append (Array.sub lo 0 shared)
                      (spell (Int64.to_int v)))))
            (along
               (Int64.of_int (Int.min low high))
               (Int64.of_int (Int.max low high))))
  | ( ( Signed _ | Unsigned _ | Text _ | Binary _ | Exact _ | Day _ | Instant _
      | Duration _ ),
      _ ) ->
      []
