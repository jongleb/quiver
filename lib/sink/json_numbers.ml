type misread = { path : string; literal : string }

let pow10 = Iarray.of_list (List.init 309 (fun n -> float_of_string ("1e" ^ string_of_int n)))

let unsigned_float x =
  if Int64.compare x 0L >= 0 then Int64.to_float x
  else Int64.to_float (Int64.logor (Int64.shift_right_logical x 1) (Int64.logand x 1L)) *. 2.

(** How RapidJSON ends up holding a number. *)
type reading = Integer | Double of float

let server_reads bytes start stop =
  let peek at = if at < stop then Bytes.get bytes at else '\000' in
  let digit at = peek at >= '0' && peek at <= '9' in
  let minus = peek start = '-' in
  let at = ref (if minus then start + 1 else start) in
  let take () =
    let d = Char.code (peek !at) - 48 in
    incr at;
    d
  in
  let i = ref 0 and i64 = ref 0L and wide = ref false and significant = ref 0 in
  let () =
    if peek !at = '0' then incr at
    else (
      i := take ();
      let limit = if minus then 214748364 else 429496729 and last = if minus then '8' else '5' in
      let rec go () =
        if digit !at then
          if !i >= limit && (!i <> limit || peek !at > last) then (
            i64 := Int64.of_int !i;
            wide := true)
          else (
            i := (!i * 10) + take ();
            incr significant;
            go ())
      in
      go ())
  in
  let floating = ref false and d = ref 0. in
  let () =
    if !wide then
      let limit = if minus then 0x0CCCCCCCCCCCCCCCL else 0x1999999999999999L and last = if minus then '8' else '5' in
      let rec go () =
        if digit !at then
          if Int64.unsigned_compare !i64 limit >= 0 && ((not (Int64.equal !i64 limit)) || peek !at > last) then (
            d := unsigned_float !i64;
            floating := true)
          else (
            i64 := Int64.add (Int64.mul !i64 10L) (Int64.of_int (take ()));
            incr significant;
            go ())
      in
      go ()
  in
  let () =
    if !floating then
      let rec go () =
        if digit !at then (
          d := (!d *. 10.) +. Float.of_int (take ());
          go ())
      in
      go ()
  in
  let fraction = ref 0 in
  let () =
    if peek !at = '.' then (
      incr at;
      if not !floating then (
        if not !wide then i64 := Int64.of_int !i;
        let rec go () =
          if digit !at && Int64.unsigned_compare !i64 0x1FFFFFFFFFFFFFL <= 0 then (
            i64 := Int64.add (Int64.mul !i64 10L) (Int64.of_int (take ()));
            decr fraction;
            if not (Int64.equal !i64 0L) then incr significant;
            go ())
        in
        go ();
        d := unsigned_float !i64;
        floating := true);
      let rec go () =
        if digit !at then (
          if !significant < 17 then (
            d := (!d *. 10.) +. Float.of_int (take ());
            decr fraction;
            if !d > 0. then incr significant)
          else incr at;
          go ())
      in
      go ())
  in
  let exponent = ref 0 in
  let () =
    if peek !at = 'e' || peek !at = 'E' then (
      incr at;
      if not !floating then (
        d := if !wide then unsigned_float !i64 else Float.of_int !i;
        floating := true);
      let negative = peek !at = '-' in
      if peek !at = '+' || negative then incr at;
      let rec go () =
        if digit !at then (
          exponent := (!exponent * 10) + take ();
          go ())
      in
      go ();
      if negative then exponent := - !exponent)
  in
  if !floating then
    let fast d p =
      if p < -308 then 0. else if p >= 0 then d *. Iarray.get pow10 (Int.min p 308) else d /. Iarray.get pow10 (-p)
    in
    let p = !exponent + !fraction in
    let d = if p < -308 then fast (fast !d (-308)) (p + 308) else fast !d p in
    Double (if minus then -.d else d)
  else Integer

let number_end bytes at stop =
  let rec go at =
    if at < stop then match Bytes.get bytes at with '0' .. '9' | '-' | '+' | '.' | 'e' | 'E' -> go (at + 1) | _ -> at
    else at
  in
  go at

(** What the server makes of a number's text. *)
type outcome = Same | Neighbour

let outcome bytes start stop =
  match server_reads bytes start stop with
  | Integer -> Same
  | Double read -> (
      match float_of_string_opt (Bytes.sub_string bytes start (stop - start)) with
      | Some exact when not (Int64.equal (Int64.bits_of_float read) (Int64.bits_of_float exact)) -> Neighbour
      | Some _ | None -> Same)

let string_end bytes at stop =
  let rec go at =
    if at >= stop then stop
    else match Bytes.get bytes at with '\\' -> go (at + 2) | '"' -> at + 1 | _ -> go (at + 1)
  in
  go at

let misread bytes ~pos ~length =
  let stop = pos + length in
  let rec any at =
    if at >= stop then Same
    else
      match Bytes.get bytes at with
      | '"' -> any (string_end bytes (at + 1) stop)
      | '-' | '0' .. '9' -> (
          let after = number_end bytes at stop in
          match outcome bytes at after with Neighbour -> Neighbour | Same -> any after)
      | _ -> any (at + 1)
  in
  match any pos with
  | Same -> []
  | Neighbour ->
    let rec space at =
      if at < stop then match Bytes.get bytes at with ' ' | '\n' | '\r' | '\t' -> space (at + 1) | _ -> at else at
    in
    let peek at = if at < stop then Bytes.get bytes at else '\000' in
    let rec value at path acc =
      match peek at with
      | '{' ->
          let rec members at acc =
            let at = space at in
            match peek at with
            | '"' ->
                let after = string_end bytes (at + 1) stop in
                let key = Bytes.sub_string bytes at (after - at) in
                let at = space (space after + 1) in
                let at, acc = value at (("." ^ key) :: path) acc in
                let at = space at in
                if peek at = ',' then members (at + 1) acc else (at + 1, acc)
            | _ -> (at + 1, acc)
          in
          members (at + 1) acc
      | '[' ->
          let rec items at k acc =
            let at = space at in
            if peek at = ']' || at >= stop then (at + 1, acc)
            else
              let at, acc = value at (("[" ^ string_of_int k ^ "]") :: path) acc in
              let at = space at in
              if peek at = ',' then items (at + 1) (k + 1) acc else (at + 1, acc)
          in
          items (at + 1) 0 acc
      | '"' -> (string_end bytes (at + 1) stop, acc)
      | '-' | '0' .. '9' -> (
          let after = number_end bytes at stop in
          match outcome bytes at after with
          | Same -> (after, acc)
          | Neighbour ->
              let text = Bytes.sub_string bytes at (after - at) in
              let literal = if String.exists (fun c -> c = 'e' || c = 'E') text then text else text ^ "e0" in
              (after, { path = String.concat "" ("$" :: List.rev path); literal } :: acc))
      | _ ->
          let rec word at = if at < stop && Bytes.get bytes at >= 'a' && Bytes.get bytes at <= 'z' then word (at + 1) else at in
          (word (at + 1), acc)
    in
    let _, found = value (space pos) [] [] in
    List.rev found
