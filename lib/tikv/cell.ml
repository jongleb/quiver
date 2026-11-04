let little n width =
  let bytes = Bytes.create 8 in
  Bytes.set_int64_le bytes 0 n;
  Bytes.sub_string bytes 0 width

let signed n =
  let within low high = Int64.compare n low >= 0 && Int64.compare n high <= 0 in
  let width = if within (-128L) 127L then 1 else if within (-32768L) 32767L then 2 else if within (-2147483648L) 2147483647L then 4 else 8 in
  little n width

let unsigned n =
  let width =
    if Int64.unsigned_compare n 0xFFL <= 0 then 1
    else if Int64.unsigned_compare n 0xFFFFL <= 0 then 2
    else if Int64.unsigned_compare n 0xFFFF_FFFFL <= 0 then 4
    else 8
  in
  little n width

let numeral text = text <> "" && String.for_all Base.Char.is_digit text

let number text from length =
  if from >= 0 && length >= 0 && from + length <= String.length text && numeral (String.sub text from length) then
    int_of_string_opt (String.sub text from length)
  else None

let at text pos c = pos >= 0 && pos < String.length text && Char.equal text.[pos] c

let fraction text from ~fsp =
  let length = String.length text - from in
  if length = 0 then Some 0
  else if at text from '.' && length - 1 <= fsp && length - 1 <= 6 then
    Option.map (fun digits -> digits * Base.Int.pow 10 (6 - (length - 1))) (number text (from + 1) (length - 1))
  else None

let calendar text =
  let open Base.Option.Let_syntax in
  let%bind year = number text 0 4 and month = number text 5 2 and day = number text 8 2 in
  if at text 4 '-' && at text 7 '-' && month <= 12 && day <= 31 then Some (year, month, day) else None

let packed (year, month, day) (hour, minute, second) micro =
  let ymd = Int64.of_int ((((year * 13) + month) lsl 5) lor day) and hms = Int64.of_int ((hour lsl 12) lor (minute lsl 6) lor second) in
  unsigned (Int64.logor (Int64.shift_left (Int64.logor (Int64.shift_left ymd 17) hms) 24) (Int64.of_int micro))

let comparable f =
  if Float.is_finite f then (
    let bits = Int64.bits_of_float f in
    let bits = if f >= 0. then Int64.logor bits Int64.min_int else Int64.lognot bits in
    let bytes = Bytes.create 8 in
    Bytes.set_int64_be bytes 0 bits;
    Some (Bytes.to_string bytes))
  else None

let real text =
  if text <> "" && String.for_all (function '0' .. '9' | '.' | 'e' | 'E' | '+' | '-' -> true | _ -> false) text then float_of_string_opt text
  else None

let member elements text = Option.map fst (Base.List.findi elements ~f:(fun _ element -> String.equal element text))

let of_default (kind : Info.kind) text =
  match kind with
  | Integer Signed ->
      let from = if at text 0 '-' then 1 else 0 in
      if numeral (String.sub text from (String.length text - from)) then Option.map signed (Int64.of_string_opt text) else None
  | Integer Unsigned -> if numeral text then Option.map unsigned (Int64.of_string_opt ("0u" ^ text)) else None
  | Year -> (
      match (String.length text, number text 0 (String.length text)) with
      | (1 | 2), Some n when n < 70 -> Some (signed (Int64.of_int (2000 + n)))
      | (1 | 2), Some n -> Some (signed (Int64.of_int (1900 + n)))
      | 4, Some n when n = 0 || (n >= 1901 && n <= 2155) -> Some (signed (Int64.of_int n))
      | _ -> None)
  | Float -> Option.bind (real text) (fun f -> comparable (Int32.float_of_bits (Int32.bits_of_float f)))
  | Double -> Option.bind (real text) comparable
  | Decimal { precision; scale } ->
      let sign, body = if at text 0 '-' then (`Negative, String.sub text 1 (String.length text - 1)) else (`Positive, text) in
      let whole, fraction =
        Option.fold ~none:(body, "")
          ~some:(fun dot -> (String.sub body 0 dot, String.sub body (dot + 1) (String.length body - dot - 1)))
          (String.index_opt body '.')
      in
      if precision < scale || precision > 65 || not (numeral whole && (fraction = "" || numeral fraction)) then None
      else
        let kept = if String.length fraction > scale then String.sub fraction 0 scale else fraction ^ String.make (scale - String.length fraction) '0' in
        let rounding = if String.length fraction > scale && Char.compare fraction.[scale] '5' >= 0 then `Up else `Down in
        let digits =
          match rounding with
          | `Down -> whole ^ kept
          | `Up ->
              let bytes = Bytes.of_string (whole ^ kept) in
              let rec carry i =
                if i < 0 then "1" ^ Bytes.to_string bytes
                else
                  match Bytes.get bytes i with
                  | '9' ->
                      Bytes.set bytes i '0';
                      carry (i - 1)
                  | c ->
                      Bytes.set bytes i (Char.chr (Char.code c + 1));
                      Bytes.to_string bytes
              in
              carry (Bytes.length bytes - 1)
        in
        let integral = precision - scale in
        let whole = Base.String.lstrip (String.sub digits 0 (String.length digits - scale)) ~drop:(Char.equal '0')
        and kept = String.sub digits (String.length digits - scale) scale in
        if String.length whole > integral then None
        else
          let whole = String.make (integral - String.length whole) '0' ^ whole in
          let rec nines text pos acc = if pos >= String.length text then List.rev acc else nines text (pos + 9) (String.sub text pos 9 :: acc) in
          let lead = integral mod 9 and full = scale / 9 * 9 in
          let groups =
            List.filter
              (fun group -> group <> "")
              ((String.sub whole 0 lead :: nines (String.sub whole lead (integral - lead)) 0 [])
              @ nines (String.sub kept 0 full) 0 []
              @ [ String.sub kept full (scale - full) ])
          in
          let inverted = match sign with `Negative when not (String.for_all (Char.equal '0') digits) -> 0xFF | `Negative | `Positive -> 0 in
          let buffer = Buffer.create 32 in
          Buffer.add_char buffer (Char.chr precision);
          Buffer.add_char buffer (Char.chr scale);
          List.iter
            (fun group ->
              let value = int_of_string group in
              let rec put k =
                if k > 0 then (
                  Buffer.add_char buffer (Char.chr (((value lsr (8 * (k - 1))) land 0xFF) lxor inverted));
                  put (k - 1))
              in
              let width = match String.length group with 9 -> 4 | n -> (n + 1) / 2 in
              put width)
            groups;
          let bytes = Buffer.to_bytes buffer in
          Bytes.set_uint8 bytes 2 (Bytes.get_uint8 bytes 2 lxor 0x80);
          Some (Bytes.to_string bytes)
  | Char -> Some (Base.String.rstrip text ~drop:(Char.equal ' '))
  | Bytes -> Some text
  | Date -> Option.bind (calendar text) (fun date -> if String.length text = 10 then Some (packed date (0, 0, 0) 0) else None)
  | Datetime { fsp } | Timestamp { clock = Utc; fsp } ->
      let open Base.Option.Let_syntax in
      let%bind date = calendar text
      and hour = number text 11 2
      and minute = number text 14 2
      and second = number text 17 2
      and micro = fraction text 19 ~fsp in
      if at text 10 ' ' && at text 13 ':' && at text 16 ':' && hour <= 23 && minute <= 59 && second <= 59 then
        Some (packed date (hour, minute, second) micro)
      else None
  | Timestamp { clock = Server; _ } -> None
  | Time { fsp } ->
      let from = if at text 0 '-' then 1 else 0 in
      let open Base.Option.Let_syntax in
      let%bind colon = String.index_from_opt text from ':' in
      let%bind hours = number text from (colon - from) and minute = number text (colon + 1) 2 and second = number text (colon + 4) 2 in
      let%bind micro = fraction text (colon + 6) ~fsp in
      if at text (colon + 3) ':' && colon - from <= 3 && minute <= 59 && second <= 59 then
        let nanos = Int64.add (Int64.mul (Int64.of_int ((hours * 3600) + (minute * 60) + second)) 1_000_000_000L) (Int64.of_int (micro * 1000)) in
        let nanos = if from = 1 then Int64.neg nanos else nanos in
        Some (signed nanos)
      else None
  | Enum elements -> Option.map (fun i -> unsigned (Int64.of_int (i + 1))) (member elements text)
  | Set elements ->
      let members = if text = "" then [] else String.split_on_char ',' text in
      Option.map
        (fun bits -> unsigned (List.fold_left Int64.logor 0L bits))
        (Base.Option.all (List.map (fun name -> Option.map (fun i -> Int64.shift_left 1L i) (member elements name)) members))
  | Bit ->
      if String.length text <= 8 then Some (unsigned (String.fold_left (fun n c -> Int64.logor (Int64.shift_left n 8) (Int64.of_int (Char.code c))) 0L text))
      else None
  | Json -> if text = "null" then Some "\x04\x00" else None
  | Unknown -> None
