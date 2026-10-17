open Base.Option.Let_syntax

let micros_per_second = 1_000_000
let micros_per_day = 86_400 * micros_per_second
let max_year = 9999

let divisors = [: micros_per_second; 100_000; 10_000; 1000; 100; 10; 1 :]

let fraction out (fsp : Ltype.Fsp.t) micros =
  let fsp = (fsp :> int) in
  if fsp > 0 then (
    Byte_buffer.add_char out '.';
    Digits.padded out ~width:fsp (micros / Iarray.get divisors fsp))

let clock out ~hours ~minutes ~seconds =
  if hours < 100 then Digits.clock out ~hours ~minutes ~seconds
  else (
    Digits.padded out ~width:2 hours;
    Byte_buffer.add_char out ':';
    Digits.two out minutes;
    Byte_buffer.add_char out ':';
    Digits.two out seconds)

let[@inline] clock_of_seconds out seconds =
  clock out ~hours:(seconds / 3600)
    ~minutes:(seconds / 60 mod 60)
    ~seconds:(seconds mod 60)

let day out date =
  Digits.padded out ~width:4 (Core.Date.year date);
  Byte_buffer.add_char out '-';
  Digits.two out (Core.Month.to_int (Core.Date.month date));
  Byte_buffer.add_char out '-';
  Digits.two out (Core.Date.day date)

let moment out value =
  let within = Moment.micros_of_day value in
  day out (Moment.date value);
  Byte_buffer.add_char out ' ';
  clock_of_seconds out (within / micros_per_second);
  fraction out Ltype.Fsp.microseconds (within mod micros_per_second)

let days_since_epoch date =
  Core.Date.Days.diff (Core.Date.Days.of_date date) Core.Date.Days.unix_epoch

let first_day = days_since_epoch (Core.Date.create_exn ~y:0 ~m:Jan ~d:1)
let last_day = days_since_epoch (Core.Date.create_exn ~y:max_year ~m:Dec ~d:31)

let days_of_date ~year ~month ~day =
  match year >= 0 && year <= max_year && month >= 1 && month <= 12 with
  | false -> Null
  | true -> (
      let month = Core.Month.of_int_exn month in
      match day >= 1 && day <= Core.Date.days_in_month ~year ~month with
      | true ->
          This (days_since_epoch (Core.Date.create_exn ~y:year ~m:month ~d:day))
      | false -> Null)

let[@inline] date out ~days =
  match days >= first_day && days <= last_day with
  | false -> Error `Unrepresentable_date
  | true ->
      let date =
        Core.Date.Days.to_date
          (Core.Date.Days.add_days Core.Date.Days.unix_epoch days)
      in
      Digits.padded out ~width:4 (Core.Date.year date);
      Byte_buffer.add_char out '-';
      Digits.two out (Core.Month.to_int (Core.Date.month date));
      Byte_buffer.add_char out '-';
      Digits.two out (Core.Date.day date);
      Ok ()

let[@inline] datetime out fsp ~micros =
  let micros = Int64_u.to_int_trunc micros in
  let days =
    (if micros >= 0 then micros else micros + 1 - micros_per_day)
    / micros_per_day
  in
  let within = micros - (days * micros_per_day) in
  match date out ~days with
  | Ok () ->
      Byte_buffer.add_char out ' ';
      clock_of_seconds out (within / micros_per_second);
      fraction out fsp (within mod micros_per_second);
      Ok ()
  | Error `Unrepresentable_date -> Error `Unrepresentable_date

let[@inline] time out fsp ~micros =
  let micros = Int64_u.to_int_trunc micros in
  let magnitude = abs micros in
  if micros < 0 then Byte_buffer.add_char out '-';
  clock_of_seconds out (magnitude / micros_per_second);
  fraction out fsp (magnitude mod micros_per_second)

let number text ~at ~length =
  match
    length > 0
    && at + length <= String.length text
    && Base.String.for_all (String.sub text at length) ~f:Base.Char.is_digit
  with
  | true -> int_of_string_opt (String.sub text at length)
  | false -> None

let separated text at separator =
  match at < String.length text && Char.equal text.[at] separator with
  | true -> Some ()
  | false -> None

let parse_days text =
  let%bind year = number text ~at:0 ~length:4
  and month = number text ~at:5 ~length:2
  and day = number text ~at:8 ~length:2
  and () = separated text 4 '-'
  and () = separated text 7 '-' in
  match days_of_date ~year ~month ~day with
  | This days -> Some days
  | Null -> None

let parse_clock text ~at =
  let%bind colon =
    match at < String.length text with
    | true -> String.index_from_opt text at ':'
    | false -> None
  in
  let fraction = colon + 6 in
  let%bind hours = number text ~at ~length:(colon - at)
  and minutes = number text ~at:(colon + 1) ~length:2
  and seconds = number text ~at:(colon + 4) ~length:2
  and () = separated text (colon + 3) ':'
  and micros =
    match String.length text - fraction with
    | 0 -> Some 0
    | length when length >= 2 && length <= 7 ->
        let%bind () = separated text fraction '.' in
        number
          (String.sub text (fraction + 1) (length - 1)
          ^ String.make (7 - length) '0')
          ~at:0 ~length:6
    | _ -> None
  in
  match
    (colon - at = 2 || (colon - at = 3 && hours >= 100))
    && minutes < 60 && seconds < 60
  with
  | true -> Some (hours, (((minutes * 60) + seconds) * micros_per_second) + micros)
  | false -> None

let parse_date text =
  match String.length text = 10 with
  | true ->
      Option.map
        (fun days ->
          Core.Date.Days.to_date
            (Core.Date.Days.add_days Core.Date.Days.unix_epoch days))
        (parse_days text)
  | false -> None

let parse_datetime text =
  let%bind days = parse_days text
  and () = separated text 10 ' '
  and () = separated text 13 ':'
  and hours, within = parse_clock text ~at:11 in
  match hours < 24 with
  | true ->
      Moment.of_micros
        ((days * micros_per_day) + (hours * 3600 * micros_per_second) + within)
  | false -> None

let parse_time text =
  let negative = separated text 0 '-' in
  let%bind hours, within =
    parse_clock text ~at:(Option.fold negative ~none:0 ~some:(fun () -> 1))
  in
  let micros = (hours * 3600 * micros_per_second) + within in
  match (hours <= 838, negative) with
  | true, None -> Some (Int64.of_int micros)
  | true, Some () when micros > 0 -> Some (Int64.of_int (-micros))
  | true, Some () | false, (None | Some ()) -> None

let day_bits = 5
let month_bits = 4
let micros_bits = 37
let mask bits = (1 lsl bits) - 1

let[@zero_alloc] pack_date ~year ~month ~day =
  (year lsl (month_bits + day_bits)) lor (month lsl day_bits) lor day

let max_packed_date =
  pack_date ~year:max_year ~month:(mask month_bits) ~day:(mask day_bits)

let[@zero_alloc] pack_datetime ~year ~month ~day ~micros =
  Int64_u.of_int ((pack_date ~year ~month ~day lsl micros_bits) lor micros)

let unchecked_date out packed =
  Digits.padded out ~width:4 (packed lsr (month_bits + day_bits));
  Byte_buffer.add_char out '-';
  Digits.two out ((packed lsr day_bits) land mask month_bits);
  Byte_buffer.add_char out '-';
  Digits.two out (packed land mask day_bits)

let unchecked_datetime out fsp packed =
  let packed = Int64_u.to_int_trunc packed in
  let micros = packed land mask micros_bits in
  unchecked_date out (packed lsr micros_bits);
  Byte_buffer.add_char out ' ';
  clock_of_seconds out (micros / micros_per_second);
  fraction out fsp (micros mod micros_per_second)
