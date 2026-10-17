let pairs =
  String.init 200 (fun i ->
      Char.chr (48 + if i land 1 = 0 then i / 20 else i / 2 mod 10))

let place (out : Byte_buffer.t) ~width negative =
  Byte_buffer.ensure_free out width;
  let rec go bytes start value k =
    if k >= 2 then (
      let next = value / 100 in
      let pair = 2 * ((next * 100) - value) in
      Bytes.unsafe_set bytes (start + k - 2) (String.unsafe_get pairs pair);
      Bytes.unsafe_set bytes
        (start + k - 1)
        (String.unsafe_get pairs (pair + 1));
      go bytes start next (k - 2))
    else if k = 1 then
      Bytes.unsafe_set bytes start (Char.unsafe_chr (48 - value))
  in
  go out.bs out.len negative width;
  out.len <- out.len + width

let length negative =
  if negative > -100_000_000 then
    if negative > -10_000 then
      if negative > -100 then if negative > -10 then 1 else 2
      else if negative > -1000 then 3
      else 4
    else if negative > -1_000_000 then if negative > -100_000 then 5 else 6
    else if negative > -10_000_000 then 7
    else 8
  else
    let rec go negative bound count =
      if count = 19 || negative > bound then count
      else go negative (bound * 10) (count + 1)
    in
    go negative (-1_000_000_000) 9

let[@inline] two (out : Byte_buffer.t) n =
  Byte_buffer.ensure_free out 2;
  Bytes.unsafe_set out.bs out.len pairs.[2 * n];
  Bytes.unsafe_set out.bs (out.len + 1) pairs.[(2 * n) + 1];
  out.len <- out.len + 2

let clock (out : Byte_buffer.t) ~hours ~minutes ~seconds =
  Byte_buffer.ensure_free out 8;
  let bytes = out.bs and at = out.len in
  Bytes.unsafe_set bytes at pairs.[2 * hours];
  Bytes.unsafe_set bytes (at + 1) pairs.[(2 * hours) + 1];
  Bytes.unsafe_set bytes (at + 2) ':';
  Bytes.unsafe_set bytes (at + 3) pairs.[2 * minutes];
  Bytes.unsafe_set bytes (at + 4) pairs.[(2 * minutes) + 1];
  Bytes.unsafe_set bytes (at + 5) ':';
  Bytes.unsafe_set bytes (at + 6) pairs.[2 * seconds];
  Bytes.unsafe_set bytes (at + 7) pairs.[(2 * seconds) + 1];
  out.len <- at + 8

let int out n =
  if n < 0 then (
    Byte_buffer.add_char out '-';
    place out ~width:(length n) n)
  else place out ~width:(length (-n)) (-n)

let padded out ~width n = place out ~width:(Int.max width (length (-n))) (-n)

let[@inline] int64 out n =
  let small = Int64_u.to_int_trunc n in
  if Int64_u.equal (Int64_u.of_int small) n then int out small
  else
    let head = Int64_u.to_int_trunc Int64_u.O.(n / #10L)
    and tail = Int64_u.to_int_trunc (Int64_u.rem n #10L) in
    if head < 0 then (
      Byte_buffer.add_char out '-';
      place out ~width:(length head) head;
      Byte_buffer.add_char out (Char.unsafe_chr (48 - tail)))
    else (
      place out ~width:(length (-head)) (-head);
      Byte_buffer.add_char out (Char.unsafe_chr (48 + tail)))

let[@inline] uint64 out n =
  if Int64_u.is_non_negative n then int64 out n
  else
    let quotient = Int64_u.O.(Int64_u.shift_right_logical n 1 / #5L) in
    int out (Int64_u.to_int_trunc quotient);
    Byte_buffer.add_char out
      (Char.unsafe_chr
         (48 + Int64_u.to_int_trunc Int64_u.O.(n - (quotient * #10L))))
