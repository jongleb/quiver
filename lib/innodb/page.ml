let size = 16384
let null = 0xFFFF_FFFF
let infimum = 99
let supremum = 112
let heap_start = 120
let empty_free = size - heap_start - 8 - 4
let reserve = size / 16

type t = {
  bytes : Bytes.t;
  level : int;
  mutable top : int;
  mutable count : int;
  mutable free : int;
  mutable last : int;
}

type room = Fits | Full
type status = Ordinary | Node_pointer
type flag = Plain | Leftmost

let create ~level ~index =
  let bytes = Bytes.make size '\000' in
  Bytes.blit_string "\x01\x00\x02\x00\x0dinfimum\x00\x01\x00\x0b\x00\x00supremum" 0 bytes 94 26;
  Bytes.set_uint16_be bytes 64 level;
  Bytes.set_int64_be bytes 66 (Int64.of_int index);
  { bytes; level; top = heap_start; count = 0; free = empty_free; last = infimum }

let level t = t.level
let count t = t.count
let bytes t = t.bytes
let slots n = ((2 * n) + 3) / 4

let room t record =
  let need = record + slots (t.count + 1) - slots t.count in
  if need > t.free || (t.count >= 2 && t.free - need < reserve) then Full else Fits

let set_next bytes origin next = Bytes.set_uint16_be bytes (origin - 2) ((next - origin) land 0xFFFF)

let append t ~extra ~data status flag =
  let origin = t.top + extra + 5 in
  let info = match flag with Plain -> 0 | Leftmost -> 0x10 in
  let code = match status with Ordinary -> 0 | Node_pointer -> 1 in
  Bytes.set_uint8 t.bytes (origin - 5) info;
  Bytes.set_uint16_be t.bytes (origin - 4) (((2 + t.count) lsl 3) lor code);
  set_next t.bytes t.last origin;
  set_next t.bytes origin supremum;
  t.free <- t.free - (extra + 5 + data) - (slots (t.count + 1) - slots t.count);
  t.top <- origin + data;
  t.count <- t.count + 1;
  t.last <- origin;
  origin

let place t image ~pos ~length ~origin =
  Bytes.blit_string image pos t.bytes t.top length;
  append t ~extra:(origin - 5) ~data:(length - origin) Ordinary Plain

let first t = (infimum + Bytes.get_uint16_be t.bytes (infimum - 2)) land 0xFFFF

let close t =
  let bytes = t.bytes in
  let slot n origin = Bytes.set_uint16_be bytes (size - 8 - (2 * (n + 1))) origin in
  let owned origin n = Bytes.set_uint8 bytes (origin - 5) (Bytes.get_uint8 bytes (origin - 5) land 0xF0 lor n) in
  let next origin = (origin + Bytes.get_uint16_be bytes (origin - 2)) land 0xFFFF in
  slot 0 infimum;
  let rec assign origin left slots =
    if left >= 8 then
      let fourth = next (next (next (next origin))) in
      owned fourth 4;
      slot slots fourth;
      assign fourth (left - 4) (slots + 1)
    else (
      slot slots supremum;
      owned supremum (left + 1);
      slots + 1)
  in
  let n_slots = assign infimum t.count 1 in
  Bytes.set_uint16_be bytes 38 n_slots;
  Bytes.set_uint16_be bytes 40 t.top;
  Bytes.set_uint16_be bytes 42 (0x8000 lor (2 + t.count));
  let last_insert, direction = if t.count > 0 then (t.last, 2) else (0, 5) in
  Bytes.set_uint16_be bytes 48 last_insert;
  Bytes.set_uint16_be bytes 50 direction;
  Bytes.set_uint16_be bytes 54 t.count

let checksum bytes ~pos =
  Crc32c.digest bytes ~pos:(pos + 4) ~len:22
  lxor Crc32c.digest bytes ~pos:(pos + 38) ~len:(size - 46)

type kind = Index | Lob_first | Lob_data | Lob_index

let seal bytes ~kind ~number ~prev ~next ~space =
  Bytes.set_int32_be bytes 4 (Int32.of_int number);
  Bytes.set_int32_be bytes 8 (Int32.of_int prev);
  Bytes.set_int32_be bytes 12 (Int32.of_int next);
  let code = match kind with Index -> 17855 | Lob_index -> 22 | Lob_data -> 23 | Lob_first -> 24 in
  Bytes.set_uint16_be bytes 24 code;
  Bytes.set_int32_be bytes 34 (Int32.of_int space);
  let sum = Int32.of_int (checksum bytes ~pos:0) in
  Bytes.set_int32_be bytes 0 sum;
  Bytes.set_int32_be bytes (size - 8) sum
