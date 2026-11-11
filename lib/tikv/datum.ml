let uvarint text pos =
  let length = String.length text in
  let rec go pos shift n =
    if pos >= length || shift > 63 then None
    else
      let byte = Char.code text.[pos] in
      let n = Int64.logor n (Int64.shift_left (Int64.of_int (byte land 0x7F)) shift) in
      if byte < 0x80 then Some (n, pos + 1) else go (pos + 1) (shift + 7) n
  in
  go pos 0 0L

let signed (n, next) =
  let half = Int64.shift_right_logical n 1 in
  ((if Int64.logand n 1L = 0L then half else Int64.lognot half), next)

let put_uvarint buffer n =
  let rec go n =
    if Int64.unsigned_compare n 0x80L < 0 then Buffer.add_char buffer (Char.chr (Int64.to_int n))
    else (
      Buffer.add_char buffer (Char.chr (Int64.to_int (Int64.logand n 0x7FL) lor 0x80));
      go (Int64.shift_right_logical n 7))
  in
  go n

let zigzag n = Int64.logxor (Int64.shift_left n 1) (Int64.shift_right n 63)

let length text pos =
  let size = String.length text in
  let within n = if pos + n <= size then Some n else None in
  let dig2bytes = [| 0; 1; 1; 2; 2; 3; 3; 4; 4 |] in
  let after = pos + 1 in
  if pos >= size then None
  else
    match Char.code text.[pos] with
    | 0 -> Some 1
    | 1 ->
        let rec groups at =
          if at + 9 > size then None
          else if Char.code text.[at + 8] = 0xFF then groups (at + 9)
          else Some (at + 9 - pos)
        in
        groups after
    | 2 ->
        Option.bind (uvarint text after) (fun raw ->
            let n, next = signed raw in
            if Int64.compare n 0L < 0 || Int64.compare n (Int64.of_int size) > 0 then None else within (next - pos + Int64.to_int n))
    | 3 | 4 | 5 | 7 -> within 9
    | 6 ->
        if after + 2 > size then None
        else
          let precision = Char.code text.[after] and frac = Char.code text.[after + 1] in
          if frac > precision || precision > 65 then None
          else
            let bin digits = (digits / 9 * 4) + dig2bytes.(digits mod 9) in
            within (3 + bin (precision - frac) + bin frac)
    | 8 | 9 -> Option.map (fun (_, next) -> next - pos) (uvarint text after)
    | 10 -> (
        let value = after + 1 in
        if after >= size then None
        else
          match Char.code text.[after] with
          | 0x01 | 0x03 -> if value + 8 > size then None else within (2 + (Int32.to_int (String.get_int32_le text (value + 4)) land 0xFFFF_FFFF))
          | 0x0c -> Option.bind (uvarint text value) (fun (n, next) -> within (next - pos + Int64.to_int n))
          | 0x09 | 0x0a | 0x0b | 0x0e | 0x0f | 0x10 -> within 10
          | 0x04 -> within 3
          | 0x0d -> Option.bind (uvarint text (value + 1)) (fun (n, next) -> within (next - pos + Int64.to_int n))
          | 0x11 -> within 14
          | _ -> None)
    | 20 -> if after + 4 > size then None else within (5 + (4 * (Int32.to_int (String.get_int32_le text after) land 0x3FFF_FFFF)))
    | _ -> None

let id text pos =
  if pos < String.length text && Char.code text.[pos] = 8 then
    Option.bind (uvarint text (pos + 1)) (fun raw ->
        let n, next = signed raw in
        if Int64.compare n 0L >= 0 && Int64.compare n 0x7FFF_FFFFL <= 0 then Some (Int64.to_int n, next) else None)
  else None

let of_id id =
  let buffer = Buffer.create 6 in
  Buffer.add_char buffer '\x08';
  put_uvarint buffer (zigzag (Int64.of_int id));
  Buffer.contents buffer

let of_cell (kind : Info.kind) cell =
  let little ~extend =
    let width = String.length cell in
    if width <> 1 && width <> 2 && width <> 4 && width <> 8 then None
    else
      let bytes = Bytes.make 8 '\x00' in
      Bytes.blit_string cell 0 bytes 0 width;
      let n = Bytes.get_int64_le bytes 0 in
      let shift = 64 - (8 * width) in
      Some (match extend with `Sign -> Int64.shift_right (Int64.shift_left n shift) shift | `Zero -> n)
  in
  let flagged flag body =
    let buffer = Buffer.create (String.length cell + 11) in
    Buffer.add_char buffer flag;
    let () =
      match body with
      | `Varint n -> put_uvarint buffer (zigzag n)
      | `Uvarint n -> put_uvarint buffer n
      | `Raw raw -> Buffer.add_string buffer raw
      | `Compact raw ->
          put_uvarint buffer (zigzag (Int64.of_int (String.length raw)));
          Buffer.add_string buffer raw
    in
    Buffer.contents buffer
  in
  match kind with
  | Integer Signed | Year | Time _ -> Option.map (fun n -> flagged '\x08' (`Varint n)) (little ~extend:`Sign)
  | Integer Unsigned | Date | Datetime _ | Timestamp _ | Enum _ | Set _ | Bit ->
      Option.map (fun n -> flagged '\x09' (`Uvarint n)) (little ~extend:`Zero)
  | Float | Double -> if String.length cell = 8 then Some (flagged '\x05' (`Raw cell)) else None
  | Decimal _ -> Some (flagged '\x06' (`Raw cell))
  | Char | Bytes -> Some (flagged '\x02' (`Compact cell))
  | Json -> Some (flagged '\x0a' (`Raw cell))
  | Unknown -> None
