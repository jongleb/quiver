type error = Short | Marker of int | Padding [@@deriving sexp_of ~portable]

let encode_bytes text =
  let length = String.length text in
  let buffer = Buffer.create (((length / 8) + 1) * 9) in
  let rec go pos =
    let remain = length - pos in
    if remain >= 8 then (
      Buffer.add_string buffer (String.sub text pos 8);
      Buffer.add_char buffer '\xff';
      go (pos + 8))
    else (
      Buffer.add_string buffer (String.sub text pos remain);
      Buffer.add_string buffer (String.make (8 - remain) '\x00');
      Buffer.add_char buffer (Char.chr (0xff - (8 - remain))))
  in
  go 0;
  Buffer.contents buffer

let decode_bytes text =
  let buffer = Buffer.create (String.length text) in
  let rec go pos =
    if String.length text - pos < 9 then Error (`Codec Short)
    else
      let padding = 0xff - Char.code text.[pos + 8] in
      if padding > 8 then Error (`Codec (Marker (Char.code text.[pos + 8])))
      else (
        Buffer.add_string buffer (String.sub text pos (8 - padding));
        if padding = 0 then go (pos + 9)
        else if String.exists (fun c -> c <> '\x00') (String.sub text (pos + 8 - padding) padding) then Error (`Codec Padding)
        else Ok (Buffer.contents buffer))
  in
  go 0

let prefix table =
  let id = Int64.logxor table Int64.min_int in
  "t" ^ String.init 8 (fun i -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical id (8 * (7 - i))) 0xFFL)))

let records table = (prefix table ^ "_r", prefix table ^ "_s")
let table id = (prefix id, prefix (Int64.add id 1L))
let retabled ~into key =
  let bytes = Bytes.of_string key in
  Bytes.set_int64_be bytes 1 (Int64.logxor into Int64.min_int);
  Bytes.unsafe_to_string bytes

let kind_end key =
  let length = String.length key in
  if length >= 19 && String.sub key 9 2 = "_i" then
    let rec increment text i =
      if i < 0 then None
      else if text.[i] = '\xff' then increment (String.sub text 0 i) (i - 1)
      else Some (String.sub text 0 i ^ String.make 1 (Char.chr (Char.code text.[i] + 1)))
    in
    increment (String.sub key 0 19) 18
  else if length >= 11 && String.sub key 9 2 = "_r" then Some (String.sub key 0 9 ^ "_s")
  else None

let hashed key = "m" ^ encode_bytes key ^ "\x00\x00\x00\x00\x00\x00\x00h"
let databases =
  let first = hashed "DBs" in
  (first, String.sub first 0 (String.length first - 1) ^ "i")
let described ~database ~table = hashed ("DB:" ^ Int64.to_string database) ^ encode_bytes ("Table:" ^ Int64.to_string table)
