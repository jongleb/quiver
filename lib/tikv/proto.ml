type error =
  | Truncated
  | Overlong_varint
  | Unknown_wire of int
  | Field_zero
  | Unexpected of { field : int; wire : int }
[@@deriving sexp_of ~portable]

let varint buffer n =
  let rec go n =
    if Int64.equal (Int64.logand n (Int64.lognot 0x7FL)) 0L then Buffer.add_char buffer (Char.chr (Int64.to_int n))
    else (
      Buffer.add_char buffer (Char.chr (Int64.to_int (Int64.logand n 0x7FL) lor 0x80));
      go (Int64.shift_right_logical n 7))
  in
  go n

let uint buffer field n =
  varint buffer (Int64.of_int (field lsl 3));
  varint buffer n

let delimited buffer field length =
  varint buffer (Int64.of_int ((field lsl 3) lor 2));
  varint buffer (Int64.of_int length)

let varint_size n =
  let rec go n size = if Int64.equal (Int64.logand n (Int64.lognot 0x7FL)) 0L then size else go (Int64.shift_right_logical n 7) (size + 1) in
  go n 1

let uint_size field n = varint_size (Int64.of_int (field lsl 3)) + varint_size n
let delimited_size field length = varint_size (Int64.of_int ((field lsl 3) lor 2)) + varint_size (Int64.of_int length) + length

let put_varint bytes pos n =
  let rec go pos n =
    if n land lnot 0x7F = 0 then (
      Bytes.set bytes pos (Char.chr n);
      pos + 1)
    else (
      Bytes.set bytes pos (Char.chr (n land 0x7F lor 0x80));
      go (pos + 1) (n lsr 7))
  in
  go pos n

let put_delimited bytes pos field length = put_varint bytes (put_varint bytes pos ((field lsl 3) lor 2)) length

let bytes buffer field text =
  varint buffer (Int64.of_int ((field lsl 3) lor 2));
  varint buffer (Int64.of_int (String.length text));
  Buffer.add_string buffer text

type value =
  | Varint of int64
  | Fixed64 of int64
  | Fixed32 of int32
  | Delimited of { pos : int; len : int }

let varint_at text ~limit pos =
  let rec go pos shift acc =
    if pos >= limit then Error (`Proto Truncated)
    else
      let byte = Char.code text.[pos] in
      if shift = 63 && byte > 1 then Error (`Proto Overlong_varint)
      else
        let acc = Int64.logor acc (Int64.shift_left (Int64.of_int (byte land 0x7F)) shift) in
        if byte land 0x80 = 0 then Ok (acc, pos + 1) else go (pos + 1) (shift + 7) acc
  in
  go pos 0 0L

let fold text ~pos ~len ~init ~f =
  let limit = pos + len in
  let read_varint = varint_at text ~limit
  and fixed pos width =
    if limit - pos < width then Error (`Proto Truncated)
    else
      Ok
        ( Base.Fn.apply_n_times ~n:width
            (fun (acc, i) -> (Int64.logor acc (Int64.shift_left (Int64.of_int (Char.code text.[pos + i])) (8 * i)), i + 1))
            (0L, 0)
          |> fst,
          pos + width )
  in
  let rec go pos acc =
    if pos >= limit then Ok acc
    else
      match read_varint pos with
      | Error _ as error -> error
      | Ok (key, pos) -> (
          let field = Int64.to_int (Int64.shift_right_logical key 3) and wire = Int64.to_int key land 7 in
          let field_value =
            match wire with
            | 0 -> Result.map (fun (n, next) -> (Varint n, next)) (read_varint pos)
            | 1 -> Result.map (fun (n, next) -> (Fixed64 n, next)) (fixed pos 8)
            | 5 -> Result.map (fun (n, next) -> (Fixed32 (Int64.to_int32 n), next)) (fixed pos 4)
            | 2 ->
                Result.bind (read_varint pos) (fun (length, pos) ->
                    if Int64.compare length 0L < 0 || Int64.compare length (Int64.of_int (limit - pos)) > 0 then
                      Error (`Proto Truncated)
                    else
                      let len = Int64.to_int length in
                      Ok (Delimited { pos; len }, pos + len))
            | other -> Error (`Proto (Unknown_wire other))
          in
          match field_value with
          | Error _ as error -> error
          | Ok _ when field = 0 -> Error (`Proto Field_zero)
          | Ok (value, next) -> ( match f acc field value with Ok acc -> go next acc | Error _ as error -> error))
  in
  go pos init

type walked = Walked | Unusual

let walk text ~pos ~len ~(delimited @ local) ~(scalar @ local) =
  let limit = pos + len in
  let rec varint pos shift acc =
    if pos >= limit || shift > 49 then #(0, -1)
    else
      let byte = Char.code text.[pos] in
      let acc = acc lor ((byte land 0x7F) lsl shift) in
      if byte land 0x80 = 0 then #(acc, pos + 1) else varint (pos + 1) (shift + 7) acc
  in
  let rec go pos =
    if pos >= limit then Walked
    else
      let #(key, next) = varint pos 0 0 in
      let field = key lsr 3 in
      if next < 0 || field = 0 then Unusual
      else
        let after =
          match key land 7 with
          | 2 ->
              let #(length, start) = varint next 0 0 in
              if start < 0 || length > limit - start then -1
              else (match delimited field start length with Walked -> start + length | Unusual -> -1)
          | 0 ->
              let #(_, after) = varint next 0 0 in
              if after < 0 then -1 else (match scalar field with Walked -> after | Unusual -> -1)
          | 1 when limit - next >= 8 -> (match scalar field with Walked -> next + 8 | Unusual -> -1)
          | 5 when limit - next >= 4 -> (match scalar field with Walked -> next + 4 | Unusual -> -1)
          | _ -> -1
        in
        if after < 0 then Unusual else go after
  in
  go pos [@nontail]

let unexpected field (value : value) =
  let wire = match value with Varint _ -> 0 | Fixed64 _ -> 1 | Delimited _ -> 2 | Fixed32 _ -> 5 in
  Error (`Proto (Unexpected { field; wire }))

let number field (value : value) = match value with Varint n -> Ok n | other -> unexpected field other

let slice field (value : value) =
  match value with Delimited { pos; len } -> Ok (pos, len) | other -> unexpected field other

let contents text field (value : value) =
  match value with Delimited { pos; len } -> Ok (String.sub text pos len) | other -> unexpected field other

let numbers text field (value : value) =
  match value with
  | Varint n -> Ok [ n ]
  | Delimited { pos; len } ->
      let limit = pos + len in
      let rec go pos acc =
        if pos >= limit then Ok (List.rev acc) else Result.bind (varint_at text ~limit pos) (fun (n, next) -> go next (n :: acc))
      in
      go pos []
  | other -> unexpected field other
