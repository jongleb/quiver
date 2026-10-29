type t = string

let of_secret secret = Quiver_openssl.Sha.sha256 ("quiver mask\000" ^ secret)

let stream key ~tag text length =
  let rec blocks counter acc have =
    if have >= length then String.concat "" (List.rev acc)
    else
      let block =
        Quiver_openssl.Sha.hmac_sha256 ~key ~prefix:(Int64.of_int ((tag lsl 32) lor counter)) text
      in
      blocks (counter + 1) (block :: acc) (have + String.length block)
  in
  blocks 0 [] 0

let number key ~tag text = String.get_int64_be (stream key ~tag text 8) 0

let shape key text =
  let length = String.length text in
  let digest = stream key ~tag:1 text length in
  let out = Buffer.create length in
  let rec go pos k =
    if pos < length then (
      let random = Char.code digest.[k] in
      let width, code = Utf8.decode text pos in
      let within first count = Uchar.of_int (first + (random mod count)) in
      let replaced =
        if code >= Char.code '0' && code <= Char.code '9' then within (Char.code '0') 10
        else if code >= Char.code 'A' && code <= Char.code 'Z' then within (Char.code 'A') 26
        else if code >= Char.code 'a' && code <= Char.code 'z' then within (Char.code 'a') 26
        else if code >= 0 && code < 0x80 then Uchar.of_int code
        else if (code >= 0x410 && code <= 0x42F) || code = 0x401 then within 0x410 32
        else if (code >= 0x430 && code <= 0x44F) || code = 0x451 then within 0x430 32
        else within (Char.code 'a') 26
      in
      Buffer.add_utf_8_uchar out replaced;
      go (pos + width) (k + 1))
  in
  go 0 0;
  Buffer.contents out

let email key text =
  Option.fold (String.rindex_opt text '@') ~none:(shape key text) ~some:(fun at ->
      shape key (String.sub text 0 at) ^ String.sub text at (String.length text - at))

let hash_text key text =
  let n = Utf8.points text in
  let digest = stream key ~tag:2 text ((n + 1) / 2) in
  let hex = "0123456789abcdef" in
  String.init n (fun i ->
      let byte = Char.code digest.[i / 2] in
      let nibble = if i mod 2 = 0 then byte lsr 4 else byte land 15 in
      hex.[nibble])

let hash_bytes key text =
  let n = String.length text in
  String.sub (stream key ~tag:3 text n) 0 n

let hash_int64 key value = number key ~tag:4 (Int64.to_string value)

let within key text ~low ~high =
  let drawn = number key ~tag:5 text in
  let span = Int64.succ (Int64.sub high low) in
  if Int64.compare low high > 0 || Int64.compare span 0L <= 0 then drawn
  else Int64.add low (Int64.unsigned_rem drawn span)

let choice key text n = Int64.to_int (Int64.unsigned_rem (number key ~tag:6 text) (Int64.of_int (Int.max 1 n)))

let digits key text n =
  let n = Int.max 0 n in
  let digest = stream key ~tag:7 text n in
  String.init n (fun i -> Char.chr (Char.code '0' + (Char.code digest.[i] mod 10)))

let hex key text n =
  let n = Int.max 0 n in
  let digest = stream key ~tag:8 text ((n + 1) / 2) in
  let digits = "0123456789abcdef" in
  String.init n (fun i ->
      let byte = Char.code digest.[i / 2] in
      let nibble = if i mod 2 = 0 then byte lsr 4 else byte land 15 in
      digits.[nibble])
