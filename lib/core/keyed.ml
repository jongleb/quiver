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

type permutation = string

let permutation key ~label =
  Quiver_openssl.Sha.hmac_sha256 ~key ~prefix:(Int64.of_int (9 lsl 32)) ("permute/v1/" ^ label)

let permute_integer key ~low ~high value =
  Int64.add low (Quiver_openssl.Ff1.below ~key ~limit:(Int64.sub high low) (Int64.sub value low))

type case = Lower | Upper
type place = Fixed | Digit of int | Latin of { value : int; case : case } | Cyrillic of { value : int; case : case }

let permute_text key text =
  let length = String.length text in
  let rec trailing pos = if pos > 0 && Char.equal text.[pos - 1] ' ' then trailing (pos - 1) else pos in
  let ends = trailing length in
  let cyrillic code ~first ~yo =
    if code = yo then 6
    else
      let i = code - first in
      if i <= 5 then i else i + 1
  in
  let place code =
    if code >= Char.code '0' && code <= Char.code '9' then Digit (code - Char.code '0')
    else if code >= Char.code 'a' && code <= Char.code 'z' then Latin { value = code - Char.code 'a'; case = Lower }
    else if code >= Char.code 'A' && code <= Char.code 'Z' then Latin { value = code - Char.code 'A'; case = Upper }
    else if (code >= 0x430 && code <= 0x44F) || code = 0x451 then
      Cyrillic { value = cyrillic code ~first:0x430 ~yo:0x451; case = Lower }
    else if (code >= 0x410 && code <= 0x42F) || code = 0x401 then
      Cyrillic { value = cyrillic code ~first:0x410 ~yo:0x401; case = Upper }
    else Fixed
  in
  let rec read pos acc =
    if pos >= length then List.rev acc
    else
      let width, code = Utf8.decode text pos in
      read (pos + width) ((pos, width, place code) :: acc)
  in
  let places = read 0 [] in
  let pattern = Buffer.create (length + 8) in
  Buffer.add_string pattern "text\000";
  let digits = Buffer.create length and radixes = Buffer.create length in
  List.iter
    (fun (pos, width, place) ->
      let changing tag radix value =
        Buffer.add_char pattern '\000';
        Buffer.add_char pattern tag;
        Buffer.add_char radixes (Char.chr radix);
        Buffer.add_char digits (Char.chr value)
      in
      match place with
      | Digit value -> changing 'd' 10 value
      | Latin { value; _ } -> changing 'l' 26 value
      | Cyrillic { value; _ } -> changing 'c' 33 value
      | Fixed when pos >= ends -> ()
      | Fixed when width = 1 && Char.equal text.[pos] '\000' -> Buffer.add_string pattern "\000\000"
      | Fixed -> Buffer.add_string pattern (String.sub text pos width))
    places;
  let tweak = Quiver_openssl.Sha.sha256 (Buffer.contents pattern) in
  Option.map
    (fun permuted ->
      let out = Buffer.create length in
      let cyrillic value ~first ~yo = if value = 6 then yo else if value < 6 then first + value else first + value - 1 in
      let rec write k = function
        | [] -> ()
        | (pos, width, place) :: rest -> (
            let next () = Char.code permuted.[k] in
            match place with
            | Fixed ->
                Buffer.add_string out (String.sub text pos width);
                write k rest
            | Digit _ ->
                Buffer.add_char out (Char.chr (Char.code '0' + next ()));
                write (k + 1) rest
            | Latin { case; _ } ->
                let first = match case with Upper -> 'A' | Lower -> 'a' in
                Buffer.add_char out (Char.chr (next () + Char.code first));
                write (k + 1) rest
            | Cyrillic { case; _ } ->
                let code =
                  match case with
                  | Upper -> cyrillic (next ()) ~first:0x410 ~yo:0x401
                  | Lower -> cyrillic (next ()) ~first:0x430 ~yo:0x451
                in
                Buffer.add_utf_8_uchar out (Uchar.of_int code);
                write (k + 1) rest)
      in
      write 0 places;
      Buffer.contents out)
    (Quiver_openssl.Ff1.digits ~key ~tweak ~radixes:(Buffer.contents radixes) (Buffer.contents digits))

let permute_bytes key value =
  let length = String.length value in
  if length > 1024 then None
  else Quiver_openssl.Ff1.digits ~key ~tweak:"bytes" ~radixes:(String.make length '\000') value
