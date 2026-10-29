let decode text pos =
  let length = String.length text in
  let byte i = Char.code text.[i] in
  let continued i = i < length && byte i land 0xC0 = 0x80 in
  let lead = byte pos in
  let tail i = byte (pos + i) land 0x3F in
  if lead < 0x80 then (1, lead)
  else if lead land 0xE0 = 0xC0 && continued (pos + 1) then (2, ((lead land 0x1F) lsl 6) lor tail 1)
  else if lead land 0xF0 = 0xE0 && continued (pos + 1) && continued (pos + 2) then
    (3, ((lead land 0x0F) lsl 12) lor (tail 1 lsl 6) lor tail 2)
  else if
    lead land 0xF8 = 0xF0 && continued (pos + 1) && continued (pos + 2) && continued (pos + 3)
  then (4, ((lead land 0x07) lsl 18) lor (tail 1 lsl 12) lor (tail 2 lsl 6) lor tail 3)
  else (1, -1)

let points text =
  let rec count pos n = if pos >= String.length text then n else count (pos + fst (decode text pos)) (n + 1) in
  count 0 0

let offset text point =
  let rec walk pos n =
    if n >= point || pos >= String.length text then Int.min pos (String.length text)
    else walk (pos + fst (decode text pos)) (n + 1)
  in
  walk 0 0
