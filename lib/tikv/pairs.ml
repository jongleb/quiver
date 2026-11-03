type t = { mutable data : Bytes.t; mutable used : int; mutable ends : int array; mutable count : int }
type span = { pairs : t; first : int; past : int }

let create () = { data = Bytes.create 4096; used = 0; ends = Array.make 512 0; count = 0 }

let add t key value =
  let need = t.used + String.length key + String.length value in
  let () =
    if need > Bytes.length t.data then (
      let data = Bytes.create (Int.max need (2 * Bytes.length t.data)) in
      Bytes.blit t.data 0 data 0 t.used;
      t.data <- data)
  in
  let () =
    if (2 * t.count) + 2 > Array.length t.ends then (
      let ends = Array.make (2 * Array.length t.ends) 0 in
      Array.blit t.ends 0 ends 0 (2 * t.count);
      t.ends <- ends)
  in
  Bytes.blit_string key 0 t.data t.used (String.length key);
  t.ends.(2 * t.count) <- t.used + String.length key;
  Bytes.blit_string value 0 t.data (t.used + String.length key) (String.length value);
  t.ends.((2 * t.count) + 1) <- need;
  t.used <- need;
  t.count <- t.count + 1

let clear t =
  t.used <- 0;
  t.count <- 0

let all t = { pairs = t; first = 0; past = t.count }

let of_list pairs =
  let t = create () in
  List.iter (fun (pair : Kv.pair) -> add t pair.key pair.value) pairs;
  all t

let length span = span.past - span.first
let start t i = if i = 0 then 0 else t.ends.((2 * i) - 1)
let key_length span i = span.pairs.ends.(2 * (span.first + i)) - start span.pairs (span.first + i)
let value_length span i = span.pairs.ends.((2 * (span.first + i)) + 1) - span.pairs.ends.(2 * (span.first + i))
let key span i = Bytes.sub_string span.pairs.data (start span.pairs (span.first + i)) (key_length span i)
let blit_key span i bytes pos = Bytes.blit span.pairs.data (start span.pairs (span.first + i)) bytes pos (key_length span i)
let blit_value span i bytes pos = Bytes.blit span.pairs.data span.pairs.ends.(2 * (span.first + i)) bytes pos (value_length span i)

let to_list span =
  List.init (length span) (fun i ->
      ({ key = key span i; value = Bytes.sub_string span.pairs.data span.pairs.ends.(2 * (span.first + i)) (value_length span i) } : Kv.pair))

let split span (bound : Kv.bound) =
  match bound with
  | Endless -> (span, { span with first = span.past })
  | Before stop ->
  let order i =
    let pos = start span.pairs (span.first + i) and len = key_length span i in
    let n = Int.min len (String.length stop) in
    let rec go j =
      if j = n then Int.compare len (String.length stop)
      else match Char.compare (Bytes.get span.pairs.data (pos + j)) stop.[j] with 0 -> go (j + 1) | order -> order
    in
    go 0
  in
  let rec search low high =
    if low >= high then low
    else
      let middle = (low + high) / 2 in
      if order middle < 0 then search (middle + 1) high else search low middle
  in
  let at = span.first + search 0 (length span) in
  ({ span with past = at }, { span with first = at })

let cut span ~bytes =
  let size i = key_length span i + value_length span i
  and run from past = { span with first = span.first + from; past = span.first + past } in
  let rec go acc from i taken =
    if i = length span then
      let acc = if i > from then run from i :: acc else acc in
      List.rev acc
    else if i > from && taken + size i > bytes then go (run from i :: acc) i (i + 1) (size i)
    else go acc from (i + 1) (taken + size i)
  in
  go [] 0 0 0
