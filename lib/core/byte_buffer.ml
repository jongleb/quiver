type t = { mutable bs : Bytes.t @@ aliased; mutable len : int }

let create ?(cap = 0) () = { bs = Bytes.create cap; len = 0 }
let[@inline] length t = t.len
let[@inline] clear t = t.len <- 0

(* Growing allocates, but buffers come from pools and keep their size, so a
   dump stops growing them after its first batches: the checker is told to
   trust this one function and checks every other path. *)
let[@inline never] [@zero_alloc assume] grow t wanted =
  let capacity = Bytes.length t.bs in
  let bytes = Bytes.create (max wanted (capacity + (capacity lsl 1) + 5)) in
  Bytes.blit t.bs 0 bytes 0 t.len;
  t.bs <- bytes

let[@inline] ensure_cap t wanted =
  if wanted > Bytes.length t.bs then grow t wanted

let[@inline] ensure_free t count =
  if count > Bytes.length t.bs - t.len then grow t (t.len + count)

let[@inline] add_char t c =
  if t.len = Bytes.length t.bs then grow t (t.len + 1);
  Bytes.unsafe_set t.bs t.len c;
  t.len <- t.len + 1

let append_subbytes t bytes off len =
  ensure_free t len;
  Bytes.blit bytes off t.bs t.len len;
  t.len <- t.len + len

let[@inline] add_int32_le t n =
  ensure_free t 4;
  Bytes.set_int32_le t.bs t.len (Int32.of_int n);
  t.len <- t.len + 4

let[@inline] append_string t text =
  append_subbytes t (Bytes.unsafe_of_string text) 0 (String.length text)

let[@inline] append_buffer t source = append_subbytes t source.bs 0 source.len

let[@inline] contents t = Bytes.sub_string t.bs 0 t.len
