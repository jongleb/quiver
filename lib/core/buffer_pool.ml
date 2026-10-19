type t = { free : Byte_buffer.t Free_list.t; capacity : int }

let create ?(capacity = 1 lsl 20) () = { free = Free_list.create (); capacity }

let take t =
  match Free_list.take t.free with
  | This buffer ->
      Byte_buffer.clear (borrow_ buffer);
      buffer
  | Null -> Byte_buffer.create ~cap:t.capacity ()

let give t buffer = Free_list.put t.free buffer
