type t = Byte_buffer.t Free_list.t

let create () = Free_list.create ()

let take t =
  match Free_list.take t with
  | This buffer ->
      Byte_buffer.clear (borrow_ buffer);
      buffer
  | Null -> Byte_buffer.create ~cap:(1 lsl 20) ()

let give t buffer = Free_list.put t buffer
