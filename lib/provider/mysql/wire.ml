open Base.Result.Let_syntax

type error =
  | Closed
  | Out_of_order of { expected : int; got : int }
  | Malformed
[@@deriving sexp_of ~portable]

module Cursor = struct
  type t = {
    mutable buffer : Base_bigstring.t;
    mutable pos : int;
    mutable limit : int;
    mutable sequence : int;
  }

  type reader = t

  exception Short

  let create () =
    { buffer = Base_bigstring.create 0; pos = 0; limit = 0; sequence = 0 }

  let sequence t = t.sequence

  let[@inline] point t buffer ~off ~len ~sequence =
    t.buffer <- buffer;
    t.pos <- off;
    t.limit <- off + len;
    t.sequence <- sequence

  let decode t f =
    match f t with
    | result -> result
    | exception Short -> Error (`Wire Malformed)

  let[@zero_alloc] remaining t = t.limit - t.pos

  let[@zero_alloc] peek t =
    if t.pos < t.limit then Base_bigstring.unsafe_get_uint8 t.buffer ~pos:t.pos
    else raise_notrace Short

  let[@inline] [@zero_alloc] take t count =
    if count < 0 || t.pos + count > t.limit then raise_notrace Short;
    let pos = t.pos in
    t.pos <- pos + count;
    pos

  let[@zero_alloc] buffer t = t.buffer
  let[@zero_alloc] skip reader count = ignore (take reader count)

  let[@inline] [@zero_alloc] uint8 t =
    Base_bigstring.unsafe_get_uint8 t.buffer ~pos:(take t 1)

  let[@inline] [@zero_alloc] uint16 t =
    Base_bigstring.unsafe_get_uint16_le t.buffer ~pos:(take t 2)

  let[@inline] [@zero_alloc] uint24 t =
    let pos = take t 3 in
    Base_bigstring.unsafe_get_uint16_le t.buffer ~pos
    lor (Base_bigstring.unsafe_get_uint8 t.buffer ~pos:(pos + 2) lsl 16)

  let[@inline] [@zero_alloc] uint32 t =
    Base_bigstring.unsafe_get_uint32_le t.buffer ~pos:(take t 4)

  let[@inline] [@zero_alloc] int64 t =
    Int64_u.of_int64
      (Base_bigstring.unsafe_get_int64_t_le t.buffer ~pos:(take t 8))

  let[@zero_alloc] lenenc_int reader =
    match uint8 reader with
    | small when small < 0xFB -> small
    | 0xFC -> uint16 reader
    | 0xFD -> uint24 reader
    | 0xFE ->
        let low = uint32 reader in
        let high = uint32 reader in
        if high > max_int lsr 32 then raise_notrace Short
        else (high lsl 32) lor low
    | _ -> raise_notrace Short

  let string t count =
    Base_bigstring.To_string.sub t.buffer ~pos:(take t count) ~len:count

  let lenenc_string reader = string reader (lenenc_int reader)

  let lenenc_text t =
    if t.pos < t.limit && Base_bigstring.unsafe_get t.buffer t.pos = '\xfb' then (
      t.pos <- t.pos + 1;
      None)
    else Some (lenenc_string t)

  let null_terminated t =
    match
      Base_bigstring.unsafe_find t.buffer '\000' ~pos:t.pos ~len:(t.limit - t.pos)
    with
    | stop when stop < 0 -> raise_notrace Short
    | stop ->
        let text = string t (stop - t.pos) in
        skip t 1;
        text

  let rest reader = string reader (remaining reader)
end

type t = {
  read : Base_bigstring.t -> off:int -> len:int -> int;
  mutable buffer : Base_bigstring.t;
  mutable start : int;
  mutable stop : int;
  mutable sequence : int;
  mutable joined : Base_bigstring.t;
      (** Payload of a packet split into several frames. *)
}

type marker =
  | Ok_packet
  | Auth_more_data
  | Local_infile
  | Eof_packet
  | Err_packet
  | Data

type command = Query | Prepare | Execute | Close_statement

let ok_byte = 0x00
let eof_byte = 0xFE
let err_byte = 0xFF

let marker = function
  | 0x00 -> Ok_packet
  | 0x01 -> Auth_more_data
  | 0xFB -> Local_infile
  | 0xFE -> Eof_packet
  | 0xFF -> Err_packet
  | _ -> Data

let command = function
  | Query -> "\x03"
  | Prepare -> "\x16"
  | Execute -> "\x17"
  | Close_statement -> "\x19"

let frame_limit = 0xFFFFFF

let create read =
  {
    read;
    buffer = Base_bigstring.create (1 lsl 20);
    start = 0;
    stop = 0;
    sequence = 0;
    joined = Base_bigstring.create 0;
  }

let restart t = t.sequence <- 0

let frame t payload =
  let out = Buffer.create (String.length payload + 8) in
  let rec add offset =
    let length = Int.min frame_limit (String.length payload - offset) in
    Buffer.add_uint16_le out (length land 0xFFFF);
    Buffer.add_uint8 out (length lsr 16);
    Buffer.add_uint8 out t.sequence;
    t.sequence <- (t.sequence + 1) land 0xFF;
    Buffer.add_substring out payload offset length;
    if length = frame_limit then add (offset + length)
  in
  add 0;
  Buffer.contents out

(* Reading blocks on the socket and may grow the buffer: it runs once per
   buffer of rows, not per row, so the checker trusts it. *)
let[@inline never] [@zero_alloc assume] receive t needed =
  if t.start + needed > Base_bigstring.length t.buffer then (
    let kept = t.stop - t.start in
    let target =
      if needed > Base_bigstring.length t.buffer then
        Base_bigstring.create
          (Int.max needed (2 * Base_bigstring.length t.buffer))
      else t.buffer
    in
    Base_bigstring.blit ~src:t.buffer ~src_pos:t.start ~dst:target ~dst_pos:0
      ~len:kept;
    t.buffer <- target;
    t.start <- 0;
    t.stop <- kept);
  let rec loop () =
    if t.stop - t.start >= needed then Ok ()
    else
      match
        t.read t.buffer ~off:t.stop
          ~len:(Base_bigstring.length t.buffer - t.stop)
      with
      | 0 -> Error (`Wire Closed)
      | count ->
          t.stop <- t.stop + count;
          loop ()
  in
  loop ()

let[@zero_alloc] fill t needed =
  if t.stop - t.start >= needed then Ok () else receive t needed

let header t =
  let%bind () = fill t 4 in
  let length =
    Base_bigstring.unsafe_get_uint16_le t.buffer ~pos:t.start
    lor (Base_bigstring.unsafe_get_uint8 t.buffer ~pos:(t.start + 2) lsl 16)
  and sequence = Base_bigstring.unsafe_get_uint8 t.buffer ~pos:(t.start + 3) in
  if sequence = t.sequence then (
    t.sequence <- (sequence + 1) land 0xFF;
    Ok length)
  else Error (`Wire (Out_of_order { expected = t.sequence; got = sequence }))

(* A packet of 16 MiB or more arrives in several frames and is copied into one
   buffer: rare, and costly anyway, so the checker trusts it. *)
let[@inline never] [@zero_alloc assume] joined t cursor ~sequence =
  let%bind length = header t in
  let rec join joined length =
  let%bind () = fill t (4 + length) in
    if Base_bigstring.length t.joined < joined + length then (
      let grown =
        Base_bigstring.create
          (Int.max (joined + length)
             (2 * Base_bigstring.length t.joined))
      in
      Base_bigstring.blit ~src:t.joined ~src_pos:0 ~dst:grown
        ~dst_pos:0 ~len:joined;
      t.joined <- grown);
    Base_bigstring.blit ~src:t.buffer ~src_pos:(t.start + 4)
      ~dst:t.joined ~dst_pos:joined ~len:length;
    t.start <- t.start + 4 + length;
    if length < frame_limit then (
      Cursor.point cursor t.joined ~off:0 ~len:(joined + length)
        ~sequence;
      Ok ())
    else
      let%bind next = header t in
      join (joined + length) next
  in
  join 0 length

let[@zero_alloc] next t cursor =
  let sequence = t.sequence in
  match fill t 4 with
  | Error _ as error -> error
  | Ok () ->
      let length =
        Base_bigstring.unsafe_get_uint16_le t.buffer ~pos:t.start
        lor (Base_bigstring.unsafe_get_uint8 t.buffer ~pos:(t.start + 2) lsl 16)
      in
      if
        length < frame_limit
        && Base_bigstring.unsafe_get_uint8 t.buffer ~pos:(t.start + 3) = sequence
      then (
        t.sequence <- (sequence + 1) land 0xFF;
        match fill t (4 + length) with
        | Error _ as error -> error
        | Ok () ->
            Cursor.point cursor t.buffer ~off:(t.start + 4) ~len:length
              ~sequence;
            t.start <- t.start + 4 + length;
            Ok ())
      else
        joined t cursor ~sequence
