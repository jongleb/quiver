type kind = Message | Batch | Ack
type codec = Plain | Zstd

type error =
  | Bad_magic
  | Unsupported_version of int
  | Damaged_header
  | Damaged_payload
  | Unknown_kind of int
  | Unknown_codec of int
  | Too_large of int
[@@deriving sexp_of ~portable]

type header = {
  kind : kind;
  codec : codec;
  chunk : int;
  seq : int;
  length : int;
  raw : int;
  checksum : int32;
}

let header_bytes = 32
let max_payload = 1 lsl 30
let version = 5
let checked_bytes = 28

let xxh3 bytes ~first ~length =
  Int64.to_int32 (Quiver_sink.Xxh3.digest bytes ~pos:first ~len:length)

let check bytes =
  Bytes.set_int32_le bytes checked_bytes
    (xxh3 bytes ~first:0 ~length:checked_bytes)

let start (buffer : Quiver_core.Byte_buffer.t) =
  Quiver_core.Byte_buffer.clear buffer;
  Quiver_core.Byte_buffer.ensure_cap buffer header_bytes;
  Bytes.fill buffer.bs 0 header_bytes '\000';
  buffer.len <- header_bytes

let seal (buffer : Quiver_core.Byte_buffer.t) kind codec ~raw ~chunk =
  let bs = buffer.bs and length = buffer.len - header_bytes in
  Bytes.set bs 0 'Q';
  Bytes.set bs 1 'W';
  Bytes.set_uint8 bs 2 version;
  Bytes.set_uint8 bs 3 0;
  Bytes.set_uint8 bs 4 (match kind with Message -> 0 | Batch -> 1 | Ack -> 2);
  Bytes.set_uint8 bs 5 (match codec with Plain -> 0 | Zstd -> 1);
  Bytes.set_uint16_le bs 6 0;
  Bytes.set_int32_le bs 8 (Int32.of_int chunk);
  Bytes.set_int32_le bs 12 0l;
  Bytes.set_int32_le bs 16 (Int32.of_int length);
  Bytes.set_int32_le bs 20 (Int32.of_int raw);
  Bytes.set_int32_le bs 24 (xxh3 bs ~first:header_bytes ~length);
  check bs

let number bytes ~seq =
  Bytes.set_int32_le bytes 12 (Int32.of_int seq);
  check bytes

let header text =
  let bytes = Bytes.of_string text in
  let unsigned at =
    Int32.to_int (Bytes.get_int32_le bytes at) land 0xFFFF_FFFF
  in
  match
    ( Bytes.sub_string bytes 0 2,
      Bytes.get_uint8 bytes 2,
      Int32.equal
        (Bytes.get_int32_le bytes checked_bytes)
        (xxh3 bytes ~first:0 ~length:checked_bytes) )
  with
  | exception Invalid_argument _ -> Error (`Frame Damaged_header)
  | magic, _, _ when not (String.equal magic "QW") -> Error (`Frame Bad_magic)
  | _, found, _ when found <> version ->
      Error (`Frame (Unsupported_version found))
  | _, _, false -> Error (`Frame Damaged_header)
  | _, _, true -> (
      match
        ( Bytes.get_uint8 bytes 4,
          Bytes.get_uint8 bytes 5,
          unsigned 16,
          unsigned 20 )
      with
      | kind, _, _, _ when kind > 2 -> Error (`Frame (Unknown_kind kind))
      | _, codec, _, _ when codec > 1 -> Error (`Frame (Unknown_codec codec))
      | _, _, length, _ when length > max_payload ->
          Error (`Frame (Too_large length))
      | _, _, _, raw when raw > max_payload -> Error (`Frame (Too_large raw))
      | kind, codec, length, raw ->
          Ok
            {
              kind = (match kind with 0 -> Message | 1 -> Batch | _ -> Ack);
              codec = (match codec with 0 -> Plain | _ -> Zstd);
              chunk = unsigned 8;
              seq = unsigned 12;
              length;
              raw;
              checksum = Bytes.get_int32_le bytes 24;
            })

let payload header text =
  match
    Int32.equal header.checksum
      (xxh3 (Bytes.unsafe_of_string text) ~first:0
         ~length:(Int.min header.length (String.length text)))
  with
  | true -> Ok ()
  | false -> Error (`Frame Damaged_payload)
