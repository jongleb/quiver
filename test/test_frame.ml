open Base.Result.Let_syntax

let frame ?(chunk = 0) ?(seq = 0) kind codec payload ~raw =
  let buffer = Quiver_core.Byte_buffer.create () in
  Quiver_net.Frame.start buffer;
  Quiver_core.Byte_buffer.append_string buffer payload;
  Quiver_net.Frame.seal buffer kind codec ~raw ~chunk;
  let bytes = Bytes.sub buffer.bs 0 buffer.len in
  Quiver_net.Frame.number bytes ~seq;
  Bytes.to_string bytes

let kinds = QCheck2.Gen.oneof_list Quiver_net.Frame.[ Message; Batch ]
let codecs = QCheck2.Gen.oneof_list Quiver_net.Frame.[ Plain; Zstd ]

let read text =
  let%bind header =
    Quiver_net.Frame.header (String.sub text 0 Quiver_net.Frame.header_bytes)
  in
  let%map () =
    Quiver_net.Frame.payload header
      (String.sub text Quiver_net.Frame.header_bytes
         (String.length text - Quiver_net.Frame.header_bytes))
  in
  header

let sealed =
  QCheck2.Test.make ~count:500 ~name:"a sealed frame reads back as written"
    QCheck2.Gen.(
      pair
        (quad kinds codecs
           (string_size (int_range 0 300))
           (int_range 0 100_000))
        (pair (int_range 0 0xFFFF_FFFF) (int_range 0 0xFFFF_FFFF)))
    (fun ((kind, codec, payload, raw), (chunk, seq)) ->
      match read (frame ~chunk ~seq kind codec payload ~raw) with
      | Ok header ->
          header.kind = kind && header.codec = codec && header.chunk = chunk
          && header.seq = seq
          && header.length = String.length payload
          && header.raw = raw
      | Error _ -> false)

let damaged =
  QCheck2.Test.make ~count:2000
    ~name:"a frame with any byte changed is refused, never read wrong"
    QCheck2.Gen.(
      let* kind = kinds and* codec = codecs in
      let* payload = string_size (int_range 0 64) in
      let* at =
        int_range 0 (Quiver_net.Frame.header_bytes + String.length payload - 1)
      in
      let+ flip = int_range 1 255 in
      (kind, codec, payload, at, flip))
    (fun (kind, codec, payload, at, flip) ->
      let text = Bytes.of_string (frame kind codec payload ~raw:7) in
      Bytes.set_uint8 text at (Bytes.get_uint8 text at lxor flip);
      match read (Bytes.to_string text) with
      | Ok _ -> false
      | Error (`Frame _) -> true)

let short =
  Alcotest.test_case "a cut header is refused" `Quick (fun () ->
      Alcotest.(check bool)
        "refused" true
        (Result.is_error (Quiver_net.Frame.header "QW\001")))

let tests = short :: List.map QCheck_alcotest.to_alcotest [ sealed; damaged ]
