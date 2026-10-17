let level = Result.get_ok (Quiver_sink.Compression.Level.of_int 3)

let frame text =
  let source = Quiver_core.Byte_buffer.create () in
  Quiver_core.Byte_buffer.append_string source text;
  let into = Quiver_core.Byte_buffer.create () in
  match Quiver_sink.Compression.zstd level source ~into with
  | Ok () -> Quiver_core.Byte_buffer.contents into
  | Error message -> failwith message

let reference text =
  Bytesrw.Bytes.Reader.to_string
    (Bytesrw_zstd.decompress_reads ~all_frames:true ()
       (Bytesrw.Bytes.Reader.of_string text))

let inflated compressed ~slice ~ask =
  let at = ref 0 in
  let stored bytes ~pos ~len =
    let length = Int.min (Int.min len slice) (String.length compressed - !at) in
    Bytes.blit_string compressed !at bytes pos length;
    at := !at + length;
    Ok length
  in
  let read =
    Quiver_sink.Compression.inflate stored ~corrupt:(fun message -> message)
  in
  let out = Buffer.create 256 and bytes = Bytes.create ask in
  let rec go () =
    match read bytes ~pos:0 ~len:ask with
    | Ok 0 -> Ok (Buffer.contents out)
    | Ok length ->
        Buffer.add_subbytes out bytes 0 length;
        go ()
    | Error message -> Error message
  in
  go ()

let unzstd compressed =
  let into = Quiver_core.Byte_buffer.create () in
  Result.map
    (fun () -> Quiver_core.Byte_buffer.contents into)
    (Quiver_sink.Compression.unzstd compressed ~into)

let texts =
  QCheck2.Gen.(
    list_size (int_range 1 4)
      (oneof
         [
           string_size (int_range 0 300);
           map
             (fun (unit, times) ->
               String.concat "" (List.init times (fun _ -> unit)))
             (pair (string_size (int_range 1 8)) (int_range 1 40_000));
         ]))

let print (texts, slice, ask) =
  Printf.sprintf "%s / slice %d / ask %d"
    (String.concat " | "
       (List.map (fun text -> string_of_int (String.length text)) texts))
    slice ask

let tests =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:300
         ~name:"frames inflate to their text however the reads are cut"
         ~print
         QCheck2.Gen.(triple texts (int_range 1 70_000) (int_range 1 70_000))
         (fun (texts, slice, ask) ->
           let compressed = String.concat "" (List.map frame texts)
           and text = String.concat "" texts in
           String.equal (reference compressed) text
           && Result.equal ~ok:String.equal ~error:String.equal
                (inflated compressed ~slice ~ask)
                (Ok text)
           && Result.equal ~ok:String.equal ~error:String.equal
                (unzstd compressed) (Ok text)));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:300
         ~name:"frames cut anywhere but between frames are refused"
         QCheck2.Gen.(
           triple texts (int_range 1 70_000) (int_range 1 70_000)
           >>= fun (texts, slice, ask) ->
           let total =
             List.fold_left
               (fun total text -> total + String.length (frame text))
               0 texts
           in
           map (fun cut -> (texts, slice, ask, cut)) (int_range 1 (total - 1)))
         ~print:(fun (texts, slice, ask, cut) ->
           Printf.sprintf "%s / cut %d" (print (texts, slice, ask)) cut)
         (fun (texts, slice, ask, cut) ->
           let frames = List.map frame texts in
           let boundaries =
             List.fold_left
               (fun (at, found) frame ->
                 (at + String.length frame, (at + String.length frame) :: found))
               (0, []) frames
             |> snd
           in
           QCheck2.assume (not (List.mem cut boundaries));
           let compressed = String.sub (String.concat "" frames) 0 cut in
           Result.is_error (inflated compressed ~slice ~ask)
           && Result.is_error (unzstd compressed)));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:500
         ~name:"xxh3 of a range and of its pieces matches the reference"
         QCheck2.Gen.(
           pair (string_size (int_range 0 2000)) (list_size (int_range 0 6) nat))
         (fun (text, cuts) ->
           let bytes = Bytes.of_string text and length = String.length text in
           let cuts =
             List.sort_uniq Int.compare
               (List.map (fun cut -> cut mod (length + 1)) cuts)
           in
           let state = Quiver_sink.Xxh3.create () in
           let last =
             List.fold_left
               (fun from cut ->
                 Quiver_sink.Xxh3.add state bytes ~pos:from ~len:(cut - from);
                 cut)
               0 cuts
           in
           Quiver_sink.Xxh3.add state bytes ~pos:last ~len:(length - last);
           let expected =
             Bytesrw_xxhash.Xxh3_64.to_uint64 (Bytesrw_xxhash.Xxh3_64.string text)
           in
           Int64.equal (Quiver_sink.Xxh3.value state) expected
           && Int64.equal
                (Quiver_sink.Xxh3.digest bytes ~pos:0 ~len:length)
                expected));
  ]

