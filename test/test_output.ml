open Base.Result.Let_syntax

let frames =
  QCheck2.Gen.(
    list_size (int_range 0 6)
      (string_size ~gen:char
         (oneof [ pure 0; int_range 1 100; int_range 1000 300_000 ])))

let compression =
  QCheck2.Gen.(
    oneof
      [
        pure Quiver_sink.Compression.Plain;
        map
          (fun level ->
            Quiver_sink.Compression.Zstd
              (Result.get_ok (Quiver_sink.Compression.Level.of_int level)))
          (int_range 1 5);
      ])

let stored =
  QCheck2.Test.make ~count:150
    ~name:
      "output decompresses to its frames; manifest counts and hashes the \
       stored bytes"
    ~print:(fun (frames, compression) ->
      Printf.sprintf "%s: %s"
        (Sexplib0.Sexp.to_string_hum (Quiver_sink.Compression.sexp_of_t compression))
        (String.concat ","
           (List.map (fun f -> string_of_int (String.length f)) frames)))
    QCheck2.Gen.(pair frames compression)
    (fun (frames, compression) ->
      let file =
        File_path.of_string (Filename.temp_file "quiver-output" ".out")
      in
      let buffers = Quiver_core.Buffer_pool.create () in
      match
        let%bind output = Quiver_sink.Output.create file compression in
        let%bind () =
          Base.List.fold_result frames ~init:() ~f:(fun () text ->
              let buffer = Quiver_sink.Output.start buffers in
              Quiver_core.Byte_buffer.append_string (borrow_ buffer) text;
              Quiver_sink.Output.emit output buffers buffer)
        in
        Quiver_sink.Output.close output ~rows:(List.length frames)
      with
      | Error _ -> QCheck2.Test.fail_report "output failed"
      | Ok { bytes; stored_bytes; checksum = Xxh3_64 hash; rows; _ } ->
          let on_disk = Stdio.In_channel.read_all (File_path.to_string file) in
          let plain =
            match compression with
            | Plain -> on_disk
            | Zstd _ ->
                Bytesrw.Bytes.Reader.to_string
                  (Bytesrw_zstd.decompress_reads ~all_frames:true ()
                     (Bytesrw.Bytes.Reader.of_string on_disk))
          in
          String.equal plain (String.concat "" frames)
          && bytes = String.length plain
          && stored_bytes = String.length on_disk
          && rows = List.length frames
          && Int64.equal hash
               (Bytesrw_xxhash.Xxh3_64.to_uint64
                  (Bytesrw_xxhash.Xxh3_64.string on_disk)))

let tests = [ QCheck_alcotest.to_alcotest stored ]
