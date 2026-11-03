let () =
  let responses = int_of_string Sys.argv.(1) and scan_only = Array.length Sys.argv > 2 && Sys.argv.(2) = "scan" in
  let response first =
    let buffer = Buffer.create (1024 * 100) in
    Base.Fn.apply_n_times ~n:1024
      (fun i ->
        let pair = Buffer.create 100 in
        Quiver_tikv.Proto.bytes pair 2 (fst (Quiver_tikv.Codec.records 75L) ^ Printf.sprintf "%08d" (first + i));
        Quiver_tikv.Proto.bytes pair 3 (String.make (40 + (i mod 40)) 'v');
        Quiver_tikv.Proto.bytes buffer 2 (Buffer.contents pair);
        i + 1)
      0
    |> ignore;
    Buffer.contents buffer
  in
  let texts = Array.init 8 (fun i -> response (i * 1024)) in
  let started = Unix.gettimeofday () in
  let gathered = Quiver_tikv.Pairs.create () and size = ref 0 and checksum = ref 0L and encoded = ref 0 in
  let flush () =
    let job : Quiver_tikv.Import.job =
      {
        uuid = String.make 16 'u';
        region = { id = 1L; conf_ver = 1L; version = 1L; peer = 1L; store = 1L };
        commit_ts = 7L;
        pairs = Quiver_tikv.Pairs.cut (Quiver_tikv.Pairs.all gathered) ~bytes:16384;
      }
    in
    List.iter (fun request -> encoded := !encoded + String.length request) (Quiver_tikv.Import.write_requests job);
    Quiver_tikv.Pairs.clear gathered;
    size := 0
  in
  Base.Fn.apply_n_times ~n:responses
    (fun i ->
      let () =
        match Quiver_tikv.Kv.scanned texts.(i mod 8) with
        | Ok (Pairs _) when scan_only -> ()
        | Ok (Pairs pairs) ->
            List.iter
              (fun (pair : Quiver_tikv.Kv.pair) ->
                let key = Quiver_tikv.Codec.retabled ~into:300L pair.key in
                Quiver_tikv.Pairs.add gathered key pair.value;
                checksum := Int64.logxor !checksum (Quiver_tikv.Crc64.pair key pair.value);
                size := !size + String.length pair.key + String.length pair.value)
              pairs
        | Ok _ | Error _ -> exit 1
      in
      let () = if !size >= 16 lsl 20 then flush () in
      i + 1)
    0
  |> ignore;
  flush ();
  let seconds = Unix.gettimeofday () -. started in
  Printf.printf "%d pairs: %.3f s, %.0f ns/pair, %d bytes encoded, minor words %.0f/pair (%Lx)\n" (responses * 1024) seconds
    (seconds *. 1e9 /. float (responses * 1024)) !encoded (Gc.minor_words () /. float (responses * 1024)) !checksum
