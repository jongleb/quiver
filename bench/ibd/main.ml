let fail what sexp = failwith (Printf.sprintf "%s: %s" what (Sexplib0.Sexp.to_string sexp))

let ok = function
  | Ok value -> value
  | Error (`Cfg error) -> fail "cfg" (Quiver_innodb.Cfg.sexp_of_error error)
  | Error (`Layout error) -> fail "layout" (Quiver_innodb.Layout.sexp_of_error error)
  | Error (`Tablespace error) -> fail "tablespace" (Quiver_innodb.Tablespace.sexp_of_error error)
  | Error (`File error) -> fail "file" (Quiver_innodb.File.sexp_of_error error)
  | Error (`Check error) -> fail "check" (Quiver_innodb.Check.sexp_of_error error)

let seconds started = Core.Time_ns.Span.to_sec (Quiver_core.Stopwatch.elapsed started)

let () =
  let dir = Sys.argv.(1) in
  let rows = int_of_string Sys.argv.(2) and parts = int_of_string Sys.argv.(3) in
  let threads = int_of_string Sys.argv.(4) in
  let read_file name = In_channel.with_open_bin (Filename.concat dir name) In_channel.input_all in
  let cfg = ok (Quiver_innodb.Cfg.parse (read_file "t049.cfg")) in
  let dumped =
    ("t", Quiver_core.Ltype.String)
    :: List.map (fun name -> (name, Quiver_core.Ltype.Uint32)) [ "a"; "b"; "c"; "d"; "e"; "f" ]
  in
  let collations = [ (83, Quiver_innodb.Layout.Utf8mb3); (33, Quiver_innodb.Layout.Utf8mb3) ] in
  let layout = ok (Quiver_innodb.Layout.of_cfg cfg ~collations dumped) in
  let secondary = List.map (fun index -> ok (Quiver_innodb.Layout.secondary layout cfg index)) cfg.secondary in
  let path = Filename.concat (Filename.get_temp_dir_name ()) "bench-ibd.ibd" in
  let file = ok (Quiver_innodb.File.create path) in
  let scratch = ok (Quiver_innodb.File.create (path ^ ".sort")) in
  let space = ok (Quiver_innodb.Tablespace.create ~template:(read_file "t049.ibd") layout ~secondary file ~scratch) in
  let urls = Option.fold ~none:0 ~some:int_of_string (Sys.getenv_opt "BENCH_URLS") in
  let span = Int.max 1 (rows * 2 / 15) in
  let b i = i * 7919 mod span in
  let order =
    let part i = b i * parts / span in
    let order = Array.init rows Fun.id in
    Array.stable_sort (fun x y -> Int.compare (part x) (part y)) order;
    let starts =
      Iarray.init (parts + 1) (fun p ->
          match
            Base.Array.binary_search_segmented order
              ~segment_of:(fun i -> if part i < p then `Left else `Right)
              `First_on_right
          with
          | Some at -> at
          | None -> rows)
    in
    (Iarray.of_array order, starts)
  in
  let started = Quiver_core.Stopwatch.start () in
  let finished =
    Quiver_core.Concurrent.spread ~workers:threads
      (Iarray.init parts (fun p -> p))
      (fun p ->
        let order, starts = order in
        let run = Quiver_innodb.Tablespace.run space ~rank:p in
        let text i =
          let lead =
            if i * 7 mod 100 < urls then "https://www."
            else
              match i mod 7 with
              | 0 -> "http://"
              | 1 -> "https://www."
              | 2 -> "ftp "
              | 3 -> "x"
              | 4 -> "é日"
              | 5 -> ""
              | _ -> " "
          in
          lead ^ Digest.to_hex (Digest.string (string_of_int (i / 3)))
          ^ (if i mod 11 = 0 then "  " else "")
          ^ if i mod 13 = 0 then "\t" else ""
        in
        let rec go at until =
          if at < until then (
            let n = Int.min 2000 (until - at) in
            let columns =
              Iarray.of_list
                (List.map
                   (fun (name, ltype) ->
                     let (Quiver_core.Column.Any column) = Quiver_core.Column.create ltype in
                     let repr = Quiver_core.Column.repr column in
                     let rec fill k =
                       if k < at + n then (
                       let i = Iarray.get order k in
                       let added : (unit, [ `Column of Quiver_core.Column.error ]) result =
                         match (repr, name) with
                         | Text, _ -> Quiver_core.Column.add column (text i)
                         | Int, "a" when i mod 17 = 0 ->
                             Quiver_core.Column.add_null column;
                             Ok ()
                         | Int, "a" -> Quiver_core.Column.add column (i mod 1000)
                         | Int, "b" -> Quiver_core.Column.add column (b i)
                         | Int, "c" -> Quiver_core.Column.add column (i mod 3)
                         | Int, "d" -> Quiver_core.Column.add column (i mod 5)
                         | Int, "e" -> Quiver_core.Column.add column (i mod 7)
                         | Int, _ -> Quiver_core.Column.add column i
                         | _ -> Ok ()
                       in
                       let () = match added with Ok () -> () | Error (`Column _) -> failwith "column" in
                       fill (k + 1))
                     in
                     fill at;
                     Quiver_core.Column.Any column)
                   dumped)
            in
            match Quiver_innodb.Tablespace.encode space columns ~rows:n with
            | Error (`Tablespace error) -> Error (`Tablespace error)
            | Ok records -> (
                match Quiver_innodb.Tablespace.add run records with
                | Ok () -> go (at + n) until
                | Error error -> Error error))
          else Ok ()
        in
        let closed =
          match go (Iarray.get starts p) (Iarray.get starts (p + 1)) with
          | Ok () -> Quiver_innodb.Tablespace.close run
          | Error error -> Error error
        in
        match closed with
        | Ok finished -> (Ok finished : (Quiver_innodb.Tablespace.finished, Sexplib0.Sexp.t) result)
        | Error (`Tablespace error) -> Error (Quiver_innodb.Tablespace.sexp_of_error error)
        | Error (`File error) -> Error (Quiver_innodb.File.sexp_of_error error))
  in
  let finished = List.map (function Ok finished -> finished | Error sexp -> fail "part" sexp) finished in
  let built = seconds started in
  Option.iter
    (fun pause ->
      Printf.eprintf "finishing\n%!";
      Unix.sleepf (float_of_string pause))
    (Sys.getenv_opt "BENCH_PAUSE");
  let started = Quiver_core.Stopwatch.start () in
  let pages = ok (Quiver_innodb.Tablespace.finish space finished ~threads) in
  let finishing = seconds started in
  let read n =
    let page = Bytes.make Quiver_innodb.Page.size '\000' in
    let (_ : (unit, _) result) = Quiver_innodb.File.read file n page in
    page
  in
  let started = Quiver_core.Stopwatch.start () in
  let checked = ok (Quiver_innodb.Check.tablespace layout ~secondary ~pages ~threads ~read) in
  let checking = seconds started in
  Printf.printf "rows %d parts %d threads %d pages %d: parts %.3f s, finish %.3f s, check %.3f s\n" checked.rows parts
    threads pages built finishing checking;
  Quiver_innodb.File.close file;
  Quiver_innodb.File.close scratch;
  Sys.remove path;
  Sys.remove (path ^ ".sort")
