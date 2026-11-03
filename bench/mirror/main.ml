let fail text =
  prerr_endline text;
  exit 1

let show error = Base.Sexp.to_string_hum error

let () =
  let port = int_of_string Sys.argv.(1) and rows = int_of_string Sys.argv.(2) in
  let link =
    match
      Quiver_mysql.Link.connect ~local_data:Without_local_data
        (Tcp { host = "127.0.0.1"; port; tls = Plaintext })
        ~timeouts:{ connecting = Core.Time_ns.Span.of_int_sec 10; silence = Core.Time_ns.Span.of_int_sec 600 }
        ~user:"root" ~password:None
    with
    | Ok link -> link
    | Error _ -> fail "no connection"
  in
  let run text = match Quiver_mysql.Link.execute link text with Ok () -> () | Error _ -> fail ("failed: " ^ text) in
  let query text = match Quiver_mysql.Link.query link text with Ok rows -> rows | Error _ -> fail ("failed: " ^ text) in
  let one text = match query text with [ row ] -> Array.to_list (Array.map (Option.value ~default:"NULL") row) | _ -> fail ("not one row: " ^ text) in
  let tso () = match query "SHOW MASTER STATUS" with [ row ] -> Int64.of_string (Option.get row.(1)) | _ -> fail "no TSO" in
  let () =
    if rows > 0 then (
      run "DROP DATABASE IF EXISTS mq";
      run "CREATE DATABASE mq";
      run
        "CREATE TABLE mq.keyed (id BIGINT PRIMARY KEY CLUSTERED, a INT NOT NULL, b VARCHAR(64), c DOUBLE, d JSON, KEY ka (a), UNIQUE KEY kb (b))";
      run "CREATE TABLE mq.loose (a INT, b VARCHAR(300), KEY kb (b(20)))";
      let started = Unix.gettimeofday () in
      let batch = 2000 in
      Base.Fn.apply_n_times ~n:((rows + batch - 1) / batch)
        (fun from ->
          let values f = String.concat ", " (List.init (Int.min batch (rows - from)) (fun i -> f (from + i))) in
          run
            ("INSERT INTO mq.keyed VALUES "
            ^ values (fun i ->
                  Printf.sprintf "(%d, %d, %s, %d.25, '{\"k\": %d}')" (i * 7) (i mod 1000)
                    (if i mod 10 = 0 then "NULL" else Printf.sprintf "'b%d-%s'" i (String.make (i mod 40) 'x'))
                    i i));
          run ("INSERT INTO mq.loose VALUES " ^ values (fun i -> Printf.sprintf "(%d, '%s%d')" (i mod 97) (String.make (i mod 250) 'y') i));
          from + batch)
        0
      |> ignore;
      Printf.printf "inserted %d rows twice in %.1f s\n%!" rows (Unix.gettimeofday () -. started);
      run "SPLIT TABLE mq.keyed BETWEEN (0) AND (1000000000) REGIONS 4";
      ())
  in
  run "DROP TABLE IF EXISTS mq.keyed_copy, mq.loose_copy";
  run "DELETE FROM mq.keyed WHERE id = -1";
  ignore tso;
  let name table : Quiver_core.Schema.Name.t =
    match (Quiver_core.Ident.of_string "mq", Quiver_core.Ident.of_string table) with
    | Ok database, Ok table -> { database; table }
    | _ -> fail "name"
  in
  let stores : Quiver_tikv.Mirror.stores = if Array.length Sys.argv > 4 && Sys.argv.(4) = "imported" then Imported else Untouched in
  List.iter
    (fun table ->
      let started = Unix.gettimeofday () in
      match
        Quiver_tikv.Mirror.copy link ~source:(name table) ~target:(name (table ^ "_copy"))
          {
            timeout = Core.Time_ns.Span.of_int_sec 30;
            sizes = { scan = 1024; job = 16 lsl 20; batch = 16 lsl 10 };
            workers = int_of_string Sys.argv.(3);
            stores;
          }
      with
      | Ok copied ->
          Printf.printf "%s: %d keys, %d bytes, checksum %Lx as TiDB's, in %.2f s\n%!" table copied.keys copied.bytes copied.checksum
            (Unix.gettimeofday () -. started)
      | Error error ->
          fail
            (show
               ([%sexp_of:
                  [ `Mirror of Quiver_tikv.Mirror.error
                  | `Read of Quiver_tikv.Read.error
                  | `Pd of Quiver_tikv.Pd.error
                  | `Ingest of Quiver_tikv.Ingest.error
                  | `Region of Quiver_tikv.Region.error
                  | `Link of Quiver_mysql.Link.error
                  | `Wire of Quiver_mysql.Wire.error
                  | `Transport of Quiver_mysql.Transport.error
                  | `Proto of Quiver_tikv.Proto.error
                  | `Grpc of Quiver_tikv.Grpc.error
                  | `Http2 of Quiver_tikv.Http2.error
                  | `Hpack of Quiver_tikv.Hpack.error ]]
                  error)))
    [ "keyed"; "loose" ];
  let compare label left right =
    let a = one left and b = one right in
    Printf.printf "%-40s %s %s\n%!" label (String.concat "," a) (if a = b then "equal" else "DIFFERENT: " ^ String.concat "," b)
  in
  let sums table rest = Printf.sprintf "SELECT COUNT(*), BIT_XOR(CRC32(CONCAT_WS('|', %s))) FROM mq.%s" rest table in
  compare "keyed rows" (sums "keyed WHERE id >= 0" "id, a, b, c, d") (sums "keyed_copy" "id, a, b, c, d");
  compare "keyed by index ka" (sums "keyed FORCE INDEX (ka) WHERE id >= 0 AND a >= 0" "id, a") (sums "keyed_copy FORCE INDEX (ka) WHERE a >= 0" "id, a");
  compare "keyed by index kb" (sums "keyed FORCE INDEX (kb) WHERE id >= 0 AND b >= ''" "id, b") (sums "keyed_copy FORCE INDEX (kb) WHERE b >= ''" "id, b");
  compare "loose rows" (sums "loose" "a, b") (sums "loose_copy" "a, b");
  compare "loose by index kb" (sums "loose FORCE INDEX (kb) WHERE b >= ''" "a, b") (sums "loose_copy FORCE INDEX (kb) WHERE b >= ''" "a, b");
  run "INSERT INTO mq.loose_copy VALUES (1, 'after')";
  run "INSERT INTO mq.keyed_copy (id, a, b) VALUES (-1, 1, 'after')";
  compare "rows added after, no id reused" "SELECT COUNT(DISTINCT _tidb_rowid) FROM mq.loose_copy" "SELECT COUNT(*) FROM mq.loose_copy";
  List.iter (fun table -> run ("ADMIN CHECK TABLE mq." ^ table); Printf.printf "ADMIN CHECK TABLE mq.%s: ok\n%!" table) [ "keyed_copy"; "loose_copy" ];
  Quiver_mysql.Link.close link
