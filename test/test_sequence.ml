open Quiver_core

let edgy =
  QCheck2.Gen.(
    oneof_weighted
      [
        (4, map Int64.of_int (int_range (-20) 20));
        (2, oneof_list [ Int64.min_int; Int64.max_int; Int64.succ Int64.min_int; Int64.pred Int64.max_int ]);
        (1, map2 (fun high low -> Int64.add (Int64.shift_left (Int64.of_int high) 32) (Int64.of_int low)) int int);
      ])

let restored =
  QCheck2.Test.make ~count:2000
    ~name:"a sequence restores the value before its next one, unless that falls outside 64 bits"
    ~print:(fun (next, increment) ->
      Option.fold increment ~none:(Printf.sprintf "above=%Ld" next) ~some:(Printf.sprintf "next=%Ld increment=%Ld" next))
    QCheck2.Gen.(pair edgy (option edgy))
    (fun (next, increment) ->
      let counter : Sequence.counter =
        Option.fold increment ~none:(Sequence.Above next) ~some:(fun increment -> Sequence.Unused { next; increment })
      in
      let { Sequence.last; _ } = Sequence.make ~name:(Support.ident "s") ~text:"" counter in
      let step =
        Option.fold increment ~none:Z.one ~some:(fun increment ->
            if Int64.equal increment 0L then Z.one else Z.of_int64 increment)
      in
      let exact = Z.sub (Z.of_int64 next) step in
      let fits = Z.leq (Z.of_int64 Int64.min_int) exact && Z.leq exact (Z.of_int64 Int64.max_int) in
      match last with
      | Some last -> fits && Z.equal (Z.of_int64 last) exact
      | None -> not fits)

let tests =
  [
    QCheck_alcotest.to_alcotest restored;
    Alcotest.test_case "a sequence is created, then set to its last value" `Quick (fun () ->
        let text = "CREATE SEQUENCE `s 1` start with 100 minvalue -50 maxvalue 200 increment by -3 cache 5 cycle ENGINE=InnoDB" in
        let sequence = Sequence.make ~name:(Support.ident "s 1") ~text (Unused { next = 85L; increment = -3L }) in
        Alcotest.(check (list string))
          "statements"
          [ text; "DO SETVAL(`s 1`, 88)" ]
          (Sequence.statements sequence);
        Alcotest.(check (list string))
          "never handed out"
          [ text ]
          (Sequence.statements (Sequence.make ~name:(Support.ident "s 1") ~text (Above Int64.min_int))));
    Alcotest.test_case "a database's sequences are files the loader reads back" `Quick (fun () ->
        let dir = Filename.temp_dir "quiver" ".sequences" in
        let layout =
          Support.get_ok
            (Result.map_error
               (fun (`Output _) -> `Schema (Schema.No_fields Support.name))
               (Quiver_sink.Layout.create ~dir:(File_path.of_string dir) Quiver_sink.Compression.Plain))
        in
        let sequences =
          [
            Sequence.make ~name:(Support.ident "a.b") ~text:"CREATE SEQUENCE `a.b` start with 1" (Above 1001L);
            Sequence.make ~name:(Support.ident "c") ~text:"CREATE SEQUENCE `c` start with 5" (Unused { next = 5L; increment = 1L });
          ]
        in
        let () =
          match Quiver_sink.Layout.database layout (Support.ident "d b") ~ddl:"CREATE DATABASE `d b`" ~sequences with
          | Ok () -> ()
          | Error (`Output _) -> Alcotest.fail "not written"
        in
        let read file =
          let text = In_channel.with_open_bin (Filename.concat dir file) In_channel.input_all in
          let pos = ref 0 and statements = ref [] in
          let () =
            match
              Quiver_load.Statements.iter
                (fun bytes ~pos:at ~len ->
                  let n = Int.min len (String.length text - !pos) in
                  Bytes.blit_string text !pos bytes at n;
                  pos := !pos + n;
                  Ok n)
                (fun statement ->
                  statements := statement :: !statements;
                  Ok ())
            with
            | Ok () -> ()
            | Error (`Statements _) -> Alcotest.fail "unsplittable"
          in
          List.filter
            (fun statement -> not (String.starts_with ~prefix:"/*!" statement))
            (List.rev !statements)
        in
        Alcotest.(check (list string))
          "files"
          [ "d%20b-schema-create.sql"; "d%20b.a%2Eb-schema-sequence.sql"; "d%20b.c-schema-sequence.sql" ]
          (List.sort String.compare (Array.to_list (Sys.readdir dir)));
        List.iter
          (fun (file, sequence) ->
            Alcotest.(check (list string)) file (Sequence.statements sequence) (read file))
          [ ("d%20b.a%2Eb-schema-sequence.sql", List.nth sequences 0); ("d%20b.c-schema-sequence.sql", List.nth sequences 1) ]);
  ]
