let done_ ?(started = 0) ~duration ~pool_wait ~rendering ~appending ~committing () : Quiver_core.Manifest.state =
  let ms = Core.Time_ns.Span.of_int_ms in
  Done
    {
      part =
        {
          file = File_path.Part.of_string "part";
          rows = 1;
          bytes = 1;
          stored_bytes = 1;
          checksum = Xxh3_64 0L;
        };
      reading =
        {
          reader = 0;
          started = ms started;
          duration = ms duration;
          pool_wait = ms pool_wait;
          batches = 1;
          allocated_words = 0;
        };
      writing =
        {
          rendering = ms rendering;
          appending = ms appending;
          reordering = Core.Time_ns.Span.zero;
          committing = ms committing;
        };
    }

let holding states =
  match Quiver_core.Spent.holding (Quiver_core.Spent.of_states states) with
  | Source -> "source"
  | Rendering -> "rendering"
  | Writing -> "writing"

let tests =
  [
    Alcotest.test_case "time is summed over the parts, a pending one adds nothing"
      `Quick (fun () ->
        let spent =
          Quiver_core.Spent.of_states
            [
              done_ ~duration:100 ~pool_wait:40 ~rendering:10 ~appending:20 ~committing:5 ();
              done_ ~duration:50 ~pool_wait:10 ~rendering:1 ~appending:2 ~committing:3 ();
              Pending;
            ]
        in
        Alcotest.(check (list int))
          "reading, waiting, rendering, writing, committing" [ 100; 50; 11; 22; 8 ]
          (List.map Core.Time_ns.Span.to_int_ms
             [ spent.reading; spent.waiting; spent.rendering; spent.writing; spent.committing ]));
    Alcotest.test_case
      "readers held back a quarter of their time or more point past the source"
      `Quick (fun () ->
        Alcotest.(check string)
          "under a quarter" "source"
          (holding [ done_ ~duration:100 ~pool_wait:24 ~rendering:0 ~appending:50 ~committing:0 () ]);
        Alcotest.(check string)
          "a quarter, the writing heavier" "writing"
          (holding [ done_ ~duration:100 ~pool_wait:25 ~rendering:10 ~appending:9 ~committing:2 () ]);
        Alcotest.(check string)
          "a quarter, the rendering heavier" "rendering"
          (holding [ done_ ~duration:100 ~pool_wait:25 ~rendering:11 ~appending:9 ~committing:2 () ]));
      Alcotest.test_case "each table's parts are summed, the last to finish reading first" `Quick
      (fun () ->
        let name table : Quiver_core.Schema.Name.t =
          let ident text = Result.get_ok (Quiver_core.Ident.of_string text) in
          { database = ident "d"; table = ident table }
        in
        let entry table state : Quiver_core.Manifest.entry = { table = name table; key = []; range = None; state } in
        let tables =
          Quiver_core.Spent.of_tables
            [
              entry "a" (done_ ~started:10 ~duration:30 ~pool_wait:5 ~rendering:0 ~appending:0 ~committing:0 ());
              entry "b" (done_ ~started:0 ~duration:20 ~pool_wait:0 ~rendering:0 ~appending:0 ~committing:0 ());
              entry "a" (done_ ~started:5 ~duration:10 ~pool_wait:1 ~rendering:0 ~appending:0 ~committing:0 ());
              entry "c" Pending;
              entry "b" (done_ ~started:30 ~duration:50 ~pool_wait:0 ~rendering:0 ~appending:0 ~committing:0 ());
            ]
        in
        Alcotest.(check (list (pair string (list int))))
          "table, parts, began, ended, reading, held back"
          [ ("b", [ 2; 0; 80; 70; 0 ]); ("a", [ 2; 5; 40; 34; 6 ]) ]
          (List.map
             (fun ((name : Quiver_core.Schema.Name.t), ({ parts; spent; began; ended } : Quiver_core.Spent.table)) ->
               ( (name.table :> string),
                 parts :: List.map Core.Time_ns.Span.to_int_ms [ began; ended; spent.reading; spent.waiting ] ))
             tables));
  ]
