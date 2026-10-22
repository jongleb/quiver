let done_ ~duration ~pool_wait ~rendering ~appending ~committing : Quiver_core.Manifest.state =
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
          started = Core.Time_ns.Span.zero;
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
              done_ ~duration:100 ~pool_wait:40 ~rendering:10 ~appending:20 ~committing:5;
              done_ ~duration:50 ~pool_wait:10 ~rendering:1 ~appending:2 ~committing:3;
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
          (holding [ done_ ~duration:100 ~pool_wait:24 ~rendering:0 ~appending:50 ~committing:0 ]);
        Alcotest.(check string)
          "a quarter, the writing heavier" "writing"
          (holding [ done_ ~duration:100 ~pool_wait:25 ~rendering:10 ~appending:9 ~committing:2 ]);
        Alcotest.(check string)
          "a quarter, the rendering heavier" "rendering"
          (holding [ done_ ~duration:100 ~pool_wait:25 ~rendering:11 ~appending:9 ~committing:2 ]));
  ]
