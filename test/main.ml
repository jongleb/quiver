let () =
  Alcotest.run "quiver"
    [
      ("chunk", Test_chunk.tests);
      ("batch", Test_batch.tests);
      ("batch model", Test_batch_model.tests);
      ("probe", Test_probe.tests);
      ("digits", Test_digits.tests);
      ("wire", Test_wire.tests);
      ("render", Test_render.tests);
      ("rows", Test_rows.tests);
      ("myloader", Test_myloader.tests);
      ("fuzz", Test_fuzz.tests);
      ("roundtrip", Test_roundtrip.tests);
      ("bisect", Test_bisect.tests);
      ("session", Test_session.tests);
      ("dump", Test_dump.tests);
      ("hostile", Test_hostile.tests);
      ("protection", Test_protection.tests);
      ("frame", Test_frame.tests);
      ("sinks", Test_sinks.tests);
      ("output", Test_output.tests);
      ("statements", Test_statements.tests);
      ("compression", Test_compression.tests);
      ("tcp", Test_tcp.tests);
      ("deferred keys", Test_deferred_keys.tests);
      ("moment", Test_moment.tests);
      ("assembly", Test_assembly.tests);
      ("concurrency", Test_concurrency.tests);
    ]
