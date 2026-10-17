let%expect_test "text probes stay in the keys' own alphabet" =
  List.iter
    (function
      | Quiver_core.Key.Text probe -> print_endline probe
      | _ -> print_endline "not text")
    (Quiver_core.Probe.between (Text "0000015f85ed37b2b6a4ed0bb5577ca28737")
       (Text "fffffdd3683a9ad3b314c6fcfc48f8d8e49f") ~count:4);
  [%expect {|
    3f
    7e
    bd
    |}]
