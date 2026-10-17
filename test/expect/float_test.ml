let%expect_test "float text" =
  List.iter
    (fun value ->
      let out = Quiver_core.Byte_buffer.create () in
      Quiver_sink.Render.float out (Float_u.of_float value);
      print_endline (Quiver_core.Byte_buffer.contents out))
    [
      0.1;
      1.;
      0.;
      -0.;
      100.;
      3.14159;
      0.000123;
      1e-5;
      12345678.9;
      1e15;
      1e16;
      1e21;
      123456789012345680000.;
      5e-324;
      Float.max_float;
      -2.5e-10;
      Int32.float_of_bits (Int32.bits_of_float 0.1);
    ];
  [%expect
    {|
    0.1
    1
    0
    -0
    100
    3.14159
    0.000123
    1e-05
    12345678.9
    1000000000000000
    1e+16
    1e+21
    1.2345678901234568e+20
    5e-324
    1.7976931348623157e+308
    -2.5e-10
    0.10000000149011612
    |}]
