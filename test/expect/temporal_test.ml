let%expect_test "date and time text" =
  let fsp n = Result.get_ok (Quiver_core.Ltype.Fsp.of_int n) in
  let show f =
    let out = Quiver_core.Byte_buffer.create () in
    (match f out with
    | Ok () -> ()
    | Error `Unrepresentable_date ->
        Quiver_core.Byte_buffer.append_string out "unrepresentable");
    print_endline (Quiver_core.Byte_buffer.contents out)
  in
  show (fun out -> Quiver_core.Temporal.date out ~days:0);
  show (fun out -> Quiver_core.Temporal.date out ~days:(-719528));
  show (fun out -> Quiver_core.Temporal.date out ~days:2932896);
  show (fun out -> Quiver_core.Temporal.date out ~days:2932897);
  List.iter
    (fun (precision, micros) ->
      show (fun out ->
          Quiver_core.Temporal.datetime out (fsp precision)
            ~micros:(Int64_u.of_int64 micros)))
    [
      (0, 0L);
      (6, 1L);
      (3, 1_234_567L);
      (6, 253402300799999999L);
      (0, -1L);
      (6, -1L);
      (2, 1_700_000_000_123_456L);
    ];
  List.iter
    (fun (precision, micros) ->
      show (fun out ->
          Quiver_core.Temporal.time out (fsp precision)
            ~micros:(Int64_u.of_int64 micros);
          Ok ()))
    [
      (0, 0L);
      (6, -1L);
      (0, 3_020_399_000_000L);
      (6, -3_020_399_000_000L);
      (1, 45_296_700_000L);
    ];
  [%expect
    {|
    1970-01-01
    0000-01-01
    9999-12-31
    unrepresentable
    1970-01-01 00:00:00
    1970-01-01 00:00:00.000001
    1970-01-01 00:00:01.234
    9999-12-31 23:59:59.999999
    1969-12-31 23:59:59
    1969-12-31 23:59:59.999999
    2023-11-14 22:13:20.12
    00:00:00
    -00:00:00.000001
    838:59:59
    -838:59:59.000000
    12:34:56.7
    |}]
