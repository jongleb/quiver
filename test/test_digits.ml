open Quiver_core

let render f value =
  let out = Quiver_core.Byte_buffer.create ~cap:32 () in
  f out value;
  Quiver_core.Byte_buffer.contents out

let agrees name gen print ours reference =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~count:5000 ~name ~print gen (fun value ->
         String.equal (render ours value) (reference value)))

let int64s =
  QCheck2.Gen.(
    oneof
      [
        int64;
        oneof_list
          [
            Int64.min_int;
            Int64.max_int;
            0L;
            -1L;
            1L;
            4611686018427387904L;
            -4611686018427387905L;
          ];
      ])

let tests =
  [
    agrees "int matches %d"
      QCheck2.Gen.(oneof [ int; oneof_list [ min_int; max_int; 0; -1 ] ])
      string_of_int Digits.int (Printf.sprintf "%d");
    agrees "int64 matches %Ld" int64s Int64.to_string
      (fun out n -> Digits.int64 out (Int64_u.of_int64 n))
      (Printf.sprintf "%Ld");
    agrees "uint64 matches %Lu" int64s Int64.to_string
      (fun out n -> Digits.uint64 out (Int64_u.of_int64 n))
      (Printf.sprintf "%Lu");
    agrees "padded matches %0*d"
      QCheck2.Gen.(pair (int_range 0 9999999) (int_range 1 8))
      (fun (n, w) -> Printf.sprintf "%d/%d" n w)
      (fun out (n, width) -> Digits.padded out ~width n)
      (fun (n, width) -> Printf.sprintf "%0*d" width n);
  ]

let tests =
  tests
  @ [
      QCheck_alcotest.to_alcotest
        (QCheck2.Test.make ~count:20000
           ~name:"floats render to an exact decimal"
           ~print:(Printf.sprintf "%h")
           QCheck2.Gen.(
             oneof
               [
                 map Int64.float_of_bits int64;
                 map (fun bits -> Int32.float_of_bits bits) int32;
                 oneof_list
                   [
                     0.;
                     -0.;
                     Float.max_float;
                     Float.min_float;
                     5e-324;
                     0.1;
                     1e21;
                     1e-7;
                     3.40282e38;
                   ];
               ])
           (fun value ->
             (not (Float.is_finite value))
             || Int64.equal
                  (Int64.bits_of_float
                     (float_of_string
                        (render
                           (fun out v ->
                             Quiver_sink.Render.float out (Float_u.of_float v))
                           value)))
                  (Int64.bits_of_float value)));
    ]
