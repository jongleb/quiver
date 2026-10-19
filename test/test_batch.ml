open Quiver_core

type case =
  | Case : {
      ltype : Ltype.t;
      repr : 'a Column.repr;
      testable : 'a Alcotest.testable;
      zero : 'a;
      values : 'a list;
    }
      -> case

let float = Alcotest.testable Format.pp_print_float Float.equal
let fsp n = Support.get_ok (Ltype.Fsp.of_int n)

let column : type a. Ltype.t -> a Column.repr -> a Column.t =
 fun ltype repr ->
  let same_repr : type b. b Column.repr -> (a, b) Type.eq option = function
    | Bool -> ( match repr with Bool -> Some Equal | _ -> None)
    | Int -> ( match repr with Int -> Some Equal | _ -> None)
    | Int64 -> ( match repr with Int64 -> Some Equal | _ -> None)
    | Float -> ( match repr with Float -> Some Equal | _ -> None)
    | Text -> ( match repr with Text -> Some Equal | _ -> None)
  in
  let (Column.Any column) = Column.create ltype in
  match same_repr (Column.repr column) with
  | Some Equal -> column
  | None ->
      Alcotest.failf "unexpected repr for %a"
        Sexplib0.Sexp.pp_hum
        (Ltype.sexp_of_t ltype)

let int ltype values =
  Case { ltype; repr = Int; testable = Alcotest.int; zero = 0; values }

let int64 ltype values =
  Case { ltype; repr = Int64; testable = Alcotest.int64; zero = 0L; values }

let text ltype values =
  Case { ltype; repr = Text; testable = Alcotest.string; zero = ""; values }

let rejects name ltype repr value expected =
  Alcotest.test_case name `Quick (fun () ->
      let column = column ltype repr in
      (match Column.add column value with
      | Ok () -> Alcotest.fail "value accepted"
      | Error (`Column error) ->
          Alcotest.check
            (Support.testable Column.sexp_of_error Column.equal_error)
            "error" expected error);
      Alcotest.(check int) "nothing written" 0 (Column.length column))

let tests =
  let presence =
    Support.testable Column.sexp_of_presence Column.equal_presence
  in
  List.map
    (fun (Case { ltype; repr; testable; zero; values }) ->
      Alcotest.test_case
        (Format.asprintf "%a with nulls"
           Sexplib0.Sexp.pp_hum
           (Ltype.sexp_of_t ltype))
        `Quick
        (fun () ->
          let column = column ltype repr in
          List.iter
            (fun value ->
              Support.get_ok (Column.add column value);
              Column.add_null column)
            values;
          Alcotest.(check int)
            "length"
            (2 * List.length values)
            (Column.length column);
          List.iteri
            (fun i value ->
              Alcotest.check presence "value present" Present
                (Column.presence column (2 * i));
              Alcotest.check testable "value" value (Column.get column (2 * i));
              Alcotest.check presence "null" Null
                (Column.presence column ((2 * i) + 1));
              Alcotest.check testable "null reads zero" zero
                (Column.get column ((2 * i) + 1)))
            values))
    [
      Case
        {
          ltype = Bool;
          repr = Bool;
          testable = Alcotest.bool;
          zero = false;
          values = [ true; false; true ];
        };
      int Int8 [ -128; 0; 127 ];
      int Uint8 [ 0; 255 ];
      int Int16 [ -32768; -1; 32767 ];
      int Uint16 [ 0; 65535 ];
      int Int32 [ -2147483648; -1; 2147483647 ];
      int Uint32 [ 0; 0xFFFF_FFFF ];
      int Date [ -719162; 0; 2932896 ];
      int64 Int64 [ Int64.min_int; -1L; Int64.max_int ];
      int64 Uint64 [ 0L; -1L ];
      int64 (Time (fsp 6)) [ -3020399000000L; 3020399000000L ];
      int64 (Datetime (fsp 0)) [ -62135596800000000L; 253402300799000000L ];
      int64 (Timestamp (fsp 3)) [ 0L; 2147483647000000L ];
      Case
        {
          ltype = Float32;
          repr = Float;
          testable = float;
          zero = 0.;
          values =
            [
              1.5;
              -0.;
              Float.infinity;
              Float.nan;
              Int32.float_of_bits 0x7F7FFFFFl;
            ];
        };
      Case
        {
          ltype = Float64;
          repr = Float;
          testable = float;
          zero = 0.;
          values = [ 0.1; Float.max_float; Float.neg_infinity; Float.nan ];
        };
      text
        (Decimal (Support.get_ok (Ltype.Decimal.make ~precision:65 ~scale:30)))
        [ "-123.450"; "0" ];
      text String [ ""; "héllo"; String.make 1000 'x' ];
      text Bytes [ "\x00\xff\x00"; "" ];
      text Json [ {|{"a":[1,null]}|} ];
      text (Enum [ "a"; "b" ]) [ "a"; "b" ];
      text (Set [ "a"; "b" ]) [ ""; "a,b" ];
    ]
  @ [
      rejects "int8 above range" Int8 Int 128
        (Out_of_range { ltype = Int8; value = 128 });
      rejects "uint8 below range" Uint8 Int (-1)
        (Out_of_range { ltype = Uint8; value = -1 });
      rejects "uint32 above range" Uint32 Int 0x1_0000_0000
        (Out_of_range { ltype = Uint32; value = 0x1_0000_0000 });
      rejects "float32 inexact" Float32 Float 0.1 (Inexact_float32 0.1);
      Alcotest.test_case "buffers grow" `Quick (fun () ->
          let ints = column Int16 Int and texts = column String Text in
          for i = 0 to 9999 do
            Support.get_ok (Column.add ints (i - 5000));
            Support.get_ok (Column.add texts (string_of_int i))
          done;
          for i = 0 to 9999 do
            Alcotest.(check int) "int" (i - 5000) (Column.get ints i);
            Alcotest.(check string)
              "text" (string_of_int i) (Column.get texts i)
          done);
      Alcotest.test_case "ragged batch is not sealed" `Quick (fun () ->
          let batch = Batch.create [ Int32; Int32 ] in
          (let view = borrow_ batch in
           let (Column.Any first) = Iarray.get (Batch.columns view) 0 in
           match Column.repr first with
           | Int -> Support.get_ok (Column.add first 1)
           | Bool | Int64 | Float | Text -> Alcotest.fail "unexpected column");
          Batch.commit_row (borrow_ batch);
          match Batch.seal batch with
          | Ok _ -> Alcotest.fail "ragged batch sealed"
          | Error (`Batch error) ->
              Alcotest.check
                (Support.testable Batch.sexp_of_error Batch.equal_error)
                "error"
                (Batch.Ragged { column = 1; length = 0; rows = 1 })
                error);
      Alcotest.test_case "writer flushes by byte limit" `Quick (fun () ->
          ignore ();
          let flushed = ref [] in
          let shipped = function
            | Ok () -> ()
            | Error (`Batch error) ->
                Alcotest.failf "%a"
                  Sexplib0.Sexp.pp_hum
                  (Batch.sexp_of_error error)
          in
          let writer =
            Batch_writer.create ~limit_bytes:64
              (Batch_pool.create ~batches:2)
              (module struct
                type error = [ `Batch of Batch.error ]

                let ship (batch : Batch.sealed @ unique) pool =
                  flushed := Batch.sealed_rows (borrow_ batch) :: !flushed;
                  Ok (Batch_pool.give pool (Batch.unseal batch))
              end)
              [ Int32; Int32 ]
          in
          let i = ref 0 in
          shipped
            (Batch_writer.fill writer (fun columns ->
                 match !i with
                 | 100 -> Ok Batch_writer.End
                 | _ ->
                     incr i;
                     for j = 0 to Iarray.length columns - 1 do
                       let (Column.Any c) = Iarray.get columns j in
                       match Column.repr c with
                       | Int ->
                           Support.get_ok
                             (Column.add c (if j = 0 then !i else - !i))
                       | Bool | Int64 | Float | Text ->
                           Alcotest.fail "unexpected column"
                     done;
                     Ok Row));
          Alcotest.(check int)
            "all rows flushed" 100
            (List.fold_left ( + ) 0 !flushed);
          Alcotest.(check bool)
            "split into several batches" true
            (List.length !flushed > 1));
      Alcotest.test_case "pool reshapes batches across tables" `Quick (fun () ->
          ignore ();
          let pool = Batch_pool.create ~batches:1 in
          let texts = ref [] in
          let writer ltypes =
            Batch_writer.create ~limit_bytes:(1 lsl 20) pool
              (module struct
                type error = Quiver_core.Error.t

                let ship (batch : Batch.sealed @ unique) pool =
                  (let view = borrow_ batch in
                   let columns = Batch.sealed_columns view in
                   for j = 0 to Iarray.length columns - 1 do
                     let (Column.Any c) = Iarray.get columns j in
                     match Column.repr c with
                     | Text ->
                         for row = 0 to Batch.sealed_rows view - 1 do
                           texts := (Column.get c row : string) :: !texts
                         done
                     | Bool | Int | Int64 | Float -> ()
                   done);
                  Ok (Batch_pool.give pool (Batch.unseal batch))
              end)
              ltypes
          in
          let fill writer values =
            let pending = ref values in
            Support.get_ok
              (Batch_writer.fill writer (fun columns ->
                   match !pending with
                   | [] -> Ok Batch_writer.End
                   | (value : string) :: rest ->
                       pending := rest;
                       for j = 0 to Iarray.length columns - 1 do
                         let (Column.Any c) = Iarray.get columns j in
                         match Column.repr c with
                         | Int -> Support.get_ok (Column.add c 7)
                         | Text -> Support.get_ok (Column.add c value)
                         | Bool | Int64 | Float ->
                             Alcotest.fail "unexpected column"
                       done;
                       Ok Row))
          in
          fill (writer [ Int32; Int32 ]) [ "a"; "b" ];
          fill (writer [ String; Int32 ]) [ "x"; "yz" ];
          fill (writer [ String ]) [ "q" ];
          Alcotest.(check (list string))
            "texts of every reshaped batch" [ "x"; "yz"; "q" ] (List.rev !texts));
    ]
