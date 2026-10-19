open Base.Result.Let_syntax

let frames ?(first = 0) payloads =
  let out = Buffer.create 1024 and sequence = ref first in
  let frame text =
    let length = String.length text in
    Buffer.add_char out (Char.chr (length land 0xFF));
    Buffer.add_char out (Char.chr ((length lsr 8) land 0xFF));
    Buffer.add_char out (Char.chr ((length lsr 16) land 0xFF));
    Buffer.add_char out (Char.chr (!sequence land 0xFF));
    Buffer.add_string out text;
    incr sequence
  in
  List.iter
    (fun payload ->
      let rec split offset =
        let rest = String.length payload - offset in
        if rest >= 0xFFFFFF then (
          frame (String.sub payload offset 0xFFFFFF);
          split (offset + 0xFFFFFF))
        else frame (String.sub payload offset rest)
      in
      split 0)
    payloads;
  Buffer.contents out

let source wire pieces =
  let offset = ref 0 and turn = ref 0 in
  fun buffer ~off ~len ->
    let piece =
      match pieces with
      | [] -> len
      | _ ->
          incr turn;
          List.nth pieces (!turn mod List.length pieces)
    in
    let count = Int.min len (Int.min piece (String.length wire - !offset)) in
    Base_bigstring.From_string.blit ~src:wire ~src_pos:!offset ~dst:buffer
      ~dst_pos:off ~len:count;
    offset := !offset + count;
    count

let read_all reader count =
  let cursor = Quiver_mysql.Wire.Cursor.create () in
  List.init count (fun _ ->
      match
        let%bind () = Quiver_mysql.Wire.next reader cursor in
        Quiver_mysql.Wire.Cursor.decode cursor (fun r ->
            Ok (Quiver_mysql.Wire.Cursor.rest r))
      with
      | Ok payload -> (payload, Quiver_mysql.Wire.Cursor.sequence cursor)
      | Error (`Wire error) ->
          Alcotest.failf "%a"
            Sexplib0.Sexp.pp_hum
            (Quiver_mysql.Wire.sexp_of_error error))

let payloads =
  QCheck2.Gen.(
    list_size (int_range 1 20)
      (string_size ~gen:char
         (oneof [ int_range 0 300; int_range 0 70_000; pure 0 ])))

let tests =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:300 ~name:"packets survive any split into reads"
         QCheck2.Gen.(
           pair payloads (list_size (int_range 0 8) (int_range 1 9000)))
         (fun (payloads, pieces) ->
           let reader =
             Quiver_mysql.Wire.create (source (frames payloads) pieces)
           in
           List.equal
             (fun (expected, sequence) (got, got_sequence) ->
               String.equal expected got && sequence = got_sequence)
             (List.mapi (fun i payload -> (payload, i land 0xFF)) payloads)
             (read_all reader (List.length payloads))));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:200 ~name:"frame writes what next reads"
         payloads (fun payloads ->
           let writer = Quiver_mysql.Wire.create (fun _ ~off:_ ~len:_ -> 0) in
           let wire =
             String.concat ""
               (List.map (Quiver_mysql.Wire.frame writer) payloads)
           in
           List.equal String.equal payloads
             (List.map fst
                (read_all
                   (Quiver_mysql.Wire.create (source wire [ 4096 ]))
                   (List.length payloads)))));
    Alcotest.test_case "packets of 16 MiB and more are joined" `Quick (fun () ->
        let big n = String.init n (fun i -> Char.chr (i land 0xFF)) in
        let payloads =
          [
            big 0xFFFFFF; "after"; big (0xFFFFFF + 1); big ((2 * 0xFFFFFF) + 5);
          ]
        in
        let reader =
          Quiver_mysql.Wire.create (source (frames payloads) [ 1 lsl 20; 7 ])
        in
        let got = read_all reader (List.length payloads) in
        let writer = Quiver_mysql.Wire.create (fun _ ~off:_ ~len:_ -> 0) in
        Alcotest.(check bool)
          "frame splits like the protocol" true
          (String.equal (frames payloads)
             (String.concat ""
                (List.map (Quiver_mysql.Wire.frame writer) payloads)));
        Alcotest.(check (list int))
          "lengths"
          (List.map String.length payloads)
          (List.map (fun (text, _) -> String.length text) got);
        Alcotest.(check bool)
          "contents" true
          (List.for_all2 (fun a (b, _) -> String.equal a b) payloads got);
        Alcotest.(check (list int))
          "first frame sequence of each packet" [ 0; 2; 3; 5 ]
          (List.map snd got));
    Alcotest.test_case "wrong sequence and early close are errors" `Quick
      (fun () ->
        let next wire =
          match
            Quiver_mysql.Wire.next
              (Quiver_mysql.Wire.create (source wire []))
              (Quiver_mysql.Wire.Cursor.create ())
          with
          | Ok _ -> "ok"
          | Error (`Wire error) ->
              Sexplib0.Sexp.to_string_hum
                (Quiver_mysql.Wire.sexp_of_error error)
        in
        Alcotest.(check string)
          "sequence" "(Out_of_order (expected 0) (got 3))"
          (next (frames ~first:3 [ "x" ]));
        Alcotest.(check string)
          "closed" "Closed"
          (next (String.sub (frames [ "hello" ]) 0 6)));
    Alcotest.test_case "cursor reads fields and rejects short packets" `Quick
      (fun () ->
        let reader =
          Quiver_mysql.Wire.create
            (source
               (frames
                  [
                    "\x05\xfc\x34\x12\xfd\x56\x34\x12\xfe\x01\x00\x00\x00\x00\x00\x00\x00\x03abcroot\x00\xfa";
                    "\xfc\x01";
                  ])
               [])
        and cursor = Quiver_mysql.Wire.Cursor.create () in
        let decode f =
          match Quiver_mysql.Wire.next reader cursor with
          | Error (`Wire error) ->
              Alcotest.failf "%a"
                Sexplib0.Sexp.pp_hum
                (Quiver_mysql.Wire.sexp_of_error error)
          | Ok () -> Quiver_mysql.Wire.Cursor.decode cursor (fun r -> Ok (f r))
        in
        (match
           decode (fun r ->
               let open Quiver_mysql.Wire.Cursor in
               let a = lenenc_int r in
               let b = lenenc_int r in
               let c = lenenc_int r in
               let d = lenenc_int r in
               let text = lenenc_string r in
               let name = null_terminated r in
               let last = uint8 r in
               (a, b, c, d, text, name, last, remaining r))
         with
        | Ok (a, b, c, d, text, name, last, remaining) ->
            Alcotest.(check (list int))
              "lenenc ints" [ 5; 0x1234; 0x123456; 1 ] [ a; b; c; d ];
            Alcotest.(check (pair string string))
              "strings" ("abc", "root") (text, name);
            Alcotest.(check (pair int int)) "tail" (0xfa, 0) (last, remaining)
        | Error (`Wire error) ->
            Alcotest.failf "%a"
              Sexplib0.Sexp.pp_hum
              (Quiver_mysql.Wire.sexp_of_error error));
        Alcotest.(check bool)
          "short lenenc" true
          (match decode Quiver_mysql.Wire.Cursor.lenenc_int with
          | Error (`Wire Quiver_mysql.Wire.Malformed) -> true
          | Ok _ | Error (`Wire _) -> false));
  ]
