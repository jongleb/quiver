open Sexplib0.Sexp_conv
open Quiver_tikv

type field = Number of int64 | Text of string [@@deriving sexp_of]

let encode fields =
  let buffer = Buffer.create 64 in
  List.iter
    (fun (number, field) ->
      match field with Number n -> Proto.uint buffer number n | Text text -> Proto.bytes buffer number text)
    fields;
  Buffer.contents buffer

let read text =
  Result.map List.rev
    (Proto.fold text ~pos:0 ~len:(String.length text) ~init:[] ~f:(fun acc number value ->
         match value with
         | Proto.Varint n -> Ok ((number, Number n) :: acc)
         | Delimited { pos; len } -> Ok ((number, Text (String.sub text pos len)) :: acc)
         | Fixed64 _ | Fixed32 _ -> Error (`Proto (Proto.Unexpected { field = number; wire = 1 }))))

let gen =
  QCheck2.Gen.(
    let number = oneof_weighted [ (4, int_range 1 40); (1, int_range 1 ((1 lsl 29) - 1)) ] in
    let value =
      oneof
        [
          map (fun n -> Number n) (oneof [ int64; oneof_list [ 0L; 1L; 127L; 128L; -1L; Int64.max_int; Int64.min_int ] ]);
          map (fun text -> Text text) (string_size ~gen:char (int_range 0 300));
        ]
    in
    list_size (int_range 0 12) (pair number value))

let print fields = Base.Sexp.to_string ([%sexp_of: (int * field) list] fields)

let roundtrip =
  QCheck2.Test.make ~count:2000 ~name:"fields read back as written, in order" ~print gen (fun fields ->
      read (encode fields) = Ok fields)

let prefixes =
  QCheck2.Test.make ~count:500
    ~name:"a message cut anywhere reads as the whole fields before the cut, or as truncated" ~print gen
    (fun fields ->
      let text = encode fields in
      List.for_all
        (fun cut ->
          match read (String.sub text 0 cut) with
          | Ok read -> List.length read < List.length fields && read = Base.List.take fields (List.length read)
          | Error (`Proto Proto.Truncated) -> true
          | Error (`Proto _) -> false)
        (List.init (String.length text) Fun.id))

let fuzz =
  QCheck2.Test.make ~count:5000 ~name:"any bytes read or fail without an exception"
    QCheck2.Gen.(string_size ~gen:char (int_range 0 40))
    (fun text ->
      ignore (Proto.fold text ~pos:0 ~len:(String.length text) ~init:() ~f:(fun () _ _ -> Ok ()));
      true)

type event = Bytes_at of int * int * int | Other of int [@@deriving sexp_of]

let walked =
  let open QCheck2.Gen in
  let key field wire = Printf.sprintf "%c" (Char.chr ((field lsl 3) lor wire)) in
  let piece =
    oneof_weighted
      [
        (6, map encode (list_size (int_range 1 3) (pair (int_range 1 15) (map (fun n -> Number n) (oneof_list [ 0L; 1L; 127L; 128L; 0xFF_FFFF_FFFF_FFFFL ])))));
        (6, map encode (list_size (int_range 1 3) (pair (int_range 1 15) (map (fun text -> Text text) (string_size (int_range 0 20))))));
        (2, map2 (fun field text -> key field 1 ^ text) (int_range 1 15) (string_size (int_range 6 9)));
        (2, map2 (fun field text -> key field 5 ^ text) (int_range 1 15) (string_size (int_range 2 5)));
        (1, map2 (fun field wire -> key field wire) (int_range 0 15) (oneof_list [ 0; 2; 3; 4; 6; 7 ]));
        ( 2,
          map2
            (fun field length -> key field 2 ^ length)
            (int_range 1 15)
            (oneof_list
               [
                 "\x80\x80\x80\x80\x80\x80\x80\x80\x01";
                 "\xff\xff\xff\xff\xff\xff\xff\xff\x7f";
                 "\xff\xff\xff\xff\xff\xff\xff\x7f";
                 "\x80\x80\x80\x80\x80\x80\x80\x80\x80\x01";
               ]) );
        (1, map2 (fun field value -> key field 0 ^ value) (int_range 1 15) (oneof_list [ "\xff\xff\xff\xff\xff\xff\xff\xff\x7f"; "\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01"; "\x80" ]));
        (1, map2 (fun field n -> String.make 1 (Char.chr (0x80 lor ((field lsl 3) land 0x7F))) ^ String.make 1 (Char.chr n)) (int_range 0 15) (int_range 0 3));
      ]
  in
  let* text = map (String.concat "") (list_size (int_range 0 6) piece) in
  oneof_weighted [ (4, return text); (1, map (fun cut -> String.sub text 0 (cut mod (String.length text + 1))) nat) ]

let walks =
  QCheck2.Test.make ~count:5000 ~name:"a walk sees the fields a fold reads, or stops where a fold refuses or reads a long varint"
    ~print:(Printf.sprintf "%S") walked
    (fun text ->
      let seen = ref [] in
      let walk =
        Proto.walk text ~pos:0 ~len:(String.length text)
          ~delimited:(fun field pos len ->
            seen := Bytes_at (field, pos, len) :: !seen;
            Walked)
          ~scalar:(fun field ->
            seen := Other field :: !seen;
            Walked)
      in
      let read = ref [] in
      let folded =
        Proto.fold text ~pos:0 ~len:(String.length text) ~init:() ~f:(fun () field value ->
            let event = match value with Delimited { pos; len } -> Bytes_at (field, pos, len) | Varint _ | Fixed64 _ | Fixed32 _ -> Other field in
            read := event :: !read;
            Ok ())
      in
      let before = Base.List.is_prefix (List.rev !read) ~prefix:(List.rev !seen) ~equal:( = ) in
      let long = Base.String.is_substring text ~substring:"\x80\x80\x80\x80\x80\x80\x80\x80" || Base.String.is_substring text ~substring:"\xff\xff\xff\xff\xff\xff\xff\xff" in
      match (walk, folded) with
      | Walked, Ok () -> !read = !seen
      | Walked, Error _ -> false
      | Unusual, Ok () -> long && before
      | Unusual, Error _ -> before)

let known () =
  let check name expected actual = Alcotest.(check string) name expected actual in
  check "field 1 = 150" "\x08\x96\x01" (encode [ (1, Number 150L) ]);
  check "field 2 = testing" "\x12\x07testing" (encode [ (2, Text "testing") ]);
  check "-1 in ten bytes" "\x08\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01" (encode [ (1, Number (-1L)) ]);
  let fixed =
    Proto.fold "\x09\x01\x02\x03\x04\x05\x06\x07\x88\x15\xff\xff\xff\xfe" ~pos:0 ~len:14 ~init:[] ~f:(fun acc number value ->
        Ok ((number, value) :: acc))
  in
  Alcotest.(check bool) "fixed64 and fixed32 little-endian" true
    (fixed = Ok [ (2, Proto.Fixed32 (-16777217l)); (1, Fixed64 (-8644934341102468607L)) ]);
  let refused name expected text = Alcotest.(check bool) name true (read text = Error (`Proto expected)) in
  refused "eleven-byte varint" Overlong_varint "\x08\x80\x80\x80\x80\x80\x80\x80\x80\x80\x80\x01";
  refused "tenth byte above 1" Overlong_varint "\x08\x80\x80\x80\x80\x80\x80\x80\x80\x80\x02";
  refused "group wire type" (Unknown_wire 3) "\x0b";
  refused "field number 0" Field_zero "\x00\x01";
  refused "length past the end" Truncated "\x12\x05abc";
  Alcotest.(check bool) "a slice of a longer text" true
    (Proto.fold "xx\x08\x01yy" ~pos:2 ~len:2 ~init:[] ~f:(fun acc number value -> Ok ((number, value) :: acc))
    = Ok [ (1, Proto.Varint 1L) ])

let packed =
  QCheck2.Test.make ~count:2000 ~name:"numbers packed in one field read back, a cut one is truncated"
    QCheck2.Gen.(list_size (int_range 0 20) (oneof [ int64; oneof_list [ 0L; 127L; 128L; -1L ] ]))
    (fun numbers ->
      let inner = Buffer.create 64 in
      let each = Buffer.create 64 in
      List.iter
        (fun n ->
          Buffer.clear each;
          Proto.uint each 1 n;
          Buffer.add_string inner (String.sub (Buffer.contents each) 1 (Buffer.length each - 1)))
        numbers;
      let text = encode [ (3, Text (Buffer.contents inner)) ] in
      let field text = Proto.fold text ~pos:0 ~len:(String.length text) ~init:(Ok []) ~f:(fun _ number value -> Ok (Proto.numbers text number value)) in
      let whole = field text = Ok (Ok numbers) in
      let cut =
        match numbers with
        | [] -> true
        | _ :: _ ->
            let inner = Buffer.contents inner in
            let last = String.length inner - 1 in
            let shortened = encode [ (3, Text (String.sub inner 0 last ^ String.make 1 (Char.chr (Char.code inner.[last] lor 0x80)))) ] in
            field shortened = Ok (Error (`Proto Proto.Truncated)) || field shortened = Ok (Error (`Proto Proto.Overlong_varint))
      in
      whole && cut && Proto.numbers "" 3 (Proto.Varint 9L) = Ok [ 9L ])

let sizes =
  QCheck2.Test.make ~count:2000 ~name:"a field's size is what it writes, a delimited start plus its bytes is the field"
    QCheck2.Gen.(triple (int_range 1 ((1 lsl 29) - 1)) int64 (string_size (int_range 0 300)))
    (fun (field, number, text) ->
      let written fill =
        let buffer = Buffer.create 16 in
        fill buffer;
        Buffer.contents buffer
      in
      let whole = written (fun buffer -> Proto.bytes buffer field text) in
      String.length (written (fun buffer -> Proto.uint buffer field number)) = Proto.uint_size field number
      && String.length whole = Proto.delimited_size field (String.length text)
      && written (fun buffer -> Proto.delimited buffer field (String.length text); Buffer.add_string buffer text) = whole)

let tests =
  Alcotest.test_case "known encodings and refusals" `Quick known
  :: List.map QCheck_alcotest.to_alcotest [ roundtrip; prefixes; fuzz; packed; sizes; walks ]
