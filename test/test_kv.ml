open Sexplib0.Sexp_conv
open Quiver_tikv

let message fill =
  let buffer = Buffer.create 64 in
  fill buffer;
  Buffer.contents buffer

let plain header : Test_hpack.choice = { header; kind = Without; name_from = Literal_name; name_coding = Raw; value_coding = Raw }

let framed text =
  let length = String.length text in
  String.init 5 (fun i -> Char.chr (if i = 0 then 0 else (length lsr (8 * (4 - i))) land 0xFF)) ^ text

let reply ?(status = 200) ?(content_type = Some "application/grpc") ?trailers body : Test_http2.reply =
  {
    opening = Direct;
    status;
    fields = List.map plain (Option.fold ~none:[] ~some:(fun kind -> [ ("content-type", kind) ]) content_type);
    fragments = 1;
    marks = Bare;
    pieces = (if body = "" then [] else [ { noise = []; text = body; padding = None } ]);
    trailers = Option.map (List.map plain) trailers;
  }

let grpc () =
  let outcome reply =
    let scenario : Test_http2.scenario =
      { initial_window = 65_535; max_frame = 16_384; exchanges = [ { path = "/p"; headers = []; body = framed "ask"; reply } ] }
    in
    let result, heard, _ = Test_http2.session scenario (fun connection -> Grpc.call connection ~path:"/p" "ask") in
    Alcotest.(check (list string)) "faults" [] heard.faults;
    Alcotest.(check (list string)) "the request" [ framed "ask" ] (List.map snd heard.requests);
    result
  in
  let check name expected reply =
    Alcotest.(check bool) name true
      (match (outcome reply, expected) with
      | Ok got, `Message text -> got = text
      | Error (`Grpc error), `Refused expected -> error = expected
      | Ok _, _ | Error _, _ -> false)
  in
  check "a message" (`Message "answer") (reply ~trailers:[ ("grpc-status", "0") ] (framed "answer"));
  check "an empty message" (`Message "") (reply ~trailers:[ ("grpc-status", "0") ] (framed ""));
  check "a status with its message decoded" (`Refused (Grpc.Status { code = 14; message = "not leader: 100%" }))
    (reply ~trailers:[ ("grpc-status", "14"); ("grpc-message", "not%20leader%3A 100%") ] "");
  check "trailers only" (`Refused (Grpc.Status { code = 12; message = "" }))
    { (reply "") with fields = List.map plain [ ("content-type", "application/grpc"); ("grpc-status", "12") ] };
  check "no status" (`Refused Grpc.No_status) (reply ~trailers:[] (framed "x"));
  check "another HTTP status" (`Refused (Grpc.Http 404)) (reply ~status:404 ~trailers:[ ("grpc-status", "0") ] "");
  check "not gRPC" (`Refused (Grpc.Not_grpc (Some "text/html"))) (reply ~content_type:(Some "text/html") ~trailers:[ ("grpc-status", "0") ] "");
  check "a gRPC subtype" (`Message "y") (reply ~content_type:(Some "application/grpc+proto") ~trailers:[ ("grpc-status", "0") ] (framed "y"));
  check "compressed" (`Refused Grpc.Compressed) (reply ~trailers:[ ("grpc-status", "0") ] ("\x01" ^ String.sub (framed "z") 1 5));
  check "two messages" (`Refused (Grpc.Framing 12)) (reply ~trailers:[ ("grpc-status", "0") ] (framed "a" ^ framed "b"));
  check "a short body" (`Refused (Grpc.Framing 3)) (reply ~trailers:[ ("grpc-status", "0") ] "\x00\x00\x00")

let region : Kv.region = { id = 7L; conf_ver = 3L; version = 11L; peer = 9L; store = 2L }

let fields text =
  match Proto.fold text ~pos:0 ~len:(String.length text) ~init:[] ~f:(fun acc field value -> Ok ((field, value) :: acc)) with
  | Ok fields -> List.rev fields
  | Error (`Proto _) -> Alcotest.fail "not protobuf"

let sub text (value : Proto.value) =
  match value with Delimited { pos; len } -> String.sub text pos len | Varint _ | Fixed64 _ | Fixed32 _ -> Alcotest.fail "not delimited"

let request () =
  let text = Kv.scan_request region ~direction:Forward ~start:"t\x80a" ~stop:(Before "t\x80b") ~version:449_000_000_000_000_000L ~limit:256 in
  let top = fields text in
  let context = sub text (List.assoc 1 top) in
  let context_fields = fields context in
  let numbers text = List.filter_map (fun (field, (value : Proto.value)) -> match value with Varint n -> Some (field, n) | _ -> None) (fields text) in
  Alcotest.(check (list (pair int int64))) "region, priority low, not filling the cache" [ (1, 7L); (6, 1L); (8, 1L) ] (numbers context);
  Alcotest.(check (list (pair int int64))) "epoch" [ (1, 3L); (2, 11L) ] (numbers (sub context (List.assoc 2 context_fields)));
  Alcotest.(check (list (pair int int64))) "peer" [ (1, 9L); (2, 2L) ] (numbers (sub context (List.assoc 3 context_fields)));
  Alcotest.(check string) "source" "quiver" (sub context (List.assoc 24 context_fields));
  Alcotest.(check string) "start" "t\x80a" (sub text (List.assoc 2 top));
  Alcotest.(check string) "end" "t\x80b" (sub text (List.assoc 7 top));
  Alcotest.(check (list (pair int int64))) "limit and version" [ (3, 256L); (4, 449_000_000_000_000_000L) ] (numbers text);
  let backward = Kv.scan_request region ~direction:Backward ~start:"t\x80a" ~stop:(Before "t\x80b") ~version:9L ~limit:1 in
  let top = fields backward in
  Alcotest.(check (pair string string)) "backward: from the stop down to the start" ("t\x80b", "t\x80a")
    (sub backward (List.assoc 2 top), sub backward (List.assoc 7 top));
  Alcotest.(check (list (pair int int64))) "backward: limit, version, reverse" [ (3, 1L); (4, 9L); (6, 1L) ] (numbers backward)

let pair_message ?error (pair : Kv.pair) =
  message (fun buffer ->
      Option.iter (Proto.bytes buffer 1) error;
      Proto.bytes buffer 2 pair.key;
      Proto.bytes buffer 3 pair.value;
      Proto.uint buffer 4 5L)

let response ?region_error ?error pairs =
  message (fun buffer ->
      Option.iter (Proto.bytes buffer 1) region_error;
      List.iter (fun pair -> Proto.bytes buffer 2 (pair_message pair)) pairs;
      Option.iter (Proto.bytes buffer 3) error)

let decoded () =
  let check name expected text =
    Alcotest.(check string) name (Base.Sexp.to_string (Kv.sexp_of_scanned expected))
      (match Kv.scanned text with Ok got -> Base.Sexp.to_string (Kv.sexp_of_scanned got) | Error (`Proto _) -> "not protobuf")
  in
  let peer = message (fun buffer -> Proto.uint buffer 1 4L; Proto.uint buffer 2 8L) in
  let inner field fill = message (fun buffer -> Proto.bytes buffer 1 "why"; Proto.bytes buffer field (message fill)) in
  check "not leader, with the leader" (Region_error (Not_leader (Some { id = 4L; store = 8L })))
    (response ~region_error:(inner 2 (fun buffer -> Proto.uint buffer 1 7L; Proto.bytes buffer 2 peer)) []);
  check "not leader, leader unknown" (Region_error (Not_leader None)) (response ~region_error:(inner 2 (fun buffer -> Proto.uint buffer 1 7L)) []);
  check "region not found" (Region_error Region_not_found) (response ~region_error:(inner 3 (fun buffer -> Proto.uint buffer 1 7L)) []);
  check "epoch not match" (Region_error Epoch_not_match) (response ~region_error:(inner 5 ignore) []);
  check "server busy" (Region_error (Server_is_busy { reason = "write stall"; backoff_ms = 20L }))
    (response ~region_error:(inner 6 (fun buffer -> Proto.bytes buffer 1 "write stall"; Proto.uint buffer 2 20L)) []);
  check "disk full, by its number" (Region_error (Other { kind = 15; message = "why" })) (response ~region_error:(inner 15 ignore) []);
  check "a message alone" (Region_error (Other { kind = 0; message = "m" }))
    (response ~region_error:(message (fun buffer -> Proto.bytes buffer 1 "m"; Proto.uint buffer 30 1L)) []);
  let lock = message (fun buffer -> Proto.bytes buffer 1 "p"; Proto.uint buffer 2 99L; Proto.bytes buffer 3 "k"; Proto.uint buffer 4 3000L) in
  check "locked" (Key_error (Locked { primary = "p"; version = 99L; key = "k"; ttl = 3000L }))
    (response ~error:(message (fun buffer -> Proto.bytes buffer 1 lock)) []);
  check "retryable" (Key_error (Retryable "again")) (response ~error:(message (fun buffer -> Proto.bytes buffer 2 "again")) []);
  check "abort" (Key_error (Abort "no")) (response ~error:(message (fun buffer -> Proto.bytes buffer 3 "no")) []);
  check "a pair's error" (Key_error (Locked { primary = "p"; version = 99L; key = "k"; ttl = 3000L }))
    (message (fun buffer ->
         Proto.bytes buffer 2 (pair_message { key = "a"; value = "1" });
         Proto.bytes buffer 2 (pair_message ~error:(message (fun buffer -> Proto.bytes buffer 1 lock)) { key = "b"; value = "" })));
  check "the region error first" (Region_error Epoch_not_match)
    (response ~region_error:(inner 5 ignore) ~error:(message (fun buffer -> Proto.bytes buffer 2 "again")) [ { key = "a"; value = "1" } ]);
  Alcotest.(check bool) "a key of the wrong wire type" true
    (Kv.scanned (message (fun buffer -> Proto.bytes buffer 2 (message (fun buffer -> Proto.uint buffer 2 1L))))
    = Error (`Proto (Proto.Unexpected { field = 2; wire = 0 })))

let pairs =
  QCheck2.Test.make ~count:1000 ~name:"scanned pairs come back in order, keys and values exact"
    QCheck2.Gen.(list_size (int_range 0 50) (pair (string_size ~gen:char (int_range 0 40)) (string_size ~gen:char (int_range 0 400))))
    (fun pairs ->
      let pairs = List.map (fun (key, value) -> { Kv.key; value }) pairs in
      match Kv.scanned (response pairs) with Ok (Pairs got) -> got = pairs | Ok _ | Error _ -> false)

let reference text =
  let open Base.Result.Let_syntax in
  let fields (pos, len) ~init ~f = Proto.fold text ~pos ~len ~init ~f in
  let key_error slice : (Kv.key_error, _) result =
    let%bind first =
      fields slice ~init:None ~f:(fun first field value -> match first with Some _ -> Ok first | None -> Ok (Some (field, value)))
    in
    match first with
    | None -> Ok (Kv.Other_key_error 0)
    | Some (1, value) ->
        let%bind slice = Proto.slice 1 value in
        Result.map
          (fun lock -> Kv.Locked lock)
          (fields slice ~init:{ Kv.primary = ""; version = 0L; key = ""; ttl = 0L } ~f:(fun lock field value ->
               match field with
               | 1 -> Result.map (fun primary -> { lock with primary }) (Proto.contents text field value)
               | 2 -> Result.map (fun version -> { lock with version }) (Proto.number field value)
               | 3 -> Result.map (fun key -> { lock with key }) (Proto.contents text field value)
               | 4 -> Result.map (fun ttl -> { lock with ttl }) (Proto.number field value)
               | _ -> Ok lock))
    | Some (2, value) -> Result.map (fun reason -> Kv.Retryable reason) (Proto.contents text 2 value)
    | Some (3, value) -> Result.map (fun reason -> Kv.Abort reason) (Proto.contents text 3 value)
    | Some (field, _) -> Ok (Kv.Other_key_error field)
  in
  let%bind region, pairs, error =
    Proto.fold text ~pos:0 ~len:(String.length text) ~init:(None, [], None) ~f:(fun (region, pairs, error) field value ->
        match field with
        | 1 -> Result.map (fun slice -> (Some slice, pairs, error)) (Proto.slice field value)
        | 2 ->
            let%bind slice = Proto.slice field value in
            let%bind pair =
              fields slice ~init:(`Pair ("", "")) ~f:(fun pair field value ->
                  match (pair, field) with
                  | `Pair (_, value_bytes), 2 -> Result.map (fun key -> `Pair (key, value_bytes)) (Proto.contents text field value)
                  | `Pair (key, _), 3 -> Result.map (fun value_bytes -> `Pair (key, value_bytes)) (Proto.contents text field value)
                  | _, 1 -> Result.map (fun slice -> `Failed slice) (Proto.slice field value)
                  | _ -> Ok pair)
            in
            Ok (region, pair :: pairs, error)
        | 3 -> Result.map (fun slice -> (region, pairs, Some slice)) (Proto.slice field value)
        | _ -> Ok (region, pairs, error))
  in
  match (region, error) with
  | Some (pos, len), _ -> Result.map (fun error -> Kv.Region_error error) (Kv.region_error text ~pos ~len)
  | None, Some slice -> Result.map (fun error -> Kv.Key_error error) (key_error slice)
  | None, None ->
      let rec collect acc = function
        | [] -> Ok (Kv.Pairs acc)
        | `Pair (key, value) :: rest -> collect ({ Kv.key; value } :: acc) rest
        | `Failed slice :: _ -> Result.map (fun error -> Kv.Key_error error) (key_error slice)
      in
      collect [] pairs

let rec raw_varint buffer n =
  if n >= 0 && n < 0x80 then Buffer.add_char buffer (Char.chr n)
  else (
    Buffer.add_char buffer (Char.chr ((n land 0x7F) lor 0x80));
    raw_varint buffer (n lsr 7))

let messages =
  let open QCheck2.Gen in
  let field = oneof_weighted [ (6, int_range 1 4); (1, return 0); (1, return 9); (1, return 300); (1, return (1 lsl 29)) ] in
  let number = oneof_list [ 0L; 1L; 127L; 128L; 0xFF_FFFF_FFFF_FFFFL; 0x100_0000_0000_0000L; Int64.max_int; -1L ] in
  let bytes = string_size ~gen:(oneof_list [ 'a'; '\x00'; '\x80'; '\xff'; '\x12' ]) (int_range 0 12) in
  let message_of entries =
    let buffer = Buffer.create 64 in
    List.iter (fun write -> write buffer) entries;
    Buffer.contents buffer
  in
  let rec message depth =
    map message_of
      (list_size (int_range 0 5)
         (oneof_weighted
            ([
               (4, map2 (fun field text buffer -> Proto.bytes buffer field text) field bytes);
               (2, map2 (fun field n buffer -> Proto.uint buffer field n) field number);
               (1, map2 (fun field text buffer -> raw_varint buffer ((field lsl 3) lor 1); Buffer.add_string buffer text) field bytes);
               (1, map2 (fun field text buffer -> raw_varint buffer ((field lsl 3) lor 5); Buffer.add_string buffer text) field bytes);
               (1, map2 (fun field wire buffer -> raw_varint buffer ((field lsl 3) lor wire)) field (oneof_list [ 3; 4; 6; 7 ]));
               (1, map (fun text buffer -> Buffer.add_string buffer text) bytes);
               (1, map (fun field buffer -> Buffer.add_string buffer "\x80\x80\x80\x80\x80\x80\x80\x80\x80\x00"; ignore field) field);
             ]
            @ if depth = 0 then [] else [ (5, map2 (fun field inner buffer -> Proto.bytes buffer field inner) field (message (depth - 1))) ])))
  in
  let plausible =
    let entry =
      oneof_weighted
        [
          (60, map2 (fun key value buffer -> Proto.bytes buffer 2 key; Proto.bytes buffer 3 value) bytes bytes);
          (5, map (fun value buffer -> Proto.bytes buffer 3 value) bytes);
          (5, map (fun key buffer -> Proto.bytes buffer 2 key) bytes);
          (5, map (fun n buffer -> Proto.uint buffer 4 n) number);
          (5, map2 (fun field text buffer -> Proto.bytes buffer field text) (int_range 4 20) bytes);
          (1, map (fun n buffer -> Proto.uint buffer 2 n) number);
          (1, map (fun text buffer -> Proto.bytes buffer 1 text) bytes);
          (1, map (fun field buffer -> raw_varint buffer ((field lsl 3) lor 1); Buffer.add_string buffer "12345678") (int_range 1 5));
        ]
    in
    let pair = map message_of (list_size (int_range 0 4) entry) in
    map message_of
      (list_size (int_range 0 30)
         (oneof_weighted
            [
              (300, map (fun pair buffer -> Proto.bytes buffer 2 pair) pair);
              (1, map (fun n buffer -> Proto.uint buffer 6 n) number);
              (1, map (fun text buffer -> Proto.bytes buffer 1 text) bytes);
              (1, map (fun text buffer -> Proto.bytes buffer 3 text) bytes);
              (1, map (fun n buffer -> Proto.uint buffer 2 n) number);
            ]))
  in
  let* text = oneof [ message 2; plausible ] in
  oneof_weighted [ (6, return text); (1, map (fun cut -> String.sub text 0 (cut mod (String.length text + 1))) nat) ]

let fast_path =
  QCheck2.Test.make ~count:5000 ~name:"scan responses read without allocating as by folding every field"
    ~print:(fun text -> Printf.sprintf "%S" text)
    messages
    (fun text ->
      let show result =
        match result with Ok scanned -> Base.Sexp.to_string (Kv.sexp_of_scanned scanned) | Error (`Proto error) -> Base.Sexp.to_string (Proto.sexp_of_error error)
      in
      show (Kv.scanned text) = show (reference text))

let scan () =
  let pairs = List.init 3000 (fun i -> { Kv.key = Printf.sprintf "t\x80%08d" i; value = String.make (i mod 300) 'v' }) in
  let request = Kv.scan_request region ~direction:Forward ~start:"t\x80" ~stop:(Before "t\x81") ~version:42L ~limit:5000 in
  let reply : Test_http2.reply = { (reply ~trailers:[ ("grpc-status", "0") ] (framed (response pairs))) with fragments = 2 } in
  let scenario : Test_http2.scenario =
    { initial_window = 65_535; max_frame = 16_384; exchanges = [ { path = "/tikvpb.Tikv/KvScan"; headers = []; body = framed request; reply } ] }
  in
  let result, heard, _ =
    Test_http2.session scenario (fun connection -> Kv.scan connection region ~direction:Forward ~start:"t\x80" ~stop:(Before "t\x81") ~version:42L ~limit:5000)
  in
  Alcotest.(check (list string)) "faults" [] heard.faults;
  Alcotest.(check bool) "the request as gRPC asks"
    true
    (match heard.requests with
    | [ (headers, body) ] ->
        body = framed request
        && List.assoc_opt ":path" headers = Some "/tikvpb.Tikv/KvScan"
        && List.assoc_opt "content-type" headers = Some "application/grpc"
        && List.assoc_opt "te" headers = Some "trailers"
    | _ -> false);
  Alcotest.(check bool) "every pair" true (match result with Ok (Pairs got) -> got = pairs | Ok _ | Error _ -> false)

let splits () =
  let text = Kv.split_request region [ "t\x80a"; "t\x80b" ] in
  let top = fields text in
  let context = sub text (List.assoc 1 top) in
  Alcotest.(check (list (pair int int64))) "the region's context" [ (1, 7L) ]
    (List.filter_map (fun (field, (value : Proto.value)) -> match value with Varint n -> Some (field, n) | _ -> None) (fields context));
  Alcotest.(check (list string)) "the keys, raw, in order" [ "t\x80a"; "t\x80b" ]
    (List.filter_map (fun (field, value) -> if field = 3 then Some (sub text value) else None) top);
  let regions ids = message (fun buffer -> List.iter (fun id -> Proto.bytes buffer 4 (message (fun buffer -> Proto.uint buffer 1 id; Proto.bytes buffer 2 "x"))) ids) in
  Alcotest.(check bool) "every region it became" true (Kv.split_response (regions [ 7L; 31L; 32L ]) = Ok (Split [ 7L; 31L; 32L ]));
  Alcotest.(check bool) "refused" true
    (Kv.split_response (message (fun buffer -> Proto.bytes buffer 1 (message (fun buffer -> Proto.bytes buffer 1 "e"; Proto.bytes buffer 5 ""))))
    = Ok (Split_refused Epoch_not_match))

let tests =
  [
    Alcotest.test_case "split requests and responses" `Quick splits;
    Alcotest.test_case "gRPC calls and their failures" `Quick grpc;
    Alcotest.test_case "a scan request carries the region, range and version" `Quick request;
    Alcotest.test_case "scan responses: pairs, region errors, key errors" `Quick decoded;
    Alcotest.test_case "a scan over HTTP/2 brings every pair" `Quick scan;
    QCheck_alcotest.to_alcotest pairs;
    QCheck_alcotest.to_alcotest fast_path;
  ]
