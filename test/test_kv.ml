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
  let settled : Kv.settled = { ignored = [ 5L; 6L ]; committed = [ 7L ] } in
  let text = Kv.scan_request region settled ~direction:Forward ~start:"t\x80a" ~stop:(Before "t\x80b") ~version:449_000_000_000_000_000L ~limit:256 in
  let top = fields text in
  let context = sub text (List.assoc 1 top) in
  let context_fields = fields context in
  let numbers text = List.filter_map (fun (field, (value : Proto.value)) -> match value with Varint n -> Some (field, n) | _ -> None) (fields text) in
  Alcotest.(check (list (pair int int64))) "region, priority low, not filling the cache, the settled transactions"
    [ (1, 7L); (6, 1L); (8, 1L); (13, 5L); (13, 6L); (22, 7L) ] (numbers context);
  Alcotest.(check (list (pair int int64))) "epoch" [ (1, 3L); (2, 11L) ] (numbers (sub context (List.assoc 2 context_fields)));
  Alcotest.(check (list (pair int int64))) "peer" [ (1, 9L); (2, 2L) ] (numbers (sub context (List.assoc 3 context_fields)));
  Alcotest.(check string) "source" "quiver" (sub context (List.assoc 24 context_fields));
  Alcotest.(check string) "start" "t\x80a" (sub text (List.assoc 2 top));
  Alcotest.(check string) "end" "t\x80b" (sub text (List.assoc 7 top));
  Alcotest.(check (list (pair int int64))) "limit and version" [ (3, 256L); (4, 449_000_000_000_000_000L) ] (numbers text);
  let backward = Kv.scan_request region Kv.nothing_settled ~direction:Backward ~start:"t\x80a" ~stop:(Before "t\x80b") ~version:9L ~limit:1 in
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

let show_scan reply = Base.Sexp.to_string ([%sexp_of: Kv.scan Kv.reply] reply)

let lock_message ?(async = false) ?(min_commit = 0L) ?(secondaries = []) ?(primary = "p") ?(version = 99L) key =
  message (fun buffer ->
      Proto.bytes buffer 1 primary;
      Proto.uint buffer 2 version;
      Proto.bytes buffer 3 key;
      Proto.uint buffer 4 3000L;
      Proto.uint buffer 6 0L;
      if async then Proto.uint buffer 8 1L;
      if not (Int64.equal min_commit 0L) then Proto.uint buffer 9 min_commit;
      List.iter (Proto.bytes buffer 10) secondaries)

let locked_by ?(key = "k") () : Kv.lock = { primary = "p"; version = 99L; key; ttl = 3000L; min_commit_ts = 0L; protocol = Two_phase }

let decoded () =
  let check name expected text =
    Alcotest.(check string) name (show_scan expected)
      (match Kv.scanned text with Ok got -> show_scan got | Error (`Proto _) -> "not protobuf")
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
  let lock = lock_message "k" in
  let failing lock = message (fun buffer -> Proto.bytes buffer 1 lock) in
  check "locked, for the whole scan" (Reply (Locks (locked_by (), []))) (response ~error:(failing lock) []);
  check "retryable" (Key_error (Retryable "again")) (response ~error:(message (fun buffer -> Proto.bytes buffer 2 "again")) []);
  check "abort" (Key_error (Abort "no")) (response ~error:(message (fun buffer -> Proto.bytes buffer 3 "no")) []);
  check "a pair's lock" (Reply (Locks (locked_by (), [])))
    (message (fun buffer ->
         Proto.bytes buffer 2 (pair_message { key = "a"; value = "1" });
         Proto.bytes buffer 2 (pair_message ~error:(failing lock) { key = "b"; value = "" })));
  check "every pair's lock, in key order" (Reply (Locks (locked_by ~key:"b" (), [ locked_by ~key:"d" () ])))
    (message (fun buffer ->
         Proto.bytes buffer 2 (pair_message { key = "a"; value = "1" });
         Proto.bytes buffer 2 (pair_message ~error:(failing (lock_message "b")) { key = "b"; value = "" });
         Proto.bytes buffer 2 (pair_message { key = "c"; value = "1" });
         Proto.bytes buffer 2 (pair_message ~error:(failing (lock_message "d")) { key = "d"; value = "" })));
  check "a pair's error other than a lock" (Key_error (Abort "no"))
    (message (fun buffer ->
         Proto.bytes buffer 2 (pair_message ~error:(failing (lock_message "b")) { key = "b"; value = "" });
         Proto.bytes buffer 2 (pair_message ~error:(message (fun buffer -> Proto.bytes buffer 3 "no")) { key = "c"; value = "" })));
  check "an async-commit primary lock" (Reply (Locks ({ (locked_by ()) with min_commit_ts = 77L; protocol = Async_commit [ "s1"; "s2" ] }, [])))
    (response ~error:(failing (lock_message ~async:true ~min_commit:77L ~secondaries:[ "s1"; "s2" ] "k")) []);
  check "secondaries of a lock not async are not kept" (Reply (Locks (locked_by (), [])))
    (response ~error:(failing (lock_message ~secondaries:[ "s1" ] "k")) []);
  check "the transaction not found" (Key_error Txn_not_found) (response ~error:(message (fun buffer -> Proto.bytes buffer 8 "")) []);
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
      match Kv.scanned (response pairs) with Ok (Reply (Pairs got)) -> got = pairs | Ok _ | Error _ -> false)

let reference text =
  let open Base.Result.Let_syntax in
  let fields (pos, len) ~init ~f = Proto.fold text ~pos ~len ~init ~f in
  let lock slice : (Kv.lock, _) result =
    let%bind lock, async, secondaries =
      fields slice ~init:({ Kv.primary = ""; version = 0L; key = ""; ttl = 0L; min_commit_ts = 0L; protocol = Two_phase }, false, [])
        ~f:(fun (lock, async, secondaries) field value ->
          match field with
          | 1 -> Result.map (fun primary -> ({ lock with primary }, async, secondaries)) (Proto.contents text field value)
          | 2 -> Result.map (fun version -> ({ lock with version }, async, secondaries)) (Proto.number field value)
          | 3 -> Result.map (fun key -> ({ lock with key }, async, secondaries)) (Proto.contents text field value)
          | 4 -> Result.map (fun ttl -> ({ lock with ttl }, async, secondaries)) (Proto.number field value)
          | 8 -> Result.map (fun flag -> (lock, not (Int64.equal flag 0L), secondaries)) (Proto.number field value)
          | 9 -> Result.map (fun min_commit_ts -> ({ lock with min_commit_ts }, async, secondaries)) (Proto.number field value)
          | 10 -> Result.map (fun key -> (lock, async, secondaries @ [ key ])) (Proto.contents text field value)
          | _ -> Ok (lock, async, secondaries))
    in
    Ok (if async then { lock with protocol = Async_commit secondaries } else lock)
  in
  let key_error slice : (Kv.key_error, _) result =
    let%bind first =
      fields slice ~init:None ~f:(fun first field value -> match first with Some _ -> Ok first | None -> Ok (Some (field, value)))
    in
    match first with
    | None -> Ok (Kv.Other_key_error 0)
    | Some (1, value) ->
        let%bind slice = Proto.slice 1 value in
        Result.map (fun lock -> Kv.Locked lock) (lock slice)
    | Some (2, value) -> Result.map (fun reason -> Kv.Retryable reason) (Proto.contents text 2 value)
    | Some (3, value) -> Result.map (fun reason -> Kv.Abort reason) (Proto.contents text 3 value)
    | Some (8, _) -> Ok Kv.Txn_not_found
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
            Ok (region, pairs @ [ pair ], error)
        | 3 -> Result.map (fun slice -> (region, pairs, Some slice)) (Proto.slice field value)
        | _ -> Ok (region, pairs, error))
  in
  match (region, error) with
  | Some (pos, len), _ -> Result.map (fun error -> Kv.Region_error error) (Kv.region_error text ~pos ~len)
  | None, Some slice ->
      Result.map (fun (error : Kv.key_error) -> match error with Locked lock -> Kv.Reply (Kv.Locks (lock, [])) | error -> Kv.Key_error error) (key_error slice)
  | None, None -> (
      let failed = List.filter_map (function `Failed slice -> Some slice | `Pair _ -> None) pairs in
      let%bind backwards = Base.Result.all (List.map key_error (List.rev failed)) in
      let errors = List.rev backwards in
      match errors with
      | [] -> Ok (Kv.Reply (Kv.Pairs (List.filter_map (function `Pair (key, value) -> Some { Kv.key; value } | `Failed _ -> None) pairs)))
      | _ :: _ -> (
          match List.find_opt (function Kv.Locked _ -> false | _ -> true) errors with
          | Some error -> Ok (Kv.Key_error error)
          | None -> (
              match List.filter_map (function Kv.Locked lock -> Some lock | _ -> None) errors with
              | first :: rest -> Ok (Kv.Reply (Kv.Locks (first, rest)))
              | [] -> Ok (Kv.Key_error (Other_key_error 0)))))

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
        match result with Ok scanned -> show_scan scanned | Error (`Proto error) -> Base.Sexp.to_string (Proto.sexp_of_error error)
      in
      show (Kv.scanned text) = show (reference text))

let scan () =
  let pairs = List.init 3000 (fun i -> { Kv.key = Printf.sprintf "t\x80%08d" i; value = String.make (i mod 300) 'v' }) in
  let request = Kv.scan_request region Kv.nothing_settled ~direction:Forward ~start:"t\x80" ~stop:(Before "t\x81") ~version:42L ~limit:5000 in
  let reply : Test_http2.reply = { (reply ~trailers:[ ("grpc-status", "0") ] (framed (response pairs))) with fragments = 2 } in
  let scenario : Test_http2.scenario =
    { initial_window = 65_535; max_frame = 16_384; exchanges = [ { path = "/tikvpb.Tikv/KvScan"; headers = []; body = framed request; reply } ] }
  in
  let result, heard, _ =
    Test_http2.session scenario (fun connection -> Kv.scan connection region Kv.nothing_settled ~direction:Forward ~start:"t\x80" ~stop:(Before "t\x81") ~version:42L ~limit:5000)
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
  Alcotest.(check bool) "every pair" true (match result with Ok (Reply (Pairs got)) -> got = pairs | Ok _ | Error _ -> false)

let statuses () =
  let numbers text = List.filter_map (fun (field, (value : Proto.value)) -> match value with Varint n -> Some (field, n) | _ -> None) (fields text) in
  let check_of current missing sync : Kv.check = { primary = "pk"; version = 400L; caller = 900L; current; missing; sync } in
  let text = Kv.check_request region (check_of (At 1000L) Leave_if_missing As_locked) in
  let top = fields text in
  Alcotest.(check (list (pair int int64))) "the region" [ (1, 7L) ] (numbers (sub text (List.assoc 1 top)));
  Alcotest.(check string) "the primary key" "pk" (sub text (List.assoc 2 top));
  Alcotest.(check (list (pair int int64))) "start, caller, now, the primary verified" [ (3, 400L); (4, 900L); (5, 1000L); (9, 1L) ] (numbers text);
  Alcotest.(check (list (pair int int64))) "past every TTL, rolled back if missing, as sync commit"
    [ (3, 400L); (4, 900L); (5, -1L); (6, 1L); (7, 1L); (9, 1L) ]
    (numbers (Kv.check_request region (check_of Past_every_ttl Rollback_if_missing Sync_commit_forced)));
  let show reply = Base.Sexp.to_string ([%sexp_of: Kv.status Kv.reply] reply) in
  let checked name expected fill =
    Alcotest.(check string) name (show expected)
      (match Kv.checked (message fill) with Ok got -> show got | Error (`Proto _) -> "not protobuf")
  in
  checked "committed" (Reply (Committed 950L)) (fun buffer -> Proto.uint buffer 4 950L);
  List.iter
    (fun action -> checked (Printf.sprintf "rolled back, action %Ld" action) (Reply Rolled_back) (fun buffer -> Proto.uint buffer 5 action))
    [ 0L; 1L; 2L ];
  checked "rolled back, no action at all" (Reply Rolled_back) ignore;
  checked "a pessimistic action, neither" (Reply (Unsettled 5)) (fun buffer -> Proto.uint buffer 5 5L);
  checked "alive, pushed" (Reply (Uncommitted { ttl = 3000L; pushed = Pushed; min_commit_ts = 901L; protocol = Two_phase }))
    (fun buffer ->
      Proto.uint buffer 3 3000L;
      Proto.uint buffer 5 3L;
      Proto.bytes buffer 6 (lock_message ~min_commit:901L "pk"));
  checked "alive, not pushed" (Reply (Uncommitted { ttl = 3000L; pushed = Not_pushed; min_commit_ts = 0L; protocol = Two_phase }))
    (fun buffer ->
      Proto.uint buffer 3 3000L;
      Proto.bytes buffer 6 (lock_message "pk"));
  checked "alive, async commit" (Reply (Uncommitted { ttl = 3000L; pushed = Not_pushed; min_commit_ts = 905L; protocol = Async_commit [ "s1"; "s2" ] }))
    (fun buffer ->
      Proto.uint buffer 3 3000L;
      Proto.bytes buffer 6 (lock_message ~async:true ~min_commit:905L ~secondaries:[ "s1"; "s2" ] "pk"));
  checked "alive, its lock not told" (Reply (Uncommitted { ttl = 20L; pushed = Not_pushed; min_commit_ts = 0L; protocol = Two_phase }))
    (fun buffer -> Proto.uint buffer 3 20L);
  checked "not found" (Key_error Txn_not_found) (fun buffer -> Proto.bytes buffer 2 (message (fun buffer -> Proto.bytes buffer 8 "")));
  checked "a region error first" (Region_error Epoch_not_match)
    (fun buffer ->
      Proto.bytes buffer 1 (message (fun buffer -> Proto.bytes buffer 5 ""));
      Proto.bytes buffer 2 (message (fun buffer -> Proto.bytes buffer 8 "")));
  let text = Kv.secondaries_request region ~version:400L [ "s1"; "s2" ] in
  let top = fields text in
  Alcotest.(check (list (pair int int64))) "secondaries: the region" [ (1, 7L) ] (numbers (sub text (List.assoc 1 top)));
  Alcotest.(check (list string)) "secondaries: the keys in order" [ "s1"; "s2" ]
    (List.filter_map (fun (field, value) -> if field = 2 then Some (sub text value) else None) top);
  Alcotest.(check (list (pair int int64))) "secondaries: the start" [ (3, 400L) ] (numbers text);
  let show reply = Base.Sexp.to_string ([%sexp_of: Kv.secondaries Kv.reply] reply) in
  let found name expected fill =
    Alcotest.(check string) name (show expected)
      (match Kv.secondaries_checked (message fill) with Ok got -> show got | Error (`Proto _) -> "not protobuf")
  in
  let secondary key : Kv.lock = { (locked_by ~key ()) with min_commit_ts = 903L; protocol = Async_commit [] } in
  found "the locks, in order" (Reply (Locks_found [ secondary "s1"; secondary "s2" ]))
    (fun buffer ->
      Proto.bytes buffer 3 (lock_message ~async:true ~min_commit:903L "s1");
      Proto.bytes buffer 3 (lock_message ~async:true ~min_commit:903L "s2"));
  found "none" (Reply (Locks_found [])) ignore;
  found "committed" (Reply (Committed_secondary 910L)) (fun buffer -> Proto.uint buffer 4 910L);
  found "a key error" (Key_error (Abort "no")) (fun buffer -> Proto.bytes buffer 2 (message (fun buffer -> Proto.bytes buffer 3 "no")))

let calls () =
  let check : Kv.check = { primary = "pk"; version = 400L; caller = 900L; current = At 1000L; missing = Leave_if_missing; sync = As_locked } in
  let answered path body response : Test_http2.exchange = { path; headers = []; body = framed body; reply = reply ~trailers:[ ("grpc-status", "0") ] (framed response) } in
  let scenario : Test_http2.scenario =
    {
      initial_window = 65_535;
      max_frame = 16_384;
      exchanges =
        [
          answered "/tikvpb.Tikv/KvCheckTxnStatus" (Kv.check_request region check) (message (fun buffer -> Proto.uint buffer 4 950L));
          answered "/tikvpb.Tikv/KvCheckSecondaryLocks" (Kv.secondaries_request region ~version:400L [ "s" ]) (message (fun buffer -> Proto.uint buffer 4 960L));
        ];
    }
  in
  let result, heard, _ =
    Test_http2.session scenario (fun connection ->
        Result.bind (Kv.check connection region check) (fun status ->
            Result.map (fun secondaries -> (status, secondaries)) (Kv.check_secondaries connection region ~version:400L [ "s" ])))
  in
  Alcotest.(check (list string)) "faults" [] heard.faults;
  Alcotest.(check (list (option string))) "each call on its path"
    [ Some "/tikvpb.Tikv/KvCheckTxnStatus"; Some "/tikvpb.Tikv/KvCheckSecondaryLocks" ]
    (List.map (fun (headers, _) -> List.assoc_opt ":path" headers) heard.requests);
  Alcotest.(check bool) "each answer read" true
    (match result with Ok (Reply (Committed 950L), Reply (Committed_secondary 960L)) -> true | Ok _ | Error _ -> false)

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
    Alcotest.test_case "transaction status: requests and responses" `Quick statuses;
    Alcotest.test_case "transaction status over HTTP/2" `Quick calls;
    QCheck_alcotest.to_alcotest pairs;
    QCheck_alcotest.to_alcotest fast_path;
  ]
