open Sexplib0.Sexp_conv
open Quiver_tikv

let key = QCheck2.Gen.(string_size ~gen:(oneof_list [ 'a'; 'b'; '\x00'; '\xff' ]) (int_range 0 12))

let codec_roundtrip =
  QCheck2.Test.make ~count:3000 ~name:"bytes decode back from their order-keeping form, whatever follows it"
    QCheck2.Gen.(pair key (string_size ~gen:char (int_range 0 10)))
    (fun (text, tail) -> Codec.decode_bytes (Codec.encode_bytes text ^ tail) = Ok text)

let codec_order =
  QCheck2.Test.make ~count:3000 ~name:"the order-keeping form keeps the order of bytes" QCheck2.Gen.(pair key key)
    (fun (a, b) -> Int.compare (String.compare a b) 0 = Int.compare (String.compare (Codec.encode_bytes a) (Codec.encode_bytes b)) 0)

let codec () =
  Alcotest.(check string) "empty" "\x00\x00\x00\x00\x00\x00\x00\x00\xf7" (Codec.encode_bytes "");
  Alcotest.(check string) "a full group" "12345678\xff\x00\x00\x00\x00\x00\x00\x00\x00\xf7" (Codec.encode_bytes "12345678");
  Alcotest.(check string) "a short group" "abc\x00\x00\x00\x00\x00\xfa" (Codec.encode_bytes "abc");
  let refused name expected text = Alcotest.(check bool) name true (Codec.decode_bytes text = Error (`Codec expected)) in
  refused "short" Short "abc\x00\x00\xfa";
  refused "nine bytes of padding" (Marker 0xf6) "abc\x00\x00\x00\x00\x00\xf6";
  refused "padding not zero" Padding "abc\x00\x01\x00\x00\x00\xfa";
  refused "no last group" Short "12345678\xff";
  Alcotest.(check (pair string string)) "rows of table 75" ("t\x80\x00\x00\x00\x00\x00\x00\x4b_r", "t\x80\x00\x00\x00\x00\x00\x00\x4b_s") (Codec.records 75L);
  Alcotest.(check (pair string string)) "all keys of table 255" ("t\x80\x00\x00\x00\x00\x00\x00\xff", "t\x80\x00\x00\x00\x00\x00\x01\x00") (Codec.table 255L);
  let table = fst (Codec.table 75L) in
  Alcotest.(check (option string)) "an index's keys end where the next index starts"
    (Some (table ^ "_i\x80\x00\x00\x00\x00\x00\x00\x02"))
    (Codec.kind_end (table ^ "_i\x80\x00\x00\x00\x00\x00\x00\x01\x03abc"));
  Alcotest.(check (option string)) "carrying past 0xff" (Some (table ^ "_i\x80\x00\x00\x00\x00\x00\x01"))
    (Codec.kind_end (table ^ "_i\x80\x00\x00\x00\x00\x00\x00\xff"));
  Alcotest.(check (option string)) "the rows end before _s" (Some (table ^ "_s")) (Codec.kind_end (table ^ "_r\x80\x00\x00\x00\x00\x00\x00\x07"));
  Alcotest.(check (option string)) "a key of no known kind" None (Codec.kind_end (table ^ "_x"));
  Alcotest.(check string) "an index key of table 75 as table 300's"
    "t\x80\x00\x00\x00\x00\x00\x01\x2c_i\x80\x00\x00\x00\x00\x00\x00\x01x"
    (Codec.retabled ~into:300L "t\x80\x00\x00\x00\x00\x00\x00\x4b_i\x80\x00\x00\x00\x00\x00\x00\x01x")

let hex text = String.uppercase_ascii (Hex_encode.to_hex (Codec.encode_bytes text))
let row cells = Array.of_list (List.map Option.some cells)

let regions () =
  let rows =
    [
      row [ "2"; hex "m"; ""; "1"; "7" ];
      row [ "3"; ""; hex "m"; "2"; "5" ];
      row [ "2"; hex "m"; ""; "1"; "7" ];
    ]
  and peers =
    [
      row [ "2"; "22"; "3"; "0"; "tikv-3:20160" ];
      row [ "3"; "31"; "1"; "1"; "tikv-1:20160" ];
      row [ "2"; "20"; "2"; "1"; "tikv-2:20160" ];
      row [ "2"; "21"; "1"; "0"; "tikv-1:20160" ];
    ]
  in
  let replica id store address : Region.replica = { peer = { id; store }; address } in
  let expected : Region.t list =
    [
      {
        context = { id = 3L; conf_ver = 2L; version = 5L; peer = 31L; store = 1L };
        start = "";
        stop = Before "m";
        address = "tikv-1:20160";
        replicas = [ replica 31L 1L "tikv-1:20160" ];
      };
      {
        context = { id = 2L; conf_ver = 1L; version = 7L; peer = 20L; store = 2L };
        start = "m";
        stop = Endless;
        address = "tikv-2:20160";
        replicas = [ replica 20L 2L "tikv-2:20160"; replica 21L 1L "tikv-1:20160"; replica 22L 3L "tikv-3:20160" ];
      };
    ]
  in
  let show result =
    match result with
    | Ok regions -> Base.Sexp.to_string ([%sexp_of: Region.t list] regions)
    | Error (`Region error) -> Base.Sexp.to_string (Region.sexp_of_error error)
  in
  Alcotest.(check string) "each region once, by its first key" (show (Ok expected)) (show (Region.of_rows ~regions:rows ~peers));
  Alcotest.(check string) "a key in the first" "3" (Option.fold ~none:"" ~some:(fun (region : Region.t) -> Int64.to_string region.context.id) (Region.find expected "a"));
  Alcotest.(check string) "the bound in the second" "2" (Option.fold ~none:"" ~some:(fun (region : Region.t) -> Int64.to_string region.context.id) (Region.find expected "m"));
  Alcotest.(check string) "no leader" (show (Error (`Region (Region.No_leader 2L))))
    (show (Region.of_rows ~regions:rows ~peers:(List.filter (fun row -> row.(1) <> Some "20") peers)));
  Alcotest.(check string) "leading neither 0 nor 1"
    (show (Error (`Region (Region.Malformed [ Some "3"; Some "31"; Some "1"; Some "yes"; Some "a:1" ]))))
    (show (Region.of_rows ~regions:rows ~peers:[ row [ "3"; "31"; "1"; "yes"; "a:1" ] ]));
  Alcotest.(check string) "not hex" (show (Error (`Region (Region.Undecodable { region = 2L; key = "ZZ" })))) (show (Region.of_rows ~regions:[ row [ "2"; "ZZ"; ""; "1"; "1" ] ] ~peers));
  Alcotest.(check string) "not the order-keeping form" (show (Error (`Region (Region.Undecodable { region = 2L; key = "6162" }))))
    (show (Region.of_rows ~regions:[ row [ "2"; "6162"; ""; "1"; "1" ] ] ~peers));
  Alcotest.(check string) "not a number" (show (Error (`Region (Region.Malformed [ Some "x"; Some ""; Some ""; Some "1"; Some "1" ]))))
    (show (Region.of_rows ~regions:[ row [ "x"; ""; ""; "1"; "1" ] ] ~peers))

(* A region of the model TiKV, as it is now. *)
type held = { id : int64; version : int64; leader : int64; first : string; past : Kv.bound } [@@deriving sexp_of]

type event = Split of string | Merge of int | Move of int | Busy of int | Lock of int * int | Quiet [@@deriving sexp_of]

type case = {
  keys : string list;
  bounds : string list;
  start : string;
  stop : Kv.bound;
  limit : int;
  events : event list;
}
[@@deriving sexp_of]

let cost = function Split _ | Merge _ | Move _ -> 1 | Busy n | Lock (_, n) -> n | Quiet -> 0

let gen =
  QCheck2.Gen.(
    let* keys = map (List.sort_uniq String.compare) (list_size (int_range 0 60) key)
    and* bounds = map (List.sort_uniq String.compare) (list_size (int_range 0 5) key)
    and* start = oneof [ return ""; key ]
    and* stop = oneof [ return Kv.Endless; map (fun key -> Kv.Before key) key ]
    and* limit = int_range 1 5
    and* events =
      list_size (int_range 0 12)
        (oneof
           [
             map (fun key -> Split key) key;
             map (fun n -> Merge n) (int_range 0 6);
             map (fun n -> Move n) (int_range 0 6);
             map (fun n -> Busy n) (int_range 1 3);
             map2 (fun key n -> Lock (key, n)) (int_range 0 59) (int_range 1 3);
             return Quiet;
           ])
    in
    let events = List.rev (snd (List.fold_left (fun (spent, kept) event -> if spent + cost event > 15 then (spent, kept) else (spent + cost event, event :: kept)) (0, []) events)) in
    return { keys; bounds = List.filter (fun bound -> bound <> "") bounds; start; stop; limit; events })

let before key (bound : Kv.bound) = match bound with Endless -> true | Before stop -> String.compare key stop < 0

let run case =
  let next_id = ref 100L in
  let held =
    ref
      (List.mapi
         (fun i first ->
           { id = Int64.of_int (i + 1); version = 1L; leader = 1L; first; past = Option.fold ~none:Kv.Endless ~some:(fun key -> Kv.Before key) (List.nth_opt case.bounds i) })
         ("" :: case.bounds))
  in
  let busy = ref 0 and locks = Hashtbl.create 4 and events = ref case.events and failures = ref 0 and pauses = ref 0 in
  let apply = function
    | Quiet -> ()
    | Split at ->
        held :=
          List.concat_map
            (fun region ->
              if String.compare region.first at < 0 && before at region.past then (
                next_id := Int64.add !next_id 1L;
                [ { region with version = Int64.add region.version 1L; past = Before at }; { region with id = !next_id; version = Int64.add region.version 1L; first = at } ])
              else [ region ])
            !held
    | Merge i -> (
        match (List.nth_opt !held i, List.nth_opt !held (i + 1)) with
        | Some left, Some right ->
            held :=
              List.filter_map
                (fun region ->
                  if region.id = left.id then Some { left with version = Int64.add (Int64.max left.version right.version) 1L; past = right.past }
                  else if region.id = right.id then None
                  else Some region)
                !held
        | _ -> ())
    | Move i -> held := List.mapi (fun j region -> if j = i mod List.length !held then { region with leader = Int64.add region.leader 1L } else region) !held
    | Busy n -> busy := !busy + n
    | Lock (index, n) -> Option.iter (fun key -> Hashtbl.replace locks key n) (List.nth_opt case.keys index)
  in
  let known region : Region.t =
    { context = { id = region.id; conf_ver = 1L; version = region.version; peer = region.leader; store = 1L }; start = region.first; stop = region.past; address = "model"; replicas = [] }
  in
  let cache = ref (List.map known !held) in
  let beyond (stop : Kv.bound) (past : Kv.bound) =
    match (stop, past) with _, Endless -> false | Endless, Before _ -> true | Before a, Before b -> String.compare a b > 0
  in
  let fail scanned =
    incr failures;
    Ok scanned
  in
  let store : _ Read.store =
    {
      locate =
        (fun freshness key ->
          let () = match freshness with Afresh -> cache := List.map known !held | Known -> () in
          Option.to_result ~none:(`Read (Read.Unheld key)) (Region.find !cache key));
      scan =
        (fun (region : Region.t) ~start ~stop ->
          let () = match !events with event :: rest -> events := rest; apply event | [] -> () in
          match List.find_opt (fun held -> held.id = region.context.id) !held with
          | None -> fail (Kv.Region_error Region_not_found)
          | Some current when current.version <> region.context.version -> fail (Kv.Region_error Epoch_not_match)
          | Some current when current.leader <> region.context.peer -> fail (Kv.Region_error (Not_leader (Some { id = current.leader; store = 1L })))
          | Some _ when !busy > 0 ->
              decr busy;
              fail (Kv.Region_error (Server_is_busy { reason = "model"; backoff_ms = 0L }))
          | Some current when String.compare start current.first < 0 || beyond stop current.past ->
              fail (Kv.Region_error (Other { kind = 4; message = "key not in region" }))
          | Some _ -> (
              let inside = List.filter (fun key -> String.compare start key <= 0 && before key stop) case.keys in
              let taken = Base.List.take inside case.limit in
              let reach = if List.length taken = case.limit then Kv.Before (Base.List.last_exn taken ^ "\x00") else stop in
              match List.find_opt (fun key -> String.compare start key <= 0 && before key reach && Hashtbl.find_opt locks key |> Option.fold ~none:false ~some:(fun n -> n > 0)) case.keys with
              | Some locked ->
                  Hashtbl.replace locks locked (Hashtbl.find locks locked - 1);
                  fail (Kv.Key_error (Locked { primary = locked; version = 1L; key = locked; ttl = 10L }))
              | None -> Ok (Kv.Pairs (List.map (fun key -> { Kv.key; value = "v" ^ key }) taken))));
      pause = (fun _ -> incr pauses);
    }
  in
  let read = ref [] in
  let result = Read.range store ~start:case.start ~stop:case.stop ~limit:case.limit ~f:(fun pairs -> read := List.rev_append pairs !read; Ok ()) in
  (result, List.rev !read, !failures, !pauses)

let reads =
  QCheck2.Test.make ~count:3000
    ~name:
      "a range is read whole, each key once and in order, through splits, merges, moved leaders, busy \
       servers, locks and a stale cache of regions, waiting once for each refusal"
    ~print:(fun case -> Base.Sexp.to_string_hum (sexp_of_case case))
    gen
    (fun case ->
      let result, read, failures, pauses = run case in
      let expected = List.filter (fun key -> String.compare case.start key <= 0 && before key case.stop) case.keys in
      match result with
      | Ok () -> List.map (fun (pair : Kv.pair) -> pair.key) read = expected && List.for_all (fun (pair : Kv.pair) -> pair.value = "v" ^ pair.key) read && failures = pauses
      | Error (`Read error) -> QCheck2.Test.fail_reportf "%a" Base.Sexp.pp (Read.sexp_of_error error))

let refusals () =
  let region : Region.t = { context = { id = 1L; conf_ver = 1L; version = 1L; peer = 1L; store = 1L }; start = ""; stop = Endless; address = "a"; replicas = [] } in
  let pauses = ref [] in
  let outcome ?(locate = fun (_ : Read.freshness) (_ : string) -> Ok region) answer =
    pauses := [];
    let store : _ Read.store =
      { locate; scan = (fun _ ~start:_ ~stop:_ -> Ok answer); pause = (fun span -> pauses := span :: !pauses) }
    in
    match Read.range store ~start:"" ~stop:Endless ~limit:10 ~f:(fun _ -> Ok ()) with
    | Ok () -> "read"
    | Error (`Read error) -> Base.Sexp.to_string (Read.sexp_of_error error)
  in
  let check name expected answer = Alcotest.(check string) name (Base.Sexp.to_string (Read.sexp_of_error expected)) (outcome answer) in
  let lock : Kv.lock = { primary = "p"; version = 1L; key = "k"; ttl = 1L } in
  check "a lock that stays" (Still_locked lock) (Key_error (Locked lock));
  check "a server that stays busy" (Region_kept_failing (Server_is_busy { reason = "r"; backoff_ms = 1500L }))
    (Region_error (Server_is_busy { reason = "r"; backoff_ms = 1500L }));
  Alcotest.(check (list int)) "each wait as long as the server asks" (List.init Read.attempts (fun _ -> 1500))
    (List.map (fun span -> Core.Time_ns.Span.to_int_ms span) !pauses);
  check "a retryable key error" (Key (Retryable "conflict")) (Key_error (Retryable "conflict"));
  check "keys out of order" (Disordered "a") (Pairs [ { key = "b"; value = "" }; { key = "a"; value = "" } ]);
  check "a key twice" (Disordered "b") (Pairs [ { key = "b"; value = "" }; { key = "b"; value = "" } ]);
  Alcotest.(check string) "a key past the region" (Base.Sexp.to_string (Read.sexp_of_error (Disordered "n")))
    (outcome ~locate:(fun _ _ -> Ok { region with stop = Before "m" }) (Pairs [ { key = "n"; value = "" } ]));
  Alcotest.(check string) "a region that does not hold the key" (Base.Sexp.to_string (Read.sexp_of_error (Unheld "")))
    (outcome ~locate:(fun _ _ -> Ok { region with start = "a" }) (Pairs []))

let crc64 () =
  Alcotest.(check int64) "the check value of CRC-64/XZ" 0x995DC9BBDF1939FAL (Crc64.pair "123456789" "");
  Alcotest.(check int64) "split anywhere between key and value" 0x995DC9BBDF1939FAL (Crc64.pair "1234" "56789");
  Alcotest.(check int64) "nothing" 0L (Crc64.pair "" "")

let crc64_split =
  let table =
    Array.init 256 (fun byte ->
        Base.Fn.apply_n_times ~n:8
          (fun crc ->
            if Int64.equal (Int64.logand crc 1L) 1L then Int64.logxor (Int64.shift_right_logical crc 1) 0xC96C5795D7870F42L
            else Int64.shift_right_logical crc 1)
          (Int64.of_int byte))
  in
  let bytewise text =
    Int64.lognot
      (Base.String.fold text ~init:(-1L) ~f:(fun crc c ->
           Int64.logxor table.(Int64.to_int (Int64.logand (Int64.logxor crc (Int64.of_int (Char.code c))) 0xFFL)) (Int64.shift_right_logical crc 8)))
  in
  QCheck2.Test.make ~count:3000 ~name:"a pair's CRC64 is the bytewise CRC64 of the key and value joined"
    QCheck2.Gen.(pair (string_size (int_range 0 70)) (string_size (int_range 0 300)))
    (fun (key, value) -> Int64.equal (Crc64.pair key value) (bytewise (key ^ value)))

let queries () =
  let name : Quiver_core.Schema.Name.t = { database = Support.ident "d"; table = Support.ident "t'" } in
  Alcotest.(check string) "the table's id"
    "SELECT TIDB_TABLE_ID FROM information_schema.TABLES WHERE TABLE_SCHEMA = _utf8mb4 X'64' AND TABLE_NAME = _utf8mb4 X'7427'"
    (Quiver_mysql.Query.sql (Table_id name));
  Alcotest.(check string) "its regions"
    "SELECT REGION_ID, START_KEY, END_KEY, EPOCH_CONF_VER, EPOCH_VERSION FROM information_schema.TIKV_REGION_STATUS WHERE TABLE_ID = 75"
    (Quiver_mysql.Query.sql (Table_regions 75L));
  Alcotest.(check string) "their peers"
    "SELECT p.REGION_ID, p.PEER_ID, p.STORE_ID, p.IS_LEADER, s.ADDRESS FROM information_schema.TIKV_REGION_PEERS p JOIN \
     information_schema.TIKV_STORE_STATUS s ON s.STORE_ID = p.STORE_ID WHERE p.REGION_ID IN (2, 3)"
    (Quiver_mysql.Query.sql (Region_peers (2L, [ 3L ])));
  Alcotest.(check string) "PD" "SELECT INSTANCE FROM information_schema.CLUSTER_INFO WHERE TYPE = 'pd'" (Quiver_mysql.Query.sql Pd_instances);
  Alcotest.(check string) "allocators" "SHOW TABLE `d`.`t'` NEXT_ROW_ID" (Quiver_mysql.Query.sql (Next_row_ids name));
  Alcotest.(check string) "checksum" "ADMIN CHECKSUM TABLE `d`.`t'`" (Quiver_mysql.Query.sql (Checksum name));
  let copy : Quiver_core.Schema.Name.t = { database = Support.ident "d"; table = Support.ident "q_1" } in
  Alcotest.(check string) "made like" "CREATE TABLE `d`.`q_1` LIKE `d`.`t'`" (Quiver_mysql.Statement.sql (Create_like { table = copy; like = name }));
  Alcotest.(check string) "row ids moved on" "ALTER TABLE `d`.`q_1` AUTO_INCREMENT = 30001"
    (Quiver_mysql.Statement.sql (Rebase { table = copy; allocator = Auto_increment; next = 30001L }));
  Alcotest.(check string) "random ids moved on" "ALTER TABLE `d`.`q_1` AUTO_RANDOM_BASE = 7"
    (Quiver_mysql.Statement.sql (Rebase { table = copy; allocator = Auto_random; next = 7L }))

let tests =
  [
    Alcotest.test_case "order-keeping bytes and table keys" `Quick codec;
    Alcotest.test_case "the queries for regions" `Quick queries;
    Alcotest.test_case "CRC64 as TiKV sums tables" `Quick crc64;
    QCheck_alcotest.to_alcotest crc64_split;
    Alcotest.test_case "regions from information_schema rows" `Quick regions;
    Alcotest.test_case "reads that give up" `Quick refusals;
  ]
  @ List.map QCheck_alcotest.to_alcotest [ codec_roundtrip; codec_order; reads ]
