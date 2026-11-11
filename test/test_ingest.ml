open Sexplib0.Sexp_conv
open Quiver_tikv

let key = QCheck2.Gen.(string_size ~gen:(oneof_list [ 'a'; 'b'; '\x00'; '\xff' ]) (int_range 1 10))

(* A file one store of the model wrote. *)
type sst = { uuid : string; store : int64; region : int64; conf_ver : int64; version : int64; pairs : Kv.pair list }

(* A region of the model TiKV, as it is now. *)
type held = { id : int64; conf_ver : int64; version : int64; leader : int64; peers : int64 list; first : string; past : Kv.bound }
[@@deriving sexp_of]

type event =
  | Split of string
  | Merge of int
  | Move of int
  | Join of int  (** Another peer joins the region. *)
  | Busy of int
  | Refuse_write
  | Refuse_ingest
  | Quiet
[@@deriving sexp_of]

type case = { keys : string list; bounds : string list; batch : int; events : event list } [@@deriving sexp_of]

let cost = function Split _ | Merge _ | Move _ | Join _ | Refuse_write | Refuse_ingest -> 1 | Busy n -> n | Quiet -> 0

let gen =
  QCheck2.Gen.(
    let* keys = map (List.sort_uniq String.compare) (list_size (int_range 0 60) key)
    and* bounds = map (List.sort_uniq String.compare) (list_size (int_range 0 5) key)
    and* batch = int_range 1 40
    and* events =
      list_size (int_range 0 14)
        (oneof
           [
             map (fun key -> Split key) key;
             map (fun n -> Merge n) (int_range 0 6);
             map (fun n -> Move n) (int_range 0 6);
             map (fun n -> Join n) (int_range 0 6);
             map (fun n -> Busy n) (int_range 1 3);
             return Refuse_write;
             return Refuse_ingest;
             return Quiet;
           ])
    in
    let events =
      List.rev
        (snd (List.fold_left (fun (spent, kept) event -> if spent + cost event > 15 then (spent, kept) else (spent + cost event, event :: kept)) (0, []) events))
    in
    return { keys; bounds; batch; events })

let before key (bound : Kv.bound) = match bound with Endless -> true | Before stop -> String.compare key stop < 0

let run ?(pick = fun (_ : Region.t) written -> written) case =
  let next_id = ref 100L and next_peer = ref 1000L and next_uuid = ref 0 in
  let store_of peer = Int64.rem peer 3L in
  let held =
    ref
      (List.mapi
         (fun i first ->
           let id = Int64.of_int (i + 1) in
           let peers = [ Int64.mul id 10L; Int64.add (Int64.mul id 10L) 1L ] in
           { id; conf_ver = 1L; version = 1L; leader = List.hd peers; peers; first; past = Option.fold ~none:Kv.Endless ~some:(fun key -> Kv.Before key) (List.nth_opt case.bounds i) })
         ("" :: case.bounds))
  in
  let busy = ref 0 and refuse_write = ref 0 and refuse_ingest = ref 0 and events = ref case.events in
  let failures = ref 0 and pauses = ref 0 and files = Hashtbl.create 16 and data = Hashtbl.create 64 in
  let bumped region = Int64.add region 1L in
  let apply = function
    | Quiet -> ()
    | Split at ->
        held :=
          List.concat_map
            (fun region ->
              if String.compare region.first at < 0 && before at region.past then (
                next_id := Int64.add !next_id 1L;
                let peers = List.map (fun peer -> Int64.add peer 500L) region.peers in
                [
                  { region with version = bumped region.version; past = Before at };
                  { region with id = !next_id; version = bumped region.version; first = at; peers; leader = List.hd peers };
                ])
              else [ region ])
            !held
    | Merge i -> (
        match (List.nth_opt !held i, List.nth_opt !held (i + 1)) with
        | Some left, Some right ->
            held :=
              List.filter_map
                (fun region ->
                  if region.id = left.id then Some { left with version = bumped (Int64.max left.version right.version); past = right.past }
                  else if region.id = right.id then None
                  else Some region)
                !held
        | _ -> ())
    | Move i ->
        held :=
          List.mapi
            (fun j region ->
              if j = i mod List.length !held then
                { region with leader = Option.value ~default:region.leader (List.find_opt (fun peer -> peer <> region.leader) region.peers) }
              else region)
            !held
    | Join i ->
        held :=
          List.mapi
            (fun j region ->
              if j = i mod List.length !held then (
                next_peer := Int64.add !next_peer 1L;
                { region with conf_ver = bumped region.conf_ver; peers = region.peers @ [ !next_peer ] })
              else region)
            !held
    | Busy n -> busy := !busy + n
    | Refuse_write -> incr refuse_write
    | Refuse_ingest -> incr refuse_ingest
  in
  let step () = match !events with event :: rest -> events := rest; apply event | [] -> () in
  let known region : Region.t =
    {
      context = { id = region.id; conf_ver = region.conf_ver; version = region.version; peer = region.leader; store = store_of region.leader };
      start = region.first;
      stop = region.past;
      address = "model";
      replicas = List.map (fun peer : Region.replica -> { peer = { id = peer; store = store_of peer }; address = "model" }) region.peers;
    }
  in
  let cache = ref (List.map known !held) in
  let current (region : Kv.region) = List.find_opt (fun held -> held.id = region.id) !held in
  let store : (sst, _) Ingest.store =
    {
      locate =
        (fun freshness key ->
          let () = match freshness with Afresh -> cache := List.map known !held | Known -> () in
          Option.to_result ~none:(`Ingest (Ingest.Unheld key)) (Region.find !cache key));
      write =
        (fun region job ->
          step ();
          let pairs = List.concat_map Pairs.to_list job.pairs in
          let last = Option.map (fun (replica : Region.replica) -> replica.peer.id) (Base.List.last region.replicas) in
          let outcomes =
            List.map
              (fun (replica : Region.replica) ->
                let outcome : sst Import.written =
                  match current job.region with
                  | Some _ when !refuse_write > 0 && last = Some replica.peer.id -> Write_failed "refused"
                  | Some now
                    when now.version = job.region.version && now.conf_ver = job.region.conf_ver && List.mem replica.peer.id now.peers ->
                      let sst = { uuid = job.uuid; store = replica.peer.store; region = job.region.id; conf_ver = job.region.conf_ver; version = job.region.version; pairs } in
                      Hashtbl.replace files (job.uuid, replica.peer.store) sst;
                      Wrote [ sst ]
                  | Some _ | None -> Write_failed "stale"
                in
                (replica, outcome))
              region.replicas
          in
          let () =
            if List.exists (fun (_, outcome) -> match outcome with Import.Write_failed _ -> true | Wrote _ -> false) outcomes then (
              incr failures;
              refuse_write := Int.max 0 (!refuse_write - 1))
          in
          Ok (pick region outcomes));
      ingest =
        (fun region ssts ->
          step ();
          let fail error =
            incr failures;
            Ok (Import.Ingest_failed error)
          in
          match current region.context with
          | None -> fail Region_not_found
          | Some now when now.leader <> region.context.peer -> fail (Not_leader (Some { id = now.leader; store = store_of now.leader }))
          | Some _ when !busy > 0 ->
              decr busy;
              fail (Server_is_busy { reason = "model"; backoff_ms = 0L })
          | Some _ when !refuse_ingest > 0 ->
              decr refuse_ingest;
              fail (Other { kind = 7; message = "stale command" })
          | Some now -> (
              let fitting sst =
                sst.region = now.id && sst.version = now.version && sst.conf_ver = now.conf_ver && sst.store = store_of now.leader
                && Hashtbl.find_opt files (sst.uuid, sst.store) = Some sst
                && List.for_all (fun (pair : Kv.pair) -> String.compare now.first pair.key <= 0 && before pair.key now.past) sst.pairs
              in
              let everywhere sst = List.for_all (fun peer -> Hashtbl.mem files (sst.uuid, store_of peer)) now.peers in
              match ssts with
              | [] -> fail (Other { kind = 0; message = "no files" })
              | _ :: _ when not (List.for_all fitting ssts) -> fail Epoch_not_match
              | _ :: _ when not (List.for_all everywhere ssts) -> fail (Other { kind = 0; message = "a peer lacks the file" })
              | _ :: _ ->
                  List.iter (fun sst -> List.iter (fun (pair : Kv.pair) -> Hashtbl.replace data pair.key (pair.value, sst.uuid)) sst.pairs) ssts;
                  Ok Import.Ingested));
      uuid =
        (fun () ->
          incr next_uuid;
          Printf.sprintf "%016d" !next_uuid);
      pause = (fun _ -> incr pauses);
    }
  in
  let pairs = List.map (fun key -> { Kv.key; value = "v" ^ key }) case.keys in
  let result = Ingest.deliver store ~commit_ts:7L ~batch:case.batch (Pairs.of_list pairs) in
  (result, data, !failures, !pauses)

let delivers =
  QCheck2.Test.make ~count:3000
    ~name:
      "every pair reaches its region once, through splits, merges, moved leaders, new peers, busy leaders and \
       refused writes and ingests, waiting once for each refusal"
    ~print:(fun case -> Base.Sexp.to_string_hum (sexp_of_case case))
    gen
    (fun case ->
      let result, data, failures, pauses = run case in
      match result with
      | Ok () ->
          Hashtbl.length data = List.length case.keys
          && List.for_all (fun key -> Option.map fst (Hashtbl.find_opt data key) = Some ("v" ^ key)) case.keys
          && failures = pauses
      | Error (`Ingest error) -> QCheck2.Test.fail_reportf "%a" Base.Sexp.pp (Ingest.sexp_of_error error))

let leader_files () =
  let case = { keys = [ "a"; "b"; "c" ]; bounds = []; batch = 10; events = [] } in
  let others (region : Region.t) written =
    List.map
      (fun ((replica : Region.replica), outcome) ->
        let theirs =
          List.find_map
            (fun ((other : Region.replica), outcome) -> if Int64.equal other.peer.id region.context.peer then None else Some outcome)
            written
        in
        (replica, Option.value ~default:outcome theirs))
      written
  in
  Alcotest.(check bool) "files written by another peer are never ingested" true
    (match run ~pick:others case with Error (`Ingest (Ingest.Kept_failing (Ingest_failed Epoch_not_match))), _, _, _ -> true | _ -> false);
  let leaderless (region : Region.t) written =
    List.filter (fun ((replica : Region.replica), _) -> not (Int64.equal replica.peer.id region.context.peer)) written
  in
  Alcotest.(check bool) "no leader among the peers written to" true
    (match run ~pick:leaderless case with Error (`Ingest (Ingest.Kept_failing Leaderless)), _, _, _ -> true | _ -> false)

let refusals () =
  let region : Region.t =
    {
      context = { id = 1L; conf_ver = 1L; version = 1L; peer = 1L; store = 1L };
      start = "";
      stop = Endless;
      address = "a";
      replicas = [ { peer = { id = 1L; store = 1L }; address = "a" } ];
    }
  in
  let pauses = ref [] and jobs = ref [] in
  let outcome ?(locate = fun (_ : Read.freshness) (_ : string) -> Ok region) ?(written = Import.Wrote [ () ]) answer =
    pauses := [];
    jobs := [];
    let store : (unit, _) Ingest.store =
      {
        locate;
        write = (fun region job -> jobs := job :: !jobs; Ok (List.map (fun replica -> (replica, written)) region.replicas));
        ingest = (fun _ _ -> Ok answer);
        uuid = (fun () -> "0123456789abcdef");
        pause = (fun span -> pauses := span :: !pauses);
      }
    in
    match Ingest.deliver store ~commit_ts:5L ~batch:4 (Pairs.of_list [ { key = "a"; value = "12" }; { key = "b"; value = "" }; { key = "c"; value = "3" } ]) with
    | Ok () -> "delivered"
    | Error (`Ingest error) -> Base.Sexp.to_string (Ingest.sexp_of_error error)
  in
  let check name expected text = Alcotest.(check string) name (Base.Sexp.to_string (Ingest.sexp_of_error expected)) text in
  Alcotest.(check string) "an ingest taken" "delivered" (outcome Ingested);
  Alcotest.(check (list (list (list string)))) "messages of at most four bytes, four included"
    [ [ [ "a"; "b" ]; [ "c" ] ] ]
    (List.map (fun (job : Import.job) -> List.map (fun span -> List.map (fun (pair : Kv.pair) -> pair.key) (Pairs.to_list span)) job.pairs) !jobs);
  check "a full disk" (Refused (Other { kind = 15; message = "full" })) (outcome (Ingest_failed (Other { kind = 15; message = "full" })));
  check "a leader that stays busy" (Kept_failing (Ingest_failed (Server_is_busy { reason = "r"; backoff_ms = 1500L })))
    (outcome (Ingest_failed (Server_is_busy { reason = "r"; backoff_ms = 1500L })));
  Alcotest.(check (list int)) "each wait as long as the leader asks, until the patience is spent"
    (List.init (Core.Time_ns.Span.to_int_ms Ingest.patience / 1500) (fun _ -> 1500))
    (List.map Core.Time_ns.Span.to_int_ms !pauses);
  Alcotest.(check int) "one write for all the waits" 1 (List.length !jobs);
  check "a write refused for good" (Kept_failing (Write_failed "no")) (outcome ~written:(Write_failed "no") Ingested);
  Alcotest.(check int) "written anew each time" (Ingest.attempts + 1) (List.length !jobs);
  check "a region that does not hold the key" (Unheld "a") (outcome ~locate:(fun _ _ -> Ok { region with start = "b" }) Ingested)

let job : Import.job =
  {
    uuid = "0123456789abcdef";
    region = { id = 7L; conf_ver = 3L; version = 11L; peer = 9L; store = 2L };
    commit_ts = 449_000_000_000_000_000L;
    pairs =
      List.map Pairs.of_list [ [ { key = "t\x80a"; value = "1" }; { key = "t\x80b"; value = "" } ]; [ { key = "t\x80c"; value = String.make 300 'v' } ] ];
  }

let requests () =
  let fields text =
    match Proto.fold text ~pos:0 ~len:(String.length text) ~init:[] ~f:(fun acc field value -> Ok ((field, value) :: acc)) with
    | Ok fields -> List.rev fields
    | Error (`Proto _) -> Alcotest.fail "not protobuf"
  in
  let sub text (value : Proto.value) =
    match value with Delimited { pos; len } -> String.sub text pos len | Varint _ | Fixed64 _ | Fixed32 _ -> Alcotest.fail "not delimited"
  in
  let numbers text = List.filter_map (fun (field, (value : Proto.value)) -> match value with Varint n -> Some (field, n) | _ -> None) (fields text) in
  match Import.write_requests job with
  | [ first; second; third ] ->
      let meta = sub first (List.assoc 1 (fields first)) in
      Alcotest.(check string) "the files' name" job.uuid (sub meta (List.assoc 1 (fields meta)));
      let range = sub meta (List.assoc 2 (fields meta)) in
      Alcotest.(check (pair string string)) "the range, first and last key in the order-keeping form"
        (Codec.encode_bytes "t\x80a", Codec.encode_bytes "t\x80c")
        (sub range (List.assoc 1 (fields range)), sub range (List.assoc 2 (fields range)));
      Alcotest.(check (list (pair int int64))) "the region" [ (6, 7L) ] (numbers meta);
      Alcotest.(check (list (pair int int64))) "its epoch" [ (1, 3L); (2, 11L) ] (numbers (sub meta (List.assoc 7 (fields meta))));
      Alcotest.(check string) "the source" "quiver" (let context = sub first (List.assoc 3 (fields first)) in sub context (List.assoc 24 (fields context)));
      let batch text =
        let batch = sub text (List.assoc 2 (fields text)) in
        ( numbers batch,
          List.filter_map
            (fun (field, value) ->
              if field = 2 then
                let pair = sub batch value in
                Some (sub pair (List.assoc 1 (fields pair)), sub pair (List.assoc 2 (fields pair)))
              else None)
            (fields batch) )
      in
      Alcotest.(check (pair (list (pair int int64)) (list (pair string string)))) "the first batch"
        ([ (1, job.commit_ts) ], [ ("t\x80a", "1"); ("t\x80b", "") ])
        (batch second);
      Alcotest.(check (pair (list (pair int int64)) (list (pair string string)))) "the second batch"
        ([ (1, job.commit_ts) ], [ ("t\x80c", String.make 300 'v') ])
        (batch third)
  | _ -> Alcotest.fail "a meta and two batches"

let message fill =
  let buffer = Buffer.create 64 in
  fill buffer;
  Buffer.contents buffer

let write () =
  let sst name = message (fun buffer -> Proto.bytes buffer 1 name; Proto.bytes buffer 5 "write") in
  let response = message (fun buffer -> Proto.bytes buffer 2 (sst "first"); Proto.bytes buffer 2 (sst "second")) in
  let body = String.concat "" (List.map Test_kv.framed (Import.write_requests job)) in
  let scenario ~reply : Test_http2.scenario =
    { initial_window = 65_535; max_frame = 16_384; exchanges = [ { path = "/import_sstpb.ImportSST/Write"; headers = []; body; reply } ] }
  in
  let ok = Test_kv.reply ~trailers:[ ("grpc-status", "0") ] (Test_kv.framed response) in
  let refused = Test_kv.reply ~trailers:[ ("grpc-status", "2"); ("grpc-message", "request too old") ] "" in
  let result, first, second =
    let result, first, _ =
      Test_http2.session (scenario ~reply:ok) (fun one ->
          let result, second, _ = Test_http2.session (scenario ~reply:refused) (fun two -> Import.write [ one; two ] job) in
          Ok (result, second))
    in
    match result with Ok (result, second) -> (result, first, second) | Error _ -> Alcotest.fail "no session"
  in
  Alcotest.(check (list string)) "faults of the first" [] first.faults;
  Alcotest.(check (list string)) "faults of the second" [] second.faults;
  Alcotest.(check (list string)) "each store got every message" [ body ] (List.map snd first.requests);
  Alcotest.(check (list (option string))) "through Write" [ Some "/import_sstpb.ImportSST/Write" ]
    (List.map (fun (headers, _) -> List.assoc_opt ":path" headers) first.requests);
  Alcotest.(check (list string)) "the second too" [ body ] (List.map snd second.requests);
  match result with
  | Ok [ Wrote ssts; Write_failed "request too old" ] ->
      Alcotest.(check string) "the leader's files ingested as the store described them"
        (message (fun buffer ->
             Proto.bytes buffer 1 (message (fun buffer -> Kv.context buffer job.region));
             Proto.bytes buffer 2 (sst "first");
             Proto.bytes buffer 2 (sst "second")))
        (Import.ingest_request job.region ssts)
  | Ok _ | Error _ -> Alcotest.fail "the first wrote, the second refused"

let ingest () =
  let check name expected text =
    Alcotest.(check string) name
      (Base.Sexp.to_string ([%sexp_of: [ `Ingested | `Failed of Kv.region_error ]] expected))
      (match Import.ingested text with
      | Ok Ingested -> Base.Sexp.to_string ([%sexp_of: [ `Ingested | `Failed of Kv.region_error ]] `Ingested)
      | Ok (Ingest_failed error) -> Base.Sexp.to_string ([%sexp_of: [ `Ingested | `Failed of Kv.region_error ]] (`Failed error))
      | Error (`Proto _) -> "not protobuf")
  in
  Alcotest.(check bool) "a store's reason in its response" true
    (Import.written (message (fun buffer -> Proto.bytes buffer 1 (message (fun buffer -> Proto.bytes buffer 1 "region stale")))) = Ok (Write_failed "region stale"));
  check "taken" `Ingested "";
  check "epoch not match" (`Failed Kv.Epoch_not_match) (message (fun buffer -> Proto.bytes buffer 1 (message (fun buffer -> Proto.bytes buffer 1 "e"; Proto.bytes buffer 5 ""))));
  check "a full disk" (`Failed (Kv.Other { kind = 15; message = "full" }))
    (message (fun buffer -> Proto.bytes buffer 1 (message (fun buffer -> Proto.bytes buffer 1 "full"; Proto.bytes buffer 15 ""))));
  let result, heard, _ =
    Test_http2.session
      {
        initial_window = 65_535;
        max_frame = 16_384;
        exchanges =
          [
            {
              path = "/import_sstpb.ImportSST/SwitchMode";
              headers = [];
              body = Test_kv.framed (message (fun buffer -> Proto.uint buffer 1 1L));
              reply = Test_kv.reply ~trailers:[ ("grpc-status", "0") ] (Test_kv.framed "");
            };
          ];
      }
      (fun connection -> Import.switch_mode connection Import)
  in
  Alcotest.(check (list string)) "faults" [] heard.faults;
  Alcotest.(check bool) "answered" true (result = Ok ());
  Alcotest.(check (list string)) "import mode asked for" [ Test_kv.framed (message (fun buffer -> Proto.uint buffer 1 1L)) ]
    (List.map snd heard.requests)

let held () =
  let run ~every ~refuse work =
    let log = Portable.Atomic.make [] in
    let rec record entry =
      let seen = Portable.Atomic.get log in
      match Portable.Atomic.compare_and_set log ~if_phys_equal_to:seen ~replace_with:(entry :: seen) with
      | Set_here -> ()
      | Compare_failed -> record entry
    in
    let switch mode =
      let asked = List.length (List.filter (fun mode -> mode = Import.Import) (Portable.Atomic.get log)) in
      record mode;
      match mode with Import.Import when refuse asked -> Error `Refused | Import | Normal -> Ok ()
    in
    let started = Unix.gettimeofday () in
    let result =
      Quiver_core.Concurrent.held ~every:(Core.Time_ns.Span.of_sec every) ~on:Import.Import ~off:Normal switch (fun () ->
          work (fun () -> List.rev (Portable.Atomic.get log)))
    in
    (result, List.rev (Portable.Atomic.get log), Unix.gettimeofday () -. started)
  in
  let show modes = String.concat " " (List.map (function Import.Import -> "import" | Normal -> "normal") modes) in
  let result, log, _ =
    run ~every:0.03 ~refuse:(fun _ -> false) (fun seen ->
        let before = seen () in
        Unix.sleepf 0.25;
        before)
  in
  Alcotest.(check (option string)) "import mode before the work" (Some "import") (Option.map show (Result.to_option result));
  let imports = List.length (List.filter (fun mode -> mode = Import.Import) log) in
  Alcotest.(check bool) (Printf.sprintf "asked again while it runs (%d times)" imports) true (imports >= 4);
  Alcotest.(check int) "normal once" 1 (List.length (List.filter (fun mode -> mode = Import.Normal) log));
  Alcotest.(check (option string)) "nothing after normal" (Some "normal") (Option.map (fun mode -> show [ mode ]) (Base.List.last log));
  let result, log, _ = run ~every:0.03 ~refuse:(fun asked -> asked = 0) (fun _ -> Alcotest.fail "the work ran") in
  Alcotest.(check bool) "the first refusal counts, the stores asked back" true (result = Error `Refused && show log = "import normal");
  let result, log, _ =
    run ~every:0.02 ~refuse:(fun asked -> asked = 1) (fun _ ->
        Unix.sleepf 0.1;
        7)
  in
  Alcotest.(check bool) "a later refusal does not" true (result = Ok 7 && Base.List.last log = Some Import.Normal);
  let result, log, took =
    run ~every:10. ~refuse:(fun _ -> false) (fun _ ->
        Unix.sleepf 0.1;
        8)
  in
  Alcotest.(check bool) (Printf.sprintf "done when the work is (%.2f s)" took) true (result = Ok 8 && show log = "import normal" && took < 1.)

let packed =
  let key = QCheck2.Gen.(string_size ~gen:(oneof_list [ 'a'; 'b'; 'c'; '\x00'; '\xff' ]) (int_range 0 7)) in
  let pairs =
    QCheck2.Gen.(
      map
        (fun keys -> List.map (fun (key, length) -> { Kv.key; value = String.make length 'v' }) (List.sort_uniq (fun (a, _) (b, _) -> String.compare a b) keys))
        (list_size (int_range 0 700) (pair key (oneof_weighted [ (5, int_range 0 20); (1, int_range 0 5000) ]))))
  in
  QCheck2.Test.make ~count:500 ~name:"pairs packed in one buffer read back, split and cut as a list of them is"
    ~print:(fun (pairs, _, _, _) -> Base.Sexp.to_string ([%sexp_of: Kv.pair list] pairs))
    QCheck2.Gen.(quad pairs pairs (option key) (int_range 0 300))
    (fun (pairs, again, stop, bytes) ->
      let packed = Pairs.create () in
      List.iter (fun (pair : Kv.pair) -> Pairs.add packed pair.key pair.value) pairs;
      let first = Pairs.all packed in
      let read = Pairs.to_list first = pairs && Pairs.length first = List.length pairs in
      let bound = Option.fold ~none:Kv.Endless ~some:(fun key -> Kv.Before key) stop in
      let below, rest = Pairs.split first bound in
      let split =
        let inside (pair : Kv.pair) = match bound with Endless -> true | Before stop -> String.compare pair.key stop < 0 in
        Pairs.to_list below = List.filter inside pairs && Pairs.to_list rest = List.filter (fun pair -> not (inside pair)) pairs
      in
      let rec runs acc current size = function
        | [] -> List.rev (match current with [] -> acc | _ :: _ -> List.rev current :: acc)
        | (pair : Kv.pair) :: others ->
            let cost = String.length pair.key + String.length pair.value in
            if current <> [] && size + cost > bytes then runs (List.rev current :: acc) [ pair ] cost others
            else runs acc (pair :: current) (size + cost) others
      in
      let cut = List.map Pairs.to_list (Pairs.cut rest ~bytes) = runs [] [] 0 (Pairs.to_list rest) in
      let lengths =
        List.for_all2
          (fun (pair : Kv.pair) i -> Pairs.key first i = pair.key && Pairs.key_length first i = String.length pair.key && Pairs.value_length first i = String.length pair.value)
          pairs (List.init (List.length pairs) Fun.id)
      in
      Pairs.clear packed;
      List.iter (fun (pair : Kv.pair) -> Pairs.add packed pair.key pair.value) again;
      read && split && cut && lengths && Pairs.to_list (Pairs.all packed) = again)

let encoded =
  let kv = QCheck2.Gen.(map2 (fun key value -> { Kv.key; value }) (string_size (int_range 1 200)) (string_size (int_range 0 300))) in
  QCheck2.Test.make ~count:500 ~name:"a write's batches read back as the pairs they carry, at the job's version"
    QCheck2.Gen.(pair (list_size (int_range 1 5) (list_size (int_range 1 40) kv)) (map Int64.of_int nat))
    (fun (batches, commit_ts) ->
      let job : Import.job = { job with commit_ts; pairs = List.map Pairs.of_list batches } in
      let field text (pos, len) number =
        Result.get_ok
          (Proto.fold text ~pos ~len ~init:[] ~f:(fun acc field value -> if field = number then Ok (value :: acc) else Ok acc))
        |> List.rev
      in
      let slice text = function Proto.Delimited { pos; len } -> (pos, len) | _ -> (0, String.length text) in
      match Import.write_requests job with
      | [] -> false
      | _ :: messages ->
          List.length messages = List.length batches
          && List.for_all2
               (fun message batch ->
                 match field message (0, String.length message) 2 with
                 | [ inside ] ->
                     let inside = slice message inside in
                     field message inside 1 = [ Proto.Varint commit_ts ]
                     && List.map
                          (fun pair ->
                            let pair = slice message pair in
                            match (field message pair 1, field message pair 2) with
                            | [ Delimited { pos; len } ], [ Delimited value ] ->
                                { Kv.key = String.sub message pos len; value = String.sub message value.pos value.len }
                            | _ -> { key = ""; value = "broken" })
                          (field message inside 2)
                        = batch
                 | _ -> false)
               messages batches)

let stores () =
  let listener = Unix.socket ~cloexec:true PF_INET6 SOCK_STREAM 0 in
  Unix.bind listener (ADDR_INET (Unix.inet6_addr_loopback, 0));
  Unix.listen listener 4;
  let port = match Unix.getsockname listener with ADDR_INET (_, port) -> port | ADDR_UNIX _ -> Alcotest.fail "not TCP" in
  let stores = Stores.create { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } in
  let address = Printf.sprintf "[::1]:%d" port in
  let first = Stores.connection stores address in
  let again = Stores.connection stores address in
  Alcotest.(check bool) "an IPv6 store in brackets, once" true
    (match (first, again) with Ok first, Ok again -> first == again | _ -> false);
  let malformed text = match Stores.connection stores text with Error (`Region (Region.Malformed _)) -> true | Ok _ | Error _ -> false in
  Alcotest.(check bool) "no port" true (malformed "tikv-1");
  Alcotest.(check bool) "a port not a number" true (malformed "tikv-1:x");
  Stores.close stores;
  Unix.close listener

let ranges =
  let first, past = Codec.table 75L in
  let inside = QCheck2.Gen.(map (fun tail -> first ^ tail) (string_size ~gen:(oneof_list [ '_'; 'i'; 'r'; '\x00'; '\xff' ]) (int_range 0 6))) in
  let bound = QCheck2.Gen.(oneof [ inside; return first; return past; return "t\x7f"; return "u"; map (fun key -> past ^ key) (string_size (int_range 1 3)) ]) in
  QCheck2.Test.make ~count:3000 ~name:"a table's keys split by regions cover it once, each key in one part"
    ~print:(fun (bounds, keys) -> Base.Sexp.to_string ([%sexp_of: string list * string list] (bounds, keys)))
    QCheck2.Gen.(pair (map (List.sort_uniq String.compare) (list_size (int_range 0 8) bound)) (list_size (int_range 0 20) inside))
    (fun (bounds, keys) ->
      let bounds = List.filter (fun bound -> bound <> "") bounds in
      let regions =
        List.mapi
          (fun i start : Region.t ->
            {
              context = { id = Int64.of_int i; conf_ver = 1L; version = 1L; peer = 1L; store = 1L };
              start;
              stop = Option.fold ~none:Kv.Endless ~some:(fun key -> Kv.Before key) (List.nth_opt bounds i);
              address = "a";
              replicas = [];
            })
          ("" :: bounds)
      in
      let parts = Mirror.ranges ~source:75L regions in
      let rec chained expected first_of last_of = function
        | [] -> expected
        | (part : Mirror.range) :: rest -> if first_of part = expected && String.compare (first_of part) (last_of part) < 0 then chained (last_of part) first_of last_of rest else "broken"
      in
      chained first (fun (part : Mirror.range) -> part.start) (fun part -> part.stop) parts = past
      && List.for_all
           (fun key ->
             String.compare key past >= 0
             || List.exists (fun (part : Mirror.range) -> String.compare part.start key <= 0 && String.compare key part.stop < 0) parts)
           keys)

let row_key table handle =
  fst (Codec.records table) ^ String.init 8 (fun i -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical handle (8 * (7 - i))) 0xFFL)))

let points =
  QCheck2.Test.make ~count:3000 ~name:"keys between two keys of rows are evenly apart by handle and cut the range whole"
    ~print:(fun (a, b, parts) -> Printf.sprintf "%Lx %Lx %d" a b parts)
    QCheck2.Gen.(triple (map Int64.of_int (int_range 0 1_000_000)) (oneof [ map Int64.of_int (int_range 0 2_000_000); return (-1L); return Int64.max_int ]) (int_range 1 20))
    (fun (a, b, parts) ->
      let low, high = if Int64.unsigned_compare a b <= 0 then (a, b) else (b, a) in
      let first = row_key 75L low and last = row_key 75L high in
      let points = Mirror.points ~first ~last ~parts in
      let handle key = Base.List.fold (List.init 8 (fun i -> i)) ~init:0L ~f:(fun acc i -> Int64.logor (Int64.shift_left acc 8) (Int64.of_int (Char.code key.[11 + i]))) in
      let span = Int64.sub high low in
      let quotient = Int64.unsigned_div span (Int64.of_int parts) and remainder = Int64.unsigned_rem span (Int64.of_int parts) in
      let rec ascending previous = function [] -> true | key :: rest -> String.compare previous key < 0 && ascending key rest in
      let near i key =
        let wanted =
          Int64.add low (Int64.add (Int64.mul quotient (Int64.of_int i)) (Int64.div (Int64.mul remainder (Int64.of_int i)) (Int64.of_int parts)))
        in
        let gap = Int64.sub (handle key) wanted in
        Int64.compare (Int64.abs gap) (Int64.of_int parts) <= 0
      in
      ascending first points
      && List.for_all (fun key -> String.compare key last <= 0) points
      && List.length points <= parts - 1
      && (Int64.unsigned_compare span (Int64.of_int parts) < 0 || List.length points = parts - 1)
      && Mirror.points ~first ~last:first ~parts = []
      && List.for_all (fun key -> List.exists (fun i -> near i key) (List.init parts (fun i -> i + 1))) points
      &&
      let start = fst (Codec.table 75L) and stop = snd (Codec.table 75L) in
      let whole : Mirror.range = { start; stop } in
      let parts = Mirror.divide whole points in
      let rec chained expected first_of last_of = function
        | [] -> expected
        | (part : Mirror.range) :: rest -> if first_of part = expected && String.compare (first_of part) (last_of part) < 0 then chained (last_of part) first_of last_of rest else "broken"
      in
      chained start (fun (part : Mirror.range) -> part.start) (fun part -> part.stop) parts = stop)

let neighbours () =
  let first = row_key 75L 0x00FFFFFFFFFFFFFFL and last = row_key 75L 0x0100000000000000L in
  Alcotest.(check (list string)) "no key between neighbours that differ from the first byte" [] (Mirror.points ~first ~last ~parts:4)

let cuts =
  QCheck2.Test.make ~count:3000 ~name:"split keys go to the region holding them strictly inside, each once, in order"
    ~print:(fun (bounds, keys) -> Base.Sexp.to_string ([%sexp_of: string list * string list] (bounds, keys)))
    QCheck2.Gen.(pair (map (List.sort_uniq String.compare) (list_size (int_range 0 6) key)) (map (List.sort_uniq String.compare) (list_size (int_range 0 15) key)))
    (fun (bounds, keys) ->
      let regions =
        List.mapi
          (fun i start : Region.t ->
            {
              context = { id = Int64.of_int i; conf_ver = 1L; version = 1L; peer = 1L; store = 1L };
              start;
              stop = Option.fold ~none:Kv.Endless ~some:(fun key -> Kv.Before key) (List.nth_opt bounds i);
              address = "a";
              replicas = [];
            })
          ("" :: bounds)
      in
      let cuts = Mirror.cuts regions keys in
      let assigned = List.concat_map snd cuts in
      assigned = List.filter (fun key -> not (List.mem key bounds) && key <> "") keys
      && List.for_all
           (fun ((region : Region.t), keys) ->
             keys <> [] && List.for_all (fun key -> Region.find [ region ] key <> None && key <> region.start) keys)
           cuts)

let tests =
  [
    QCheck_alcotest.to_alcotest cuts;
    Alcotest.test_case "split points between neighbouring rows" `Quick neighbours;
    QCheck_alcotest.to_alcotest points;
    QCheck_alcotest.to_alcotest ranges;
    Alcotest.test_case "connections to stores by address" `Quick stores;
    Alcotest.test_case "stores held in import mode while the work runs" `Quick held;
    QCheck_alcotest.to_alcotest packed;
    QCheck_alcotest.to_alcotest encoded;
    Alcotest.test_case "write requests carry the meta, then the batches" `Quick requests;
    Alcotest.test_case "a write to every peer at once" `Quick write;
    Alcotest.test_case "ingest responses and the import mode" `Quick ingest;
    Alcotest.test_case "only the leader's files are ingested" `Quick leader_files;
    Alcotest.test_case "deliveries that give up" `Quick refusals;
    QCheck_alcotest.to_alcotest delivers;
  ]
