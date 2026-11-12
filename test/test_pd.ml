open Sexplib0.Sexp_conv
open Quiver_tikv

let message fill =
  let buffer = Buffer.create 64 in
  fill buffer;
  Buffer.contents buffer

let ok body = Test_kv.reply ~trailers:[ ("grpc-status", "0") ] (Test_kv.framed body)

let exchange name body reply : Test_http2.exchange = { path = "/pdpb.PD/" ^ name; headers = []; body = Test_kv.framed body; reply }

let header ?refusal cluster =
  message (fun buffer ->
      Proto.uint buffer 1 cluster;
      Option.iter
        (fun (kind, text) -> Proto.bytes buffer 2 (message (fun buffer -> Proto.uint buffer 1 kind; Proto.bytes buffer 2 text)))
        refusal)

let request cluster fill = message (fun buffer -> Proto.bytes buffer 1 (message (fun buffer -> Proto.uint buffer 1 cluster)); fill buffer)

let members ?(scheme = "http") port =
  message (fun buffer ->
      Proto.bytes buffer 1 (header 42L);
      Proto.bytes buffer 3 (message (fun buffer -> Proto.bytes buffer 1 "pd-1"; Proto.bytes buffer 4 (Printf.sprintf "%s://127.0.0.1:%d" scheme port))))

let stores =
  message (fun buffer ->
      Proto.bytes buffer 1 (header 42L);
      List.iter
        (fun (id, address, peer_address, labels) ->
          Proto.bytes buffer 2
            (message (fun buffer ->
                 Proto.uint buffer 1 id;
                 Proto.bytes buffer 2 address;
                 List.iter (fun (key, value) -> Proto.bytes buffer 4 (message (fun buffer -> Proto.bytes buffer 1 key; Proto.bytes buffer 2 value))) labels;
                 Option.iter (Proto.bytes buffer 6) peer_address)))
        [
          (1L, "tikv-1:20160", None, []);
          (3L, "tiflash-3:3930", Some "tiflash-3:20170", [ ("zone", "z1"); ("engine", "tiflash") ]);
          (2L, "tikv-2:20160", Some "", [ ("zone", "tiflash"); ("tiflash", "z1"); ("engine", "tikv") ]);
        ])

(* A region as PD describes it: raw bounds, its epoch, peers by id and store, the leader's peer id. *)
type region = { id : int64; first : string; past : string; peers : (int64 * int64) list; leader : int64 option } [@@deriving sexp_of]

let encoded key = if key = "" then "" else Codec.encode_bytes key

let region_message region =
  message (fun buffer ->
      Proto.bytes buffer 1
        (message (fun buffer ->
             Proto.uint buffer 1 region.id;
             Proto.bytes buffer 2 (encoded region.first);
             Proto.bytes buffer 3 (encoded region.past);
             Proto.bytes buffer 4 (message (fun buffer -> Proto.uint buffer 1 3L; Proto.uint buffer 2 (Int64.add region.id 10L)));
             List.iter (fun (id, store) -> Proto.bytes buffer 5 (message (fun buffer -> Proto.uint buffer 1 id; Proto.uint buffer 2 store))) region.peers));
      Option.iter
        (fun leader ->
          let store = List.assoc leader region.peers in
          Proto.bytes buffer 2 (message (fun buffer -> Proto.uint buffer 1 leader; Proto.uint buffer 2 store)))
        region.leader)

let scanned ?refusal regions =
  message (fun buffer ->
      Proto.bytes buffer 1 (header ?refusal 42L);
      List.iter (fun region -> Proto.bytes buffer 4 (region_message region)) regions)

let scan_request ~from ~stop =
  request 42L (fun buffer ->
      Proto.bytes buffer 2 from;
      Proto.uint buffer 3 128L;
      Option.iter (fun stop -> Proto.bytes buffer 4 (Codec.encode_bytes stop)) stop)

let run ~start ~stop pages =
  let listener, port = Test_http2.listening () in
  let scenario : Test_http2.scenario =
    {
      initial_window = 65_535;
      max_frame = 16_384;
      exchanges =
        exchange "GetMembers" (request 0L ignore) (ok (members port))
        :: exchange "GetAllStores" (request 42L (fun buffer -> Proto.uint buffer 2 1L)) (ok stores)
        :: List.map (fun (from, response) -> exchange "ScanRegions" (scan_request ~from ~stop) (ok response)) pages;
    }
  in
  let heard = Support.start (fun () -> Test_http2.serve listener scenario) in
  let result =
    Result.bind (Pd.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } (Printf.sprintf "127.0.0.1:%d" port)) (fun pd ->
        let regions = Pd.regions pd ~start ~stop:(Option.fold ~none:Kv.Endless ~some:(fun key -> Kv.Before key) stop) in
        Pd.close pd;
        regions)
  in
  let heard = Support.join heard in
  Unix.close listener;
  (result, heard, scenario)

let expected regions : Region.t list =
  List.map
    (fun region ->
      let address store = if store = 3L then "tiflash-3:20170" else Printf.sprintf "tikv-%Ld:20160" store in
      let leader = Option.get region.leader in
      let store = List.assoc leader region.peers in
      {
        Region.context = { id = region.id; conf_ver = 3L; version = Int64.add region.id 10L; peer = leader; store };
        start = region.first;
        stop = (if region.past = "" then Endless else Before region.past);
        address = address store;
        replicas =
          List.map
            (fun (id, store) : Region.replica -> { peer = { id; store }; address = address store })
            (List.sort (fun (a, _) (b, _) -> Int64.compare a b) region.peers);
      })
    regions

let show = function
  | Ok regions -> Base.Sexp.to_string ([%sexp_of: Region.t list] regions)
  | Error (`Pd error) -> Base.Sexp.to_string (Pd.sexp_of_error error)
  | Error _ -> "another failure"

let key = QCheck2.Gen.(string_size ~gen:(oneof_list [ 'a'; 'b'; '\x00'; '\xff' ]) (int_range 1 10))

let pages =
  QCheck2.Test.make ~count:150
    ~name:"regions of a range come whole from PD's pages, in order, each asked from where the last ended, a TiFlash learner at its proxy's peer address"
    ~print:(fun (bounds, sizes, stop) -> Base.Sexp.to_string ([%sexp_of: string list * int list * string option] (bounds, sizes, stop)))
    QCheck2.Gen.(
      triple
        (map (List.sort_uniq String.compare) (list_size (int_range 0 8) key))
        (list_size (int_range 1 9) (int_range 1 3))
        (option key))
    (fun (bounds, sizes, stop) ->
      let firsts = "" :: bounds in
      let regions =
        List.mapi
          (fun i first ->
            let id = Int64.of_int (i + 1) in
            let peers = [ (Int64.mul id 10L, 2L); (Int64.add (Int64.mul id 10L) 1L, 1L); (Int64.add (Int64.mul id 10L) 2L, 3L) ] in
            { id; first; past = Option.value ~default:"" (List.nth_opt bounds i); peers; leader = Some (Int64.add (Int64.mul id 10L) 1L) })
          firsts
      in
      let wanted =
        List.filter
          (fun region -> match stop with None -> true | Some stop -> String.compare region.first stop < 0 || region.first = "")
          regions
      in
      let rec paged from regions sizes =
        match (regions, sizes) with
        | [], _ -> []
        | _, [] -> [ (from, regions) ]
        | _, size :: sizes ->
            let page = Base.List.take regions size and rest = Base.List.drop regions size in
            let last = Base.List.last_exn page in
            (from, page) :: (if rest = [] then [] else paged (encoded last.past) rest sizes)
      in
      let pages = List.map (fun (from, page) -> (from, scanned page)) (paged (Codec.encode_bytes "") wanted sizes) in
      let result, heard, scenario = run ~start:"" ~stop pages in
      heard.faults = []
      && List.map snd heard.requests = List.map (fun (exchange : Test_http2.exchange) -> exchange.body) scenario.exchanges
      && show result = show (Ok (expected wanted)))

let refusals () =
  let peers = [ (11L, 1L) ] in
  let one ?(leader = Some 11L) ?(peers = peers) id first past = { id; first; past; peers; leader } in
  let check name expected pages =
    let result, _, _ = run ~start:"" ~stop:None (List.map (fun page -> (Codec.encode_bytes "", page)) pages) in
    Alcotest.(check string) name (Base.Sexp.to_string (Pd.sexp_of_error expected)) (show result)
  in
  check "a refusal in the header" (Refused { kind = 6; message = "not found" }) [ scanned ~refusal:(6L, "not found") [] ];
  check "no regions at all" (Gap (Codec.encode_bytes "")) [ scanned [] ];
  check "a gap between two regions" (Gap (Codec.encode_bytes "m")) [ scanned [ one 1L "" "m"; one 2L "n" "" ] ];
  check "a region without a leader" (Leaderless 1L) [ scanned [ one ~leader:None 1L "" "" ] ];
  check "a region whose leader is empty" (Leaderless 1L)
    [
      message (fun buffer ->
          Proto.bytes buffer 1 (header 42L);
          Proto.bytes buffer 4 (region_message (one ~leader:None 1L "" "") ^ message (fun buffer -> Proto.bytes buffer 2 "")));
    ];
  check "a peer on a store PD did not list" (Unknown_store 9L) [ scanned [ one ~leader:(Some 11L) ~peers:[ (11L, 1L); (12L, 9L) ] 1L "" "" ] ];
  let listener, port = Test_http2.listening () in
  let scenario : Test_http2.scenario =
    {
      initial_window = 65_535;
      max_frame = 16_384;
      exchanges = [ exchange "GetMembers" (request 0L ignore) (ok (message (fun buffer -> Proto.bytes buffer 3 (message (fun buffer -> Proto.bytes buffer 4 "unix:///pd.sock"))))) ];
    }
  in
  let heard = Support.start (fun () -> Test_http2.serve listener scenario) in
  let result = Pd.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } (Printf.sprintf "127.0.0.1:%d" port) in
  let _ = Support.join heard in
  Unix.close listener;
  Alcotest.(check bool) "a leader's URL not over HTTP" true (match result with Error (`Pd (Pd.Url "unix:///pd.sock")) -> true | Ok _ | Error _ -> false)

let follower () =
  let leader_listener, leader_port = Test_http2.listening () and listener, port = Test_http2.listening () in
  let region = { id = 1L; first = ""; past = ""; peers = [ (11L, 1L) ]; leader = Some 11L } in
  let at_follower : Test_http2.scenario =
    { initial_window = 65_535; max_frame = 16_384; exchanges = [ exchange "GetMembers" (request 0L ignore) (ok (members leader_port)) ] }
  and at_leader : Test_http2.scenario =
    {
      initial_window = 65_535;
      max_frame = 16_384;
      exchanges =
        [
          exchange "GetAllStores" (request 42L (fun buffer -> Proto.uint buffer 2 1L)) (ok stores);
          exchange "ScanRegions" (scan_request ~from:(Codec.encode_bytes "") ~stop:None) (ok (scanned [ region ]));
        ];
    }
  in
  let heard = Support.start (fun () -> Test_http2.serve listener at_follower)
  and heard_leader = Support.start (fun () -> Test_http2.serve leader_listener at_leader) in
  let result =
    Result.bind (Pd.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } (Printf.sprintf "127.0.0.1:%d" port)) (fun pd ->
        let regions = Pd.regions pd ~start:"" ~stop:Endless in
        Pd.close pd;
        regions)
  in
  let heard = Support.join heard and heard_leader = Support.join heard_leader in
  Unix.close listener;
  Unix.close leader_listener;
  Alcotest.(check (list string)) "faults" [] (heard.faults @ heard_leader.faults);
  Alcotest.(check string) "the regions from the leader" (show (Ok (expected [ region ]))) (show result)

let urls () =
  let leader scheme =
    let listener, port = Test_http2.listening () in
    let scenario : Test_http2.scenario =
      {
        initial_window = 65_535;
        max_frame = 16_384;
        exchanges =
          [
            exchange "GetMembers" (request 0L ignore) (ok (members ~scheme port));
            exchange "GetAllStores" (request 42L (fun buffer -> Proto.uint buffer 2 1L)) (ok stores);
          ];
      }
    in
    let heard = Support.start (fun () -> Test_http2.serve listener scenario) in
    let result =
      Result.map
        (fun pd ->
          let stores = Pd.stores pd in
          Pd.close pd;
          stores)
        (Pd.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } (Printf.sprintf "127.0.0.1:%d" port))
    in
    let (_ : Test_http2.heard) = Support.join heard in
    Unix.close listener;
    match result with
    | Ok stores -> String.concat " " stores
    | Error (`Pd (Pd.Url url)) -> "refused " ^ Base.String.prefix url 6
    | Error _ -> "failed"
  in
  Alcotest.(check (list string)) "the leader's URL in the clear, under TLS and otherwise"
    [ "tikv-1:20160 tikv-2:20160"; "tikv-1:20160 tikv-2:20160"; "refused ftp://" ]
    (List.map leader [ "http"; "https"; "ftp" ])

let scatter () =
  let listener, port = Test_http2.listening () in
  let scenario : Test_http2.scenario =
    {
      initial_window = 65_535;
      max_frame = 16_384;
      exchanges =
        [
          exchange "GetMembers" (request 0L ignore) (ok (members port));
          exchange "GetAllStores" (request 42L (fun buffer -> Proto.uint buffer 2 1L)) (ok stores);
          exchange "ScatterRegion"
            (request 42L (fun buffer ->
                 Proto.bytes buffer 5 "quiver";
                 List.iter (Proto.uint buffer 6) [ 3L; 4L ];
                 Proto.uint buffer 7 3L))
            (ok
               (message (fun buffer ->
                    Proto.bytes buffer 1 (header 42L);
                    Proto.uint buffer 2 50L;
                    Proto.uint buffer 3 4L;
                    Proto.bytes buffer 3 (message (fun buffer -> Buffer.add_string buffer "\x05\x96\x01")))));
        ];
    }
  in
  let heard = Support.start (fun () -> Test_http2.serve listener scenario) in
  let result =
    Result.bind (Pd.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } (Printf.sprintf "127.0.0.1:%d" port)) (fun pd ->
        let failed = Pd.scatter pd [ 3L; 4L ] in
        let stores = Pd.stores pd in
        Pd.close pd;
        Result.map (fun failed -> (failed, stores)) failed)
  in
  let heard = Support.join heard in
  Unix.close listener;
  Alcotest.(check (list string)) "faults" [] heard.faults;
  Alcotest.(check bool) "the regions asked as one group, the one it could not" true
    (Result.map (fun (failed, _) -> List.sort Int64.compare failed) result = Ok [ 4L; 5L; 150L ]
    && List.map snd heard.requests = List.map (fun (exchange : Test_http2.exchange) -> exchange.body) scenario.exchanges);
  Alcotest.(check (list string)) "the TiKV stores in PD's order, without TiFlash's" [ "tikv-1:20160"; "tikv-2:20160" ]
    (Result.fold ~ok:snd ~error:(fun _ -> []) result)

let keep () =
  let listener, port = Test_http2.listening () in
  let asked ttl version =
    request 42L (fun buffer ->
        Proto.bytes buffer 2 "quiver_mirror_x";
        Proto.uint buffer 3 ttl;
        Proto.uint buffer 4 version)
  and least value = ok (message (fun buffer -> Proto.bytes buffer 1 (header 42L); Proto.bytes buffer 2 "gc_worker"; Proto.uint buffer 3 99L; Proto.uint buffer 4 value)) in
  let scenario : Test_http2.scenario =
    {
      initial_window = 65_535;
      max_frame = 16_384;
      exchanges =
        [
          exchange "GetMembers" (request 0L ignore) (ok (members port));
          exchange "GetAllStores" (request 42L (fun buffer -> Proto.uint buffer 2 1L)) (ok stores);
          exchange "UpdateServiceGCSafePoint" (asked 180L 1000L) (least 900L);
          exchange "UpdateServiceGCSafePoint" (asked 180L 1000L) (least 1000L);
          exchange "UpdateServiceGCSafePoint" (asked 180L 1000L) (least 1001L);
          exchange "UpdateServiceGCSafePoint" (asked 0L 1000L) (least 1001L);
        ];
    }
  in
  let heard = Support.start (fun () -> Test_http2.serve listener scenario) in
  let result =
    Result.map
      (fun pd ->
        let hold = Pd.Hold_for (Core.Time_ns.Span.of_sec 179.5) in
        let results = List.map (fun hold -> Pd.keep pd ~service:"quiver_mirror_x" hold 1000L) [ hold; hold; hold; Let_go ] in
        Pd.close pd;
        results)
      (Pd.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } (Printf.sprintf "127.0.0.1:%d" port))
  in
  let heard = Support.join heard in
  Unix.close listener;
  Alcotest.(check (list string)) "faults" [] heard.faults;
  Alcotest.(check bool) "held while the least kept is not later, refused once it is, let go with no time"
    true
    (result = Ok [ Ok (); Ok (); Error (`Pd (Pd.Gc_passed { version = 1000L; least = 1001L })); Ok () ]
    && List.map snd heard.requests = List.map (fun (exchange : Test_http2.exchange) -> exchange.body) scenario.exchanges)

let tso () =
  let listener, port = Test_http2.listening () in
  let stamp ?(header = header 42L) physical logical =
    ok
      (message (fun buffer ->
           Proto.bytes buffer 1 header;
           Proto.uint buffer 2 1L;
           Proto.bytes buffer 3 (message (fun buffer -> Proto.uint buffer 1 physical; Proto.uint buffer 2 logical))))
  in
  let asked = request 42L (fun buffer -> Proto.uint buffer 2 1L) in
  let scenario : Test_http2.scenario =
    {
      initial_window = 65_535;
      max_frame = 16_384;
      exchanges =
        [
          exchange "GetMembers" (request 0L ignore) (ok (members port));
          exchange "GetAllStores" (request 42L (fun buffer -> Proto.uint buffer 2 1L)) (ok stores);
          exchange "Tso" asked (stamp 1_700_000_000_123L 77L);
          exchange "Tso" asked (ok (message (fun buffer -> Proto.bytes buffer 1 (header 42L))));
          exchange "Tso" asked (stamp ~header:(header ~refusal:(2L, "not leader") 42L) 1L 1L);
        ];
    }
  in
  let heard = Support.start (fun () -> Test_http2.serve listener scenario) in
  let result =
    Result.map
      (fun pd ->
        let results = List.init 3 (fun _ -> Pd.tso pd) in
        Pd.close pd;
        results)
      (Pd.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } (Printf.sprintf "127.0.0.1:%d" port))
  in
  let heard = Support.join heard in
  Unix.close listener;
  Alcotest.(check (list string)) "faults" [] heard.faults;
  Alcotest.(check bool) "physical milliseconds over 18 bits of logical, none, refused" true
    (result
     = Ok
         [
           Ok (Int64.logor (Int64.shift_left 1_700_000_000_123L 18) 77L);
           Error (`Pd Pd.No_timestamp);
           Error (`Pd (Pd.Refused { kind = 2; message = "not leader" }));
         ]
    && List.map snd heard.requests = List.map (fun (exchange : Test_http2.exchange) -> exchange.body) scenario.exchanges)

let held () =
  let run ~every ~refuse work =
    let log = Portable.Atomic.make [] and finished = Portable.Atomic.make `Running in
    let rec record entry =
      let seen = Portable.Atomic.get log in
      match Portable.Atomic.compare_and_set log ~if_phys_equal_to:seen ~replace_with:(entry :: seen) with
      | Set_here -> ()
      | Compare_failed -> record entry
    in
    let ask (hold : Pd.hold) =
      let holds = List.length (List.filter (function Pd.Hold_for _ -> true | Let_go -> false) (Portable.Atomic.get log)) in
      record hold;
      match hold with
      | Hold_for _ when refuse ~holds ~finished:(Portable.Atomic.get finished) -> Error `Refused
      | Hold_for _ | Let_go -> Ok ()
    in
    let started = Unix.gettimeofday () in
    let result =
      Pd.held ~every:(Core.Time_ns.Span.of_sec every) ask (fun () ->
          let result = work (fun () -> List.rev (Portable.Atomic.get log)) in
          Portable.Atomic.set finished `Finished;
          result)
    in
    (result, List.rev (Portable.Atomic.get log), Unix.gettimeofday () -. started)
  in
  let show holds =
    String.concat " " (List.map (function Pd.Hold_for span -> Printf.sprintf "hold %.2f" (Core.Time_ns.Span.to_sec span) | Let_go -> "go") holds)
  in
  let never ~holds:_ ~finished:_ = false in
  let result, log, _ =
    run ~every:0.03 ~refuse:never (fun seen ->
        let before = seen () in
        Unix.sleepf 0.25;
        Ok before)
  in
  Alcotest.(check (option string)) "held before the work, for three times the interval" (Some "hold 0.09") (Option.map show (Result.to_option result));
  let holds = List.length (List.filter (function Pd.Hold_for _ -> true | Let_go -> false) log) in
  Alcotest.(check bool) (Printf.sprintf "asked again while it runs (%d times)" holds) true (holds >= 5);
  Alcotest.(check bool) "let go once, last" true
    (List.length (List.filter (( = ) Pd.Let_go) log) = 1 && Base.List.last log = Some Let_go);
  let result, log, _ = run ~every:0.03 ~refuse:(fun ~holds ~finished:_ -> holds = 0) (fun _ -> Alcotest.fail "the work ran") in
  Alcotest.(check bool) "refused at first: no work, let go" true (result = Error `Refused && show log = "hold 0.09 go");
  let result, log, _ =
    run ~every:0.02 ~refuse:(fun ~holds ~finished -> match finished with `Running -> holds = 1 | `Finished -> false) (fun _ ->
        Unix.sleepf 0.1;
        Ok 7)
  in
  Alcotest.(check bool) "a refusal meanwhile alone does not fail it" true (result = Ok 7 && Base.List.last log = Some Pd.Let_go);
  let result, log, _ =
    run ~every:10. ~refuse:(fun ~holds:_ ~finished -> match finished with `Finished -> true | `Running -> false) (fun _ ->
        Unix.sleepf 0.05;
        Ok 8)
  in
  Alcotest.(check bool) "refused once the work is done: failed, let go" true (result = Error `Refused && show log = "hold 30.00 hold 30.00 go");
  let result, log, took =
    run ~every:10. ~refuse:never (fun _ ->
        Unix.sleepf 0.05;
        Error `Work)
  in
  Alcotest.(check bool) (Printf.sprintf "the work's own failure, let go, done when it is (%.2f s)" took) true
    (result = Error `Work && show log = "hold 30.00 go" && took < 1.)

let tests =
  [
    Alcotest.test_case "a version held from GC, refused once GC may have passed it, let go" `Quick keep;
    Alcotest.test_case "a version held while the work runs, checked after it" `Quick held;
    Alcotest.test_case "a timestamp from PD's oracle" `Quick tso;
    Alcotest.test_case "PD's refusals and broken answers" `Quick refusals;
    Alcotest.test_case "a follower sends the client to the leader" `Quick follower;
    Alcotest.test_case "the leader's URL in the clear or under TLS" `Quick urls;
    Alcotest.test_case "regions scattered as a group, the TiKV stores" `Quick scatter;
    QCheck_alcotest.to_alcotest pages;
  ]
