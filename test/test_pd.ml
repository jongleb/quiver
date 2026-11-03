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

let members port =
  message (fun buffer ->
      Proto.bytes buffer 1 (header 42L);
      Proto.bytes buffer 3 (message (fun buffer -> Proto.bytes buffer 1 "pd-1"; Proto.bytes buffer 4 (Printf.sprintf "http://127.0.0.1:%d" port))))

let stores =
  message (fun buffer ->
      Proto.bytes buffer 1 (header 42L);
      List.iter
        (fun (id, address, labels) ->
          Proto.bytes buffer 2
            (message (fun buffer ->
                 Proto.uint buffer 1 id;
                 Proto.bytes buffer 2 address;
                 List.iter (fun (key, value) -> Proto.bytes buffer 4 (message (fun buffer -> Proto.bytes buffer 1 key; Proto.bytes buffer 2 value))) labels)))
        [
          (1L, "tikv-1:20160", []);
          (3L, "tiflash-3:3930", [ ("zone", "z1"); ("engine", "tiflash") ]);
          (2L, "tikv-2:20160", [ ("zone", "tiflash"); ("tiflash", "z1"); ("engine", "tikv") ]);
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
    Result.bind (Pd.connect ~timeout:(Core.Time_ns.Span.of_int_sec 5) (Printf.sprintf "127.0.0.1:%d" port)) (fun pd ->
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
      let address store = Printf.sprintf "tikv-%Ld:20160" store in
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
  QCheck2.Test.make ~count:150 ~name:"regions of a range come whole from PD's pages, in order, each asked from where the last ended"
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
            let peers = [ (Int64.mul id 10L, 2L); (Int64.add (Int64.mul id 10L) 1L, 1L) ] in
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
  let result = Pd.connect ~timeout:(Core.Time_ns.Span.of_int_sec 5) (Printf.sprintf "127.0.0.1:%d" port) in
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
    Result.bind (Pd.connect ~timeout:(Core.Time_ns.Span.of_int_sec 5) (Printf.sprintf "127.0.0.1:%d" port)) (fun pd ->
        let regions = Pd.regions pd ~start:"" ~stop:Endless in
        Pd.close pd;
        regions)
  in
  let heard = Support.join heard and heard_leader = Support.join heard_leader in
  Unix.close listener;
  Unix.close leader_listener;
  Alcotest.(check (list string)) "faults" [] (heard.faults @ heard_leader.faults);
  Alcotest.(check string) "the regions from the leader" (show (Ok (expected [ region ]))) (show result)

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
    Result.bind (Pd.connect ~timeout:(Core.Time_ns.Span.of_int_sec 5) (Printf.sprintf "127.0.0.1:%d" port)) (fun pd ->
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

let tests =
  [
    Alcotest.test_case "PD's refusals and broken answers" `Quick refusals;
    Alcotest.test_case "a follower sends the client to the leader" `Quick follower;
    Alcotest.test_case "regions scattered as a group, the TiKV stores" `Quick scatter;
    QCheck_alcotest.to_alcotest pages;
  ]
