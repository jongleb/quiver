open Sexplib0.Sexp_conv
open Quiver_tikv

let key = QCheck2.Gen.(string_size ~gen:(oneof_list [ 'a'; 'b'; '\x00'; '\xff' ]) (int_range 0 12))

let rows_end =
  QCheck2.Test.make ~count:2000 ~name:"a table's rows end where the next table starts, never at _s: a rowless region would stop TiFlash"
    ~print:(fun (id, rest) -> Printf.sprintf "%Ld %S" id rest)
    QCheck2.Gen.(
      pair
        (oneof [ map Int64.of_int (int_range (-300) 300); oneof_list [ Int64.max_int; Int64.min_int; 255L; 256L; 65535L; -1L ]; int64 ])
        (string_size ~gen:char (int_range 0 12)))
    (fun (id, rest) ->
      let first, past = Codec.table id in
      Codec.kind_end (first ^ "_r" ^ rest) = if Int64.equal id Int64.max_int then None else Some past)

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
  Alcotest.(check (option string)) "the rows end where the next table starts" (Some (snd (Codec.table 75L)))
    (Codec.kind_end (table ^ "_r\x80\x00\x00\x00\x00\x00\x00\x07"));
  Alcotest.(check (option string)) "the rows of table 255 end at table 256" (Some (snd (Codec.table 255L)))
    (Codec.kind_end (fst (Codec.table 255L) ^ "_r\x80"));
  Alcotest.(check (option string)) "a key of no known kind" None (Codec.kind_end (table ^ "_x"));
  Alcotest.(check string) "an index key of table 75 as table 300's"
    "t\x80\x00\x00\x00\x00\x00\x01\x2c_i\x80\x00\x00\x00\x00\x00\x00\x01x"
    (Codec.retabled ~into:300L "t\x80\x00\x00\x00\x00\x00\x00\x4b_i\x80\x00\x00\x00\x00\x00\x00\x01x")

let regions () =
  let region id start stop : Region.t =
    { context = { id; conf_ver = 1L; version = 1L; peer = 1L; store = 1L }; start; stop; address = "a"; replicas = [] }
  in
  let known = [ region 3L "" (Before "m"); region 2L "m" Endless ] in
  let found key = Option.fold ~none:"" ~some:(fun (region : Region.t) -> Int64.to_string region.context.id) (Region.find known key) in
  Alcotest.(check string) "a key in the first" "3" (found "a");
  Alcotest.(check string) "the bound in the second" "2" (found "m")

(* A region of the model TiKV, as it is now. *)
type held = { id : int64; version : int64; leader : int64; first : string; past : Kv.bound } [@@deriving sexp_of]

type event = Split of string | Merge of int | Move of int | Busy of int | Quiet [@@deriving sexp_of]

(* What a secondary key of an async-commit transaction shows when checked. *)
type secondary = All_present | One_missing | One_committed of int | One_two_phase [@@deriving sexp_of]

(* A transaction holding locks when the read starts; times are physical milliseconds. *)
type kind =
  | Committed_at of int  (** Its primary committed, its secondaries still locked. *)
  | Rolled_back_already
  | Alive of { min_commit : int; commits : (int * int) option }  (** At a time, at a version. *)
  | Primary_missing of { arrives : int option }  (** Its primary lock not written yet. *)
  | Async of { min_commits : int list; secondary : secondary }
[@@deriving sexp_of]

type txn = { keys : int list; start : int; ttl : int; kind : kind } [@@deriving sexp_of]

type case = {
  keys : string list;
  bounds : string list;
  start : string;
  stop : Kv.bound;
  limit : int;
  events : event list;
  txns : txn list;
}
[@@deriving sexp_of]

let read_at = 10_000
let begun = 10_100
let stamp physical = Int64.shift_left (Int64.of_int physical) 18
let cost = function Split _ | Merge _ | Move _ -> 1 | Busy n -> n | Quiet -> 0

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
             return Quiet;
           ])
    and* order = shuffle_list (List.init 60 (fun i -> i))
    and* shapes =
      list_size (int_range 0 5)
        (let* size = int_range 1 3
         and* start = int_range 9_000 9_999
         and* expiry = oneof [ int_range (begun - 50) (begun + 3_000); return 0 ]
         and* version = int_range (read_at - 50) (read_at + 50)
         and* time = int_range begun (begun + 4_000)
         and* kind = int_range 0 4
         and* pushable = bool
         and* plan = bool
         and* min_commits = list_size (return 3) (int_range (read_at - 30) (read_at + 10))
         and* secondary = int_range 0 3 in
         let ttl = if expiry = 0 then 0 else expiry - start in
         let min_commit = if pushable then start + 1 else 0 in
         let commits = if plan then Some (time, version) else None in
         let arrives = if plan then Some time else None in
         let min_commits = Base.List.take min_commits size in
         let greatest = List.fold_left Int.max 0 min_commits in
         let secondary =
           match secondary with
           | _ when size = 1 -> All_present
           | 0 -> All_present
           | 1 -> One_missing
           | 2 -> One_committed (greatest + 5)
           | _ -> One_two_phase
         in
         let kind =
           match kind with
           | 0 -> Committed_at version
           | 1 -> Rolled_back_already
           | 2 -> Alive { min_commit; commits }
           | 3 -> Primary_missing { arrives }
           | _ -> Async { min_commits; secondary }
         in
         return (size, start, ttl, kind))
    in
    let count = List.length keys in
    let txns =
      List.rev
        (snd
           (Base.List.foldi shapes ~init:([], []) ~f:(fun i (taken, txns) (size, start, ttl, kind) ->
                let free = List.filter (fun i -> i < count && not (List.mem i taken)) order in
                let unique = (start / 8 * 8) + i in
                match Base.List.take free size with
                | chosen when List.length chosen = size -> (chosen @ taken, { keys = chosen; start = unique; ttl = (if ttl = 0 then 0 else ttl + start - unique); kind } :: txns)
                | _ -> (taken, txns))))
    in
    let events = List.rev (snd (List.fold_left (fun (spent, kept) event -> if spent + cost event > 15 then (spent, kept) else (spent + cost event, event :: kept)) (0, []) events)) in
    return { keys; bounds = List.filter (fun bound -> bound <> "") bounds; start; stop; limit; events; txns })

let before key (bound : Kv.bound) = match bound with Endless -> true | Before stop -> String.compare key stop < 0

(* A transaction of the model as it is now. *)
type state = Locked | Primary_unwritten | Done of int option  (** Committed at a version, or rolled back. *)

type live = { txn : txn; serial : int; mutable state : state; mutable min_commit : int }

let fate (live : live) =
  match (live.state, live.txn.kind) with
  | Done outcome, _ -> outcome
  | _, Async { min_commits; secondary } -> (
      match secondary with
      | All_present -> Some (List.fold_left Int.max 0 min_commits)
      | One_committed version -> Some version
      | One_missing | One_two_phase -> None)
  | Locked, Alive { commits = Some (_, version); _ } -> Some (Int.max version live.min_commit)
  | (Locked | Primary_unwritten), _ -> None

let run case =
  let next_id = ref 100L in
  let held =
    ref
      (List.mapi
         (fun i first ->
           { id = Int64.of_int (i + 1); version = 1L; leader = 1L; first; past = Option.fold ~none:Kv.Endless ~some:(fun key -> Kv.Before key) (List.nth_opt case.bounds i) })
         ("" :: case.bounds))
  in
  let clock = ref begun and logical = ref 0 in
  let lives =
    List.mapi
      (fun serial (txn : txn) ->
        let state, min_commit =
          match txn.kind with
          | Committed_at version -> (Done (Some version), 0)
          | Rolled_back_already -> (Done None, 0)
          | Alive { min_commit; _ } -> (Locked, min_commit)
          | Primary_missing _ -> (Primary_unwritten, 0)
          | Async { min_commits; _ } -> (Locked, List.hd min_commits)
        in
        { txn; serial; state; min_commit })
      case.txns
  in
  let busy = ref 0 and events = ref case.events and failures = ref 0 and debt = ref 0 and late = ref false and harmed = ref false in
  let alive (live : live) = live.txn.ttl <> 0 && live.txn.start + live.txn.ttl > !clock in
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
  in
  let tick () =
    List.iter
      (fun live ->
        match (live.state, live.txn.kind) with
        | Locked, Alive { commits = Some (time, version); _ } when !clock >= time -> live.state <- Done (Some (Int.max version live.min_commit))
        | Primary_unwritten, Primary_missing { arrives = Some time } when !clock >= time -> live.state <- Locked
        | _ -> ())
      lives
  in
  let key_of i = List.nth case.keys i in
  let owner key = List.find_opt (fun live -> List.exists (fun i -> key_of i = key) live.txn.keys) lives in
  let primary live = key_of (List.hd live.txn.keys) in
  let lock live key : Kv.lock option =
    let index = Base.List.findi_exn live.txn.keys ~f:(fun _ i -> key_of i = key) |> fst in
    let first = index = 0 in
    let base : Kv.lock = { primary = primary live; version = stamp live.txn.start; key; ttl = Int64.of_int live.txn.ttl; min_commit_ts = 0L; protocol = Two_phase } in
    match (live.state, live.txn.kind) with
    | Primary_unwritten, _ when first -> None
    | Done _, _ when first -> None
    | Locked, Async { min_commits; secondary } -> (
        let min_commit_ts = stamp (List.nth min_commits index) in
        let last = index = List.length live.txn.keys - 1 in
        match secondary with
        | One_missing when last && not first -> None
        | One_committed _ when last && not first -> None
        | One_two_phase when last && not first -> Some { base with min_commit_ts }
        | All_present | One_missing | One_committed _ | One_two_phase ->
            let others = if first then List.map key_of (List.tl live.txn.keys) else [] in
            Some { base with min_commit_ts; protocol = Async_commit others })
    | Locked, _ when first -> Some { base with min_commit_ts = stamp live.min_commit }
    | (Locked | Primary_unwritten | Done _), _ -> Some base
  in
  let committed key =
    Option.bind (owner key) (fun live ->
        let index = fst (Base.List.findi_exn live.txn.keys ~f:(fun _ i -> key_of i = key)) in
        let last = index = List.length live.txn.keys - 1 && index > 0 in
        match (live.state, live.txn.kind) with
        | Done (Some version), _ when index = 0 -> Some version
        | Locked, Async { secondary = One_committed version; _ } when last -> Some version
        | _ -> None)
  in
  let value key = Option.fold ~none:("v" ^ key) ~some:(fun live -> Printf.sprintf "t%d%s" live.serial key) (owner key) in
  let visible key =
    match committed key with Some version when version <= read_at -> value key | Some _ | None -> "v" ^ key
  in
  let known region : Region.t =
    { context = { id = region.id; conf_ver = 1L; version = region.version; peer = region.leader; store = 1L }; start = region.first; stop = region.past; address = "model"; replicas = [] }
  in
  let cache = ref (List.map known !held) in
  let beyond (stop : Kv.bound) (past : Kv.bound) =
    match (stop, past) with _, Endless -> false | Endless, Before _ -> true | Before a, Before b -> String.compare a b > 0
  in
  let serve (region : Region.t) ~start ~stop answer =
    if !debt > 0 then late := true;
    let () = match !events with event :: rest -> events := rest; apply event | [] -> () in
    tick ();
    let fail error =
      incr failures;
      incr debt;
      Ok (Kv.Region_error error)
    in
    match List.find_opt (fun held -> held.id = region.context.id) !held with
    | None -> fail Region_not_found
    | Some current when current.version <> region.context.version -> fail Epoch_not_match
    | Some current when current.leader <> region.context.peer -> fail (Not_leader (Some { id = current.leader; store = 1L }))
    | Some _ when !busy > 0 ->
        decr busy;
        fail (Server_is_busy { reason = "model"; backoff_ms = 0L })
    | Some current when String.compare start current.first < 0 || beyond stop current.past -> fail (Other { kind = 4; message = "key not in region" })
    | Some _ -> Ok (answer ())
  in
  let by_start version = List.find (fun live -> stamp live.txn.start = version) lives in
  let store : _ Read.store =
    {
      locate =
        (fun freshness key ->
          let () = match freshness with Afresh -> cache := List.map known !held | Known -> () in
          Option.to_result ~none:(`Read (Read.Unheld key)) (Region.find !cache key));
      scan =
        (fun region (settled : Kv.settled) ~start ~stop ->
          serve region ~start ~stop (fun () ->
              let entries =
                List.filter_map
                  (fun key ->
                    if not (String.compare start key <= 0 && before key stop) then None
                    else
                      match Option.bind (owner key) (fun live -> Option.map (fun lock -> (live, lock)) (lock live key)) with
                      | Some (_, (lock : Kv.lock))
                        when Int64.compare lock.min_commit_ts (stamp read_at) <= 0 && not (List.mem lock.version settled.ignored) ->
                          if List.mem lock.version settled.committed then Some (`Pair { Kv.key; value = value key }) else Some (`Lock lock)
                      | Some _ | None -> Some (`Pair { Kv.key; value = visible key }))
                  case.keys
              in
              let taken = Base.List.take entries case.limit in
              match List.filter_map (function `Lock lock -> Some lock | `Pair _ -> None) taken with
              | first :: rest -> Kv.Reply (Kv.Locks (first, rest))
              | [] -> Reply (Kv.Pairs (List.filter_map (function `Pair pair -> Some pair | `Lock _ -> None) taken))));
      check =
        (fun region (check : Kv.check) ->
          serve region ~start:check.primary ~stop:(Before (check.primary ^ "\x00")) (fun () ->
              let live = by_start check.version in
              let expired () =
                match check.current with Past_every_ttl -> true | At now -> live.txn.start + live.txn.ttl < Int64.to_int (Int64.shift_right_logical now 18)
              in
              let caller = Int64.to_int (Int64.shift_right_logical check.caller 18) in
              let uncommitted protocol =
                let () = if live.min_commit <> 0 && caller >= live.min_commit then live.min_commit <- caller + 1 in
                let pushed : Kv.pushed = if live.min_commit > caller then Pushed else Not_pushed in
                Kv.Reply (Kv.Uncommitted { ttl = Int64.of_int live.txn.ttl; pushed; min_commit_ts = stamp live.min_commit; protocol })
              in
              match (live.state, live.txn.kind, check.sync) with
              | Done (Some version), _, _ -> Kv.Reply (Kv.Committed (stamp version))
              | Done None, _, _ -> Kv.Reply Kv.Rolled_back
              | Primary_unwritten, _, _ -> (
                  match check.missing with
                  | Rollback_if_missing ->
                      if alive live then harmed := true;
                      live.state <- Done None;
                      Kv.Reply Kv.Rolled_back
                  | Leave_if_missing -> Kv.Key_error Kv.Txn_not_found)
              | Locked, Async _, As_locked ->
                  Option.fold ~none:(Kv.Reply Kv.Rolled_back)
                    ~some:(fun (lock : Kv.lock) ->
                      Kv.Reply (Kv.Uncommitted { ttl = lock.ttl; pushed = Not_pushed; min_commit_ts = lock.min_commit_ts; protocol = lock.protocol }))
                    (lock live (primary live))
              | Locked, _, _ when expired () ->
                  live.state <- Done None;
                  Kv.Reply Kv.Rolled_back
              | Locked, Async _, Sync_commit_forced -> uncommitted (Async_commit [])
              | Locked, _, _ -> uncommitted Two_phase));
      secondaries =
        (fun region ~version keys ->
          let least = List.fold_left Base.String.min (List.hd keys) keys and greatest = List.fold_left Base.String.max (List.hd keys) keys in
          serve region ~start:least ~stop:(Before (greatest ^ "\x00")) (fun () ->
              let live = by_start version in
              if alive live then harmed := true;
              let rec go found = function
                | [] -> Kv.Reply (Kv.Locks_found (List.rev found))
                | key :: rest -> (
                    match (lock live key, committed key) with
                    | _, Some version -> Kv.Reply (Kv.Committed_secondary (stamp version))
                    | Some lock, None -> go (lock :: found) rest
                    | None, None ->
                        live.state <- Done None;
                        Kv.Reply (Kv.Locks_found []))
              in
              go [] keys));
      now =
        (fun () ->
          incr logical;
          Ok (Int64.add (stamp !clock) (Int64.of_int !logical)));
      pause =
        (fun span ->
          if !debt > 0 then decr debt;
          clock := !clock + Core.Time_ns.Span.to_int_ms span);
    }
  in
  let read = ref [] in
  let result = Read.range store ~version:(stamp read_at) ~start:case.start ~stop:case.stop ~limit:case.limit ~f:(fun pairs -> read := List.rev_append pairs !read; Ok ()) in
  let truth key =
    match owner key with
    | Some live -> Option.fold ~none:("v" ^ key) ~some:(fun version -> if version <= read_at then value key else "v" ^ key) (fate live)
    | None -> "v" ^ key
  in
  (result, List.rev !read, truth, !debt = 0 && not !late, not !harmed)

let reads =
  QCheck2.Test.make ~count:3000
    ~name:
      "a range is read whole, each key once and in order, as of its version, through splits, merges, moved \
       leaders, busy servers, a stale cache of regions and the locks of transactions committed, rolled \
       back, alive, pushed, with their primary not yet written and committing asynchronously; waiting \
       once after each refusal, rolling back no transaction before its time"
    ~print:(fun case -> Base.Sexp.to_string_hum (sexp_of_case case))
    gen
    (fun case ->
      let result, read, truth, paid, harmless = run case in
      let expected = List.filter (fun key -> String.compare case.start key <= 0 && before key case.stop) case.keys in
      match result with
      | Ok () ->
          List.map (fun (pair : Kv.pair) -> pair.key) read = expected
          && List.for_all (fun (pair : Kv.pair) -> pair.value = truth pair.key) read
          && paid && harmless
      | Error (`Read error) -> QCheck2.Test.fail_reportf "%a" Base.Sexp.pp (Read.sexp_of_error error))

let refusals () =
  let region : Region.t = { context = { id = 1L; conf_ver = 1L; version = 1L; peer = 1L; store = 1L }; start = ""; stop = Endless; address = "a"; replicas = [] } in
  let pauses = ref [] in
  let alive = Ok (Kv.Reply (Kv.Uncommitted { ttl = 1L; pushed = Not_pushed; min_commit_ts = 0L; protocol = Two_phase })) in
  let outcome ?(locate = fun (_ : Read.freshness) (_ : string) -> Ok region) ?(check = fun _ _ -> alive)
      ?(secondaries = fun _ ~version:_ _ -> Ok (Kv.Reply (Kv.Locks_found []))) answer =
    pauses := [];
    let store : _ Read.store =
      {
        locate;
        scan = (fun _ _ ~start:_ ~stop:_ -> Ok answer);
        check;
        secondaries;
        now = (fun () -> Ok (Int64.shift_left 1_000_000L 18));
        pause = (fun span -> pauses := span :: !pauses);
      }
    in
    match Read.range store ~version:5L ~start:"" ~stop:Endless ~limit:10 ~f:(fun _ -> Ok ()) with
    | Ok () -> "read"
    | Error (`Read error) -> Base.Sexp.to_string (Read.sexp_of_error error)
  in
  let show expected = Base.Sexp.to_string (Read.sexp_of_error expected) in
  let check ?check:asked ?secondaries name expected answer =
    Alcotest.(check string) name (show expected) (outcome ?check:asked ?secondaries answer)
  in
  let lock : Kv.lock = { primary = "p"; version = 1L; key = "k"; ttl = 5_000_000L; min_commit_ts = 0L; protocol = Two_phase } in
  check "a lock that stays" (Still_locked lock) (Reply (Locks (lock, [])));
  let waited = List.fold_left Core.Time_ns.Span.( + ) Core.Time_ns.Span.zero !pauses in
  Alcotest.(check bool) "waited for as long as patience lasts, no longer than a second more" true
    (Core.Time_ns.Span.( >= ) waited Read.patience && Core.Time_ns.Span.( < ) waited (Core.Time_ns.Span.( + ) Read.patience Core.Time_ns.Span.second));
  check "a server that stays busy" (Region_kept_failing (Server_is_busy { reason = "r"; backoff_ms = 1500L }))
    (Region_error (Server_is_busy { reason = "r"; backoff_ms = 1500L }));
  Alcotest.(check (list int)) "each wait as long as the server asks" (List.init Read.attempts (fun _ -> 1500))
    (List.map (fun span -> Core.Time_ns.Span.to_int_ms span) !pauses);
  check "a primary whose region keeps failing" (Region_kept_failing Epoch_not_match)
    ~check:(fun _ _ -> Ok (Kv.Region_error Epoch_not_match))
    (Reply (Locks (lock, [])));
  check "a primary that is not one" (Key (Other_key_error 11)) ~check:(fun _ _ -> Ok (Kv.Key_error (Other_key_error 11))) (Reply (Locks (lock, [])));
  let async = { lock with ttl = 1L; protocol = Async_commit [ "s" ] } in
  check "an async-commit lock of another transaction among its secondaries" (Disagreeing 1L)
    ~check:(fun _ _ -> Ok (Kv.Reply (Kv.Uncommitted { ttl = 1L; pushed = Not_pushed; min_commit_ts = 3L; protocol = Async_commit [ "s" ] })))
    ~secondaries:(fun _ ~version:_ _ -> Ok (Kv.Reply (Kv.Locks_found [ { async with key = "s"; version = 2L; protocol = Async_commit [] } ])))
    (Reply (Locks (async, [])));
  let passed ~ttl status =
    let scans = ref [] and checks = ref [] and now_asked = ref 0 in
    let store : _ Read.store =
      {
        locate = (fun _ _ -> Ok region);
        scan =
          (fun _ settled ~start:_ ~stop:_ ->
            scans := settled :: !scans;
            let answer = match settled with { ignored = []; committed = [] } -> Kv.Reply (Kv.Locks ({ lock with ttl }, [])) | _ -> Kv.Reply (Kv.Pairs []) in
            Ok answer);
        check = (fun _ check -> checks := check :: !checks; Ok (Kv.Reply status));
        secondaries = (fun _ ~version:_ _ -> Ok (Kv.Reply (Kv.Locks_found [])));
        now = (fun () -> incr now_asked; Ok 0L);
        pause = (fun span -> pauses := span :: !pauses);
      }
    in
    pauses := [];
    let result = Read.range store ~version:5L ~start:"" ~stop:Endless ~limit:10 ~f:(fun _ -> Ok ()) in
    (Result.is_ok result, List.rev !scans, List.rev !checks, !now_asked, List.length !pauses)
  in
  let read, scans, _, _, waits = passed ~ttl:5_000_000L (Uncommitted { ttl = 3000L; pushed = Pushed; min_commit_ts = 6L; protocol = Two_phase }) in
  Alcotest.(check bool) "a pushed transaction passed at once, its locks ignored" true
    (read && waits = 0 && scans = [ Kv.nothing_settled; { ignored = [ 1L ]; committed = [] } ]);
  let read, scans, checks, asked, _ = passed ~ttl:0L (Committed 4L) in
  Alcotest.(check bool) "a lock without a TTL: checked past every TTL, the oracle not asked, read from the lock" true
    (read && asked = 0
    && List.map (fun (check : Kv.check) -> check.current) checks = [ Past_every_ttl ]
    && scans = [ Kv.nothing_settled; { ignored = []; committed = [ 1L ] } ]);
  check "a retryable key error" (Key (Retryable "conflict")) (Key_error (Retryable "conflict"));
  check "keys out of order" (Disordered "a") (Reply (Pairs [ { key = "b"; value = "" }; { key = "a"; value = "" } ]));
  check "a key twice" (Disordered "b") (Reply (Pairs [ { key = "b"; value = "" }; { key = "b"; value = "" } ]));
  Alcotest.(check string) "a key past the region" (show (Disordered "n"))
    (outcome ~locate:(fun _ _ -> Ok { region with stop = Before "m" }) (Reply (Pairs [ { key = "n"; value = "" } ])));
  Alcotest.(check string) "a region that does not hold the key" (show (Unheld ""))
    (outcome ~locate:(fun _ _ -> Ok { region with start = "a" }) (Reply (Pairs [])))

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
  Alcotest.(check string) "names" "SELECT TABLE_NAME FROM information_schema.TABLES WHERE TABLE_SCHEMA = _utf8mb4 X'64'"
    (Quiver_mysql.Query.sql (Names (Support.ident "d")));
  Alcotest.(check string) "PD" "SELECT INSTANCE FROM information_schema.CLUSTER_INFO WHERE TYPE = 'pd'" (Quiver_mysql.Query.sql Pd_instances);
  Alcotest.(check string) "allocators" "SHOW TABLE `d`.`t'` NEXT_ROW_ID" (Quiver_mysql.Query.sql (Next_row_ids name));
  Alcotest.(check string) "checksum" "ADMIN CHECKSUM TABLE `d`.`t'`" (Quiver_mysql.Query.sql (Checksum name));
  let copy : Quiver_core.Schema.Name.t = { database = Support.ident "d"; table = Support.ident "q_1" } in
  Alcotest.(check string) "made like" "CREATE TABLE `d`.`q_1` LIKE `d`.`t'`" (Quiver_mysql.Statement.sql (Create_like { table = copy; like = name }));
  Alcotest.(check string) "row ids moved on" "ALTER TABLE `d`.`q_1` AUTO_INCREMENT = 30001"
    (Quiver_mysql.Statement.sql (Rebase { table = copy; allocator = Auto_increment; next = 30001L }));
  Alcotest.(check string) "random ids moved on" "ALTER TABLE `d`.`q_1` AUTO_RANDOM_BASE = 7"
    (Quiver_mysql.Statement.sql (Rebase { table = copy; allocator = Auto_random; next = 7L }));
  let other : Quiver_core.Schema.Name.t = { database = Support.ident "e"; table = Support.ident "q_2" } in
  Alcotest.(check string) "renamed together" "RENAME TABLE `d`.`q_1` TO `d`.`t'`, `e`.`q_2` TO `e`.`u`"
    (Quiver_mysql.Statement.sql (Rename_tables ({ from = copy; into = name }, [ { from = other; into = { other with table = Support.ident "u" } } ])));
  Alcotest.(check string) "TiFlash replicas by labels" "ALTER TABLE `d`.`q_1` SET TIFLASH REPLICA 2 LOCATION LABELS 'zone', 'it''s \\\\ odd'"
    (Quiver_mysql.Statement.sql (Set_tiflash_replica { table = copy; count = 2; labels = [ "zone"; "it's \\ odd" ] }));
  Alcotest.(check string) "no TiFlash replicas" "ALTER TABLE `d`.`q_1` SET TIFLASH REPLICA 0"
    (Quiver_mysql.Statement.sql (Set_tiflash_replica { table = copy; count = 0; labels = [] }));
  Alcotest.(check string) "TiFlash replicas of a database"
    "SELECT TABLE_NAME, REPLICA_COUNT, LOCATION_LABELS FROM information_schema.TIFLASH_REPLICA WHERE TABLE_SCHEMA = _utf8mb4 X'64'"
    (Quiver_mysql.Query.sql (Tiflash_replicas (Support.ident "d")));
  Alcotest.(check string) "TiFlash stores" "SELECT COUNT(*) FROM information_schema.CLUSTER_INFO WHERE TYPE = 'tiflash'" (Quiver_mysql.Query.sql Tiflash_stores)

let tests =
  [
    Alcotest.test_case "order-keeping bytes and table keys" `Quick codec;
    Alcotest.test_case "the queries for regions" `Quick queries;
    Alcotest.test_case "CRC64 as TiKV sums tables" `Quick crc64;
    QCheck_alcotest.to_alcotest crc64_split;
    Alcotest.test_case "the region of a key" `Quick regions;
    Alcotest.test_case "reads that give up" `Quick refusals;
  ]
  @ List.map QCheck_alcotest.to_alcotest [ codec_roundtrip; codec_order; rows_end; reads ]
