open Base.Result.Let_syntax

type region = { id : int64; conf_ver : int64; version : int64; peer : int64; store : int64 } [@@deriving sexp_of ~portable]
type peer = { id : int64; store : int64 } [@@deriving sexp_of ~portable]
type bound = Before of string | Endless [@@deriving sexp_of ~portable]
type direction = Forward | Backward

type region_error =
  | Not_leader of peer option
  | Epoch_not_match
  | Region_not_found
  | Server_is_busy of { reason : string; backoff_ms : int64 }
  | Other of { kind : int; message : string }
[@@deriving sexp_of ~portable]

type protocol = Two_phase | Async_commit of string list [@@deriving sexp_of ~portable]

type lock = { primary : string; version : int64; key : string; ttl : int64; min_commit_ts : int64; protocol : protocol }
[@@deriving sexp_of ~portable]

type key_error = Locked of lock | Retryable of string | Abort of string | Txn_not_found | Other_key_error of int
[@@deriving sexp_of ~portable]

type pair = { key : string; value : string } [@@deriving sexp_of ~portable]
type 'a reply = Reply of 'a | Region_error of region_error | Key_error of key_error [@@deriving sexp_of ~portable]
type scan = Pairs of pair list | Locks of lock * lock list [@@deriving sexp_of ~portable]
type settled = { ignored : int64 list; committed : int64 list }

let nothing_settled = { ignored = []; committed = [] }

type clock = At of int64 | Past_every_ttl
type missing = Rollback_if_missing | Leave_if_missing
type sync = As_locked | Sync_commit_forced

type check = { primary : string; version : int64; caller : int64; current : clock; missing : missing; sync : sync }

type pushed = Pushed | Not_pushed [@@deriving sexp_of ~portable]

type status =
  | Committed of int64
  | Rolled_back
  | Uncommitted of { ttl : int64; pushed : pushed; min_commit_ts : int64; protocol : protocol }
  | Unsettled of int
[@@deriving sexp_of ~portable]

type secondaries = Committed_secondary of int64 | Locks_found of lock list [@@deriving sexp_of ~portable]

let message fill =
  let buffer = Buffer.create 64 in
  fill buffer;
  Buffer.contents buffer

let context buffer (region : region) =
  Proto.uint buffer 1 region.id;
  Proto.bytes buffer 2
    (message (fun buffer ->
         Proto.uint buffer 1 region.conf_ver;
         Proto.uint buffer 2 region.version));
  Proto.bytes buffer 3
    (message (fun buffer ->
         Proto.uint buffer 1 region.peer;
         Proto.uint buffer 2 region.store));
  Proto.bytes buffer 24 "quiver"

let scan_request region settled ~direction ~start ~stop ~version ~limit =
  let context =
    message (fun buffer ->
        context buffer region;
        Proto.uint buffer 6 1L;
        Proto.uint buffer 8 1L;
        List.iter (Proto.uint buffer 13) settled.ignored;
        List.iter (Proto.uint buffer 22) settled.committed)
  in
  message (fun buffer ->
      Proto.bytes buffer 1 context;
      match direction with
      | Forward -> (
          Proto.bytes buffer 2 start;
          Proto.uint buffer 3 (Int64.of_int limit);
          Proto.uint buffer 4 version;
          match stop with Before key -> Proto.bytes buffer 7 key | Endless -> ())
      | Backward ->
          let () = match stop with Before key -> Proto.bytes buffer 2 key | Endless -> () in
          Proto.uint buffer 3 (Int64.of_int limit);
          Proto.uint buffer 4 version;
          Proto.uint buffer 6 1L;
          Proto.bytes buffer 7 start)

let region_error text ~pos ~len =
  let fields (pos, len) ~init ~f = Proto.fold text ~pos ~len ~init ~f in
  let%bind message, kind =
    fields (pos, len) ~init:("", None) ~f:(fun (message, kind) field value ->
        match field with
        | 1 -> Result.map (fun message -> (message, kind)) (Proto.contents text field value)
        | _ -> (
            match value with
            | Delimited { pos; len } -> Ok (message, Some (field, (pos, len)))
            | Varint _ | Fixed64 _ | Fixed32 _ -> Ok (message, kind)))
  in
  match kind with
  | None -> Ok (Other { kind = 0; message })
  | Some (2, inner) ->
      fields inner ~init:(Not_leader None) ~f:(fun error field value ->
          match field with
          | 2 ->
              let%bind slice = Proto.slice field value in
              let%bind leader =
                fields slice ~init:{ id = 0L; store = 0L } ~f:(fun (peer : peer) field value ->
                    match field with
                    | 1 -> Result.map (fun id -> { peer with id }) (Proto.number field value)
                    | 2 -> Result.map (fun store -> { peer with store }) (Proto.number field value)
                    | _ -> Ok peer)
              in
              Ok (Not_leader (Some leader))
          | _ -> Ok error)
  | Some (3, _) -> Ok Region_not_found
  | Some (5, _) -> Ok Epoch_not_match
  | Some (6, inner) ->
      fields inner ~init:(Server_is_busy { reason = ""; backoff_ms = 0L }) ~f:(fun error field value ->
          match (error, field) with
          | Server_is_busy busy, 1 -> Result.map (fun reason -> Server_is_busy { busy with reason }) (Proto.contents text field value)
          | Server_is_busy busy, 2 -> Result.map (fun backoff_ms -> Server_is_busy { busy with backoff_ms }) (Proto.number field value)
          | _ -> Ok error)
  | Some (kind, _) -> Ok (Other { kind; message })

let lock text (pos, len) =
  let%bind lock, secondaries =
    Proto.fold text ~pos ~len
      ~init:({ primary = ""; version = 0L; key = ""; ttl = 0L; min_commit_ts = 0L; protocol = Two_phase }, [])
      ~f:(fun (lock, secondaries) field value ->
        match field with
        | 1 -> Result.map (fun primary -> ({ lock with primary }, secondaries)) (Proto.contents text field value)
        | 2 -> Result.map (fun version -> ({ lock with version }, secondaries)) (Proto.number field value)
        | 3 -> Result.map (fun key -> ({ lock with key }, secondaries)) (Proto.contents text field value)
        | 4 -> Result.map (fun ttl -> ({ lock with ttl }, secondaries)) (Proto.number field value)
        | 8 ->
            let%bind flag = Proto.number field value in
            let protocol = if Int64.equal flag 0L then Two_phase else Async_commit [] in
            Ok ({ lock with protocol }, secondaries)
        | 9 -> Result.map (fun min_commit_ts -> ({ lock with min_commit_ts }, secondaries)) (Proto.number field value)
        | 10 -> Result.map (fun key -> (lock, key :: secondaries)) (Proto.contents text field value)
        | _ -> Ok (lock, secondaries))
  in
  match lock.protocol with
  | Two_phase -> Ok lock
  | Async_commit _ -> Ok { lock with protocol = Async_commit (List.rev secondaries) }

let key_error text slice =
  let%bind first =
    Proto.fold text ~pos:(fst slice) ~len:(snd slice) ~init:None ~f:(fun first field value ->
        match first with Some _ -> Ok first | None -> Ok (Some (field, value)))
  in
  match first with
  | None -> Ok (Other_key_error 0)
  | Some (1, value) ->
      let%bind slice = Proto.slice 1 value in
      Result.map (fun lock -> Locked lock) (lock text slice)
  | Some (2, value) -> Result.map (fun reason -> Retryable reason) (Proto.contents text 2 value)
  | Some (3, value) -> Result.map (fun reason -> Abort reason) (Proto.contents text 3 value)
  | Some (8, _) -> Ok Txn_not_found
  | Some (field, _) -> Ok (Other_key_error field)

let replied text ~init ~f ~finish =
  let%bind region, error, acc =
    Proto.fold text ~pos:0 ~len:(String.length text) ~init:(None, None, init) ~f:(fun (region, error, acc) field value ->
        match field with
        | 1 -> Result.map (fun slice -> (Some slice, error, acc)) (Proto.slice field value)
        | 2 -> Result.map (fun slice -> (region, Some slice, acc)) (Proto.slice field value)
        | _ -> Result.map (fun acc -> (region, error, acc)) (f acc field value))
  in
  match (region, error) with
  | Some (pos, len), _ -> Result.map (fun error -> Region_error error) (region_error text ~pos ~len)
  | None, Some slice -> Result.map (fun error -> Key_error error) (key_error text slice)
  | None, None -> Result.map (fun answer -> Reply answer) (finish acc)

let scanned text =
  let slowly () =
    let fields (pos, len) ~init ~f = Proto.fold text ~pos ~len ~init ~f in
    let%bind region, pairs, error =
      Proto.fold text ~pos:0 ~len:(String.length text) ~init:(None, [], None) ~f:(fun (region, pairs, error) field value ->
          match field with
          | 1 -> Result.map (fun slice -> (Some slice, pairs, error)) (Proto.slice field value)
          | 2 ->
              let%bind slice = Proto.slice field value in
              let%bind pair =
                fields slice ~init:(`Pair ("", ""))
                  ~f:(fun pair field value ->
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
    let locked = function Locked lock -> Reply (Locks (lock, [])) | error -> Key_error error in
    match (region, error) with
    | Some (pos, len), _ -> Result.map (fun error -> Region_error error) (region_error text ~pos ~len)
    | None, Some slice -> Result.map locked (key_error text slice)
    | None, None -> (
        let%bind collected =
          Base.List.fold_result pairs ~init:(`Pairs []) ~f:(fun collected pair ->
              match (collected, pair) with
              | `Pairs acc, `Pair (key, value) -> Ok (`Pairs ({ key; value } :: acc))
              | `Failed _, `Pair _ -> Ok collected
              | `Pairs _, `Failed slice -> Result.map (fun error -> `Failed [ error ]) (key_error text slice)
              | `Failed errors, `Failed slice -> Result.map (fun error -> `Failed (error :: errors)) (key_error text slice))
        in
        match collected with
        | `Pairs acc -> Ok (Reply (Pairs acc))
        | `Failed errors -> (
            let locks = List.filter_map (function Locked lock -> Some lock | _ -> None) errors in
            match (List.find_opt (function Locked _ -> false | _ -> true) errors, locks) with
            | Some error, _ -> Ok (Key_error error)
            | None, first :: rest -> Ok (Reply (Locks (first, rest)))
            | None, [] -> Ok (Key_error (Other_key_error 0))))
  in
  let pairs = ref [] and key = ref "" and value = ref "" in
  let known field = match field with 1 | 2 | 3 -> Proto.Unusual | _ -> Proto.Walked in
  let pair field pos len =
    match field with
    | 2 ->
        key := String.sub text pos len;
        Proto.Walked
    | 3 ->
        value := String.sub text pos len;
        Walked
    | 1 -> Unusual
    | _ -> Walked
  in
  let response field pos len =
    match field with
    | 2 -> (
        key := "";
        value := "";
        match Proto.walk text ~pos ~len ~delimited:pair ~scalar:known with
        | Walked ->
            pairs := { key = !key; value = !value } :: !pairs;
            Proto.Walked
        | Unusual -> Unusual)
    | 1 | 3 -> Unusual
    | _ -> Walked
  in
  match Proto.walk text ~pos:0 ~len:(String.length text) ~delimited:response ~scalar:known with
  | Walked -> Ok (Reply (Pairs (List.rev !pairs)))
  | Unusual -> slowly ()

let scan connection region settled ~direction ~start ~stop ~version ~limit =
  let%bind response =
    Grpc.call connection ~path:"/tikvpb.Tikv/KvScan" (scan_request region settled ~direction ~start ~stop ~version ~limit)
  in
  scanned response

let check_request region check =
  message (fun buffer ->
      Proto.bytes buffer 1 (message (fun buffer -> context buffer region));
      Proto.bytes buffer 2 check.primary;
      Proto.uint buffer 3 check.version;
      Proto.uint buffer 4 check.caller;
      let current = match check.current with At now -> now | Past_every_ttl -> -1L in
      Proto.uint buffer 5 current;
      let () = match check.missing with Rollback_if_missing -> Proto.uint buffer 6 1L | Leave_if_missing -> () in
      let () = match check.sync with Sync_commit_forced -> Proto.uint buffer 7 1L | As_locked -> () in
      Proto.uint buffer 9 1L)

let checked text =
  replied text ~init:(0L, 0L, 0L, None)
    ~f:(fun (ttl, commit, action, primary) field value ->
      match field with
      | 3 -> Result.map (fun ttl -> (ttl, commit, action, primary)) (Proto.number field value)
      | 4 -> Result.map (fun commit -> (ttl, commit, action, primary)) (Proto.number field value)
      | 5 -> Result.map (fun action -> (ttl, commit, action, primary)) (Proto.number field value)
      | 6 ->
          let%bind slice = Proto.slice field value in
          Result.map (fun lock -> (ttl, commit, action, Some lock)) (lock text slice)
      | _ -> Ok (ttl, commit, action, primary))
    ~finish:(fun (ttl, commit, action, primary) ->
      let pushed = if Int64.equal action 3L then Pushed else Not_pushed in
      match primary with
      | _ when not (Int64.equal commit 0L) -> Ok (Committed commit)
      | Some (primary : lock) -> Ok (Uncommitted { ttl; pushed; min_commit_ts = primary.min_commit_ts; protocol = primary.protocol })
      | None when not (Int64.equal ttl 0L) -> Ok (Uncommitted { ttl; pushed; min_commit_ts = 0L; protocol = Two_phase })
      | None -> (
          match action with 0L | 1L | 2L -> Ok Rolled_back | action -> Ok (Unsettled (Int64.to_int action))))

let check connection region check =
  let%bind response = Grpc.call connection ~path:"/tikvpb.Tikv/KvCheckTxnStatus" (check_request region check) in
  checked response

let secondaries_request region ~version keys =
  message (fun buffer ->
      Proto.bytes buffer 1 (message (fun buffer -> context buffer region));
      List.iter (Proto.bytes buffer 2) keys;
      Proto.uint buffer 3 version)

let secondaries_checked text =
  replied text ~init:([], 0L)
    ~f:(fun (locks, commit) field value ->
      match field with
      | 3 ->
          let%bind slice = Proto.slice field value in
          Result.map (fun found -> (found :: locks, commit)) (lock text slice)
      | 4 -> Result.map (fun commit -> (locks, commit)) (Proto.number field value)
      | _ -> Ok (locks, commit))
    ~finish:(fun (locks, commit) ->
      if Int64.equal commit 0L then Ok (Locks_found (List.rev locks)) else Ok (Committed_secondary commit))

let check_secondaries connection region ~version keys =
  let%bind response =
    Grpc.call connection ~path:"/tikvpb.Tikv/KvCheckSecondaryLocks" (secondaries_request region ~version keys)
  in
  secondaries_checked response

type split = Split of int64 list | Split_refused of region_error

let split_request region keys =
  message (fun buffer ->
      Proto.bytes buffer 1 (message (fun buffer -> context buffer region));
      List.iter (Proto.bytes buffer 3) keys)

let split_response text =
  let%bind refused, ids =
    Proto.fold text ~pos:0 ~len:(String.length text) ~init:(None, []) ~f:(fun (refused, ids) field value ->
        match field with
        | 1 -> Result.map (fun slice -> (Some slice, ids)) (Proto.slice field value)
        | 4 ->
            let%bind pos, len = Proto.slice field value in
            let%bind id =
              Proto.fold text ~pos ~len ~init:0L ~f:(fun id field value -> match field with 1 -> Proto.number field value | _ -> Ok id)
            in
            Ok (refused, id :: ids)
        | _ -> Ok (refused, ids))
  in
  Option.fold ~none:(Ok (Split (List.rev ids)))
    ~some:(fun (pos, len) -> Result.map (fun error -> Split_refused error) (region_error text ~pos ~len))
    refused

let split connection region keys =
  let%bind response = Grpc.call connection ~path:"/tikvpb.Tikv/SplitRegion" (split_request region keys) in
  split_response response
