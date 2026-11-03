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

type lock = { primary : string; version : int64; key : string; ttl : int64 } [@@deriving sexp_of ~portable]

type key_error = Locked of lock | Retryable of string | Abort of string | Other_key_error of int
[@@deriving sexp_of ~portable]

type pair = { key : string; value : string } [@@deriving sexp_of ~portable]

type scanned = Pairs of pair list | Region_error of region_error | Key_error of key_error
[@@deriving sexp_of ~portable]

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

let scan_request region ~direction ~start ~stop ~version ~limit =
  let context =
    message (fun buffer ->
        context buffer region;
        Proto.uint buffer 6 1L;
        Proto.uint buffer 8 1L)
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

let scanned text =
  let slowly () =
    let fields (pos, len) ~init ~f = Proto.fold text ~pos ~len ~init ~f in
    let key_error slice =
      let%bind first =
        fields slice ~init:None ~f:(fun first field value ->
            match first with Some _ -> Ok first | None -> Ok (Some (field, value)))
      in
      match first with
      | None -> Ok (Other_key_error 0)
      | Some (1, value) ->
          let%bind slice = Proto.slice 1 value in
          Result.map
            (fun lock -> Locked lock)
            (fields slice ~init:{ primary = ""; version = 0L; key = ""; ttl = 0L } ~f:(fun lock field value ->
                 match field with
                 | 1 -> Result.map (fun primary -> { lock with primary }) (Proto.contents text field value)
                 | 2 -> Result.map (fun version -> { lock with version }) (Proto.number field value)
                 | 3 -> Result.map (fun key -> { lock with key }) (Proto.contents text field value)
                 | 4 -> Result.map (fun ttl -> { lock with ttl }) (Proto.number field value)
                 | _ -> Ok lock))
      | Some (2, value) -> Result.map (fun reason -> Retryable reason) (Proto.contents text 2 value)
      | Some (3, value) -> Result.map (fun reason -> Abort reason) (Proto.contents text 3 value)
      | Some (field, _) -> Ok (Other_key_error field)
    in
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
    match (region, error) with
    | Some (pos, len), _ -> Result.map (fun error -> Region_error error) (region_error text ~pos ~len)
    | None, Some slice -> Result.map (fun error -> Key_error error) (key_error slice)
    | None, None ->
        let rec collect acc = function
          | [] -> Ok (Pairs acc)
          | `Pair (key, value) :: rest -> collect ({ key; value } :: acc) rest
          | `Failed slice :: _ -> Result.map (fun error -> Key_error error) (key_error slice)
        in
        collect [] pairs
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
  | Walked -> Ok (Pairs (List.rev !pairs))
  | Unusual -> slowly ()

let scan connection region ~direction ~start ~stop ~version ~limit =
  let%bind response =
    Grpc.call connection ~path:"/tikvpb.Tikv/KvScan" (scan_request region ~direction ~start ~stop ~version ~limit)
  in
  scanned response

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
