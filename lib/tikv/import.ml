open Base.Result.Let_syntax

type mode = Normal | Import
type sst = string
type job = { uuid : string; region : Kv.region; commit_ts : int64; pairs : Pairs.span list }
type 'sst written = Wrote of 'sst list | Write_failed of string
type ingested = Ingested | Ingest_failed of Kv.region_error

let switch_mode connection mode =
  let request = Kv.message (fun buffer -> match mode with Normal -> () | Import -> Proto.uint buffer 1 1L) in
  let%bind response = Grpc.call connection ~path:"/import_sstpb.ImportSST/SwitchMode" request in
  Proto.fold response ~pos:0 ~len:(String.length response) ~init:() ~f:(fun () _ _ -> Ok ())

let meta job =
  let bound place span = Option.fold ~none:"" ~some:(fun span -> Codec.encode_bytes (Pairs.key span (place span))) span in
  let first = bound (fun _ -> 0) (Base.List.hd job.pairs) and last = bound (fun span -> Pairs.length span - 1) (Base.List.last job.pairs) in
  let meta =
    Kv.message (fun buffer ->
        Proto.bytes buffer 1 job.uuid;
        Proto.bytes buffer 2
          (Kv.message (fun buffer ->
               Proto.bytes buffer 1 first;
               Proto.bytes buffer 2 last));
        Proto.uint buffer 6 job.region.id;
        Proto.bytes buffer 7
          (Kv.message (fun buffer ->
               Proto.uint buffer 1 job.region.conf_ver;
               Proto.uint buffer 2 job.region.version)))
  in
  Kv.message (fun buffer ->
      Proto.bytes buffer 1 meta;
      Proto.bytes buffer 3 (Kv.message (fun buffer -> Proto.bytes buffer 24 "quiver")))

let batch job pairs =
  let pair_size i = Proto.delimited_size 1 (Pairs.key_length pairs i) + Proto.delimited_size 2 (Pairs.value_length pairs i) in
  let rec measure i size = if i = Pairs.length pairs then size else measure (i + 1) (size + Proto.delimited_size 2 (pair_size i)) in
  let inside = measure 0 (Proto.uint_size 1 job.commit_ts) in
  let head = Kv.message (fun buffer -> Proto.delimited buffer 2 inside; Proto.uint buffer 1 job.commit_ts) in
  let bytes = Bytes.create (Proto.delimited_size 2 inside) in
  Bytes.blit_string head 0 bytes 0 (String.length head);
  let rec encode i pos =
    if i < Pairs.length pairs then (
      let pos = Proto.put_delimited bytes pos 2 (pair_size i) in
      let pos = Proto.put_delimited bytes pos 1 (Pairs.key_length pairs i) in
      Pairs.blit_key pairs i bytes pos;
      let pos = Proto.put_delimited bytes (pos + Pairs.key_length pairs i) 2 (Pairs.value_length pairs i) in
      Pairs.blit_value pairs i bytes pos;
      encode (i + 1) (pos + Pairs.value_length pairs i))
  in
  encode 0 (String.length head);
  Bytes.unsafe_to_string bytes

let write_requests job = meta job :: List.map (batch job) job.pairs

let written text =
  let%bind failure, ssts =
    Proto.fold text ~pos:0 ~len:(String.length text) ~init:(None, []) ~f:(fun (failure, ssts) field value ->
        match field with
        | 1 ->
            let%bind pos, len = Proto.slice field value in
            let%bind message =
              Proto.fold text ~pos ~len ~init:"" ~f:(fun message field value ->
                  match field with 1 -> Proto.contents text field value | _ -> Ok message)
            in
            Ok (Some message, ssts)
        | 2 -> Result.map (fun sst -> (failure, sst :: ssts)) (Proto.contents text field value)
        | _ -> Ok (failure, ssts))
  in
  Ok (Option.fold ~none:(Wrote (List.rev ssts)) ~some:(fun message -> Write_failed message) failure)

let write connections job =
  let%bind streams = Base.Result.all (List.map (fun connection -> Grpc.start connection ~path:"/import_sstpb.ImportSST/Write") connections) in
  let sent =
    let to_all request = Base.List.fold_result streams ~init:() ~f:(fun () stream -> Grpc.send stream request) in
    let%bind () = to_all (meta job) in
    Base.List.fold_result job.pairs ~init:() ~f:(fun () pairs -> to_all (batch job pairs))
  in
  let finished =
    List.map
      (fun stream ->
        match Grpc.finish stream with
        | Ok response -> written response
        | Error (`Grpc (Grpc.Status { message; _ })) -> Ok (Write_failed message)
        | Error _ as error -> error)
      streams
  in
  let%bind () = sent in
  Base.Result.all finished

let ingest_request region ssts =
  Kv.message (fun buffer ->
      Proto.bytes buffer 1 (Kv.message (fun buffer -> Kv.context buffer region));
      List.iter (Proto.bytes buffer 2) ssts)

let ingested text =
  let%bind failure =
    Proto.fold text ~pos:0 ~len:(String.length text) ~init:None ~f:(fun failure field value ->
        match field with 1 -> Result.map Option.some (Proto.slice field value) | _ -> Ok failure)
  in
  Option.fold ~none:(Ok Ingested)
    ~some:(fun (pos, len) -> Result.map (fun error -> Ingest_failed error) (Kv.region_error text ~pos ~len))
    failure

let ingest connection region ssts =
  let%bind response = Grpc.call connection ~path:"/import_sstpb.ImportSST/MultiIngest" (ingest_request region ssts) in
  ingested response
