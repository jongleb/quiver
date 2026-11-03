open Base.Result.Let_syntax

type error =
  | Refused of { kind : int; message : string }
  | No_leader
  | Url of string
  | Unknown_store of int64
  | Undecodable of { region : int64; key : string }
  | Leaderless of int64
  | Gap of string
[@@deriving sexp_of ~portable]

type t = { connection : Http2.t; cluster : int64; stores : (int64 * string) list; tikv : string list }

let call connection ~cluster ~name fill =
  let request =
    Kv.message (fun buffer ->
        Proto.bytes buffer 1 (Kv.message (fun buffer -> Proto.uint buffer 1 cluster));
        fill buffer)
  in
  let%bind response = Grpc.call connection ~path:("/pdpb.PD/" ^ name) request in
  let%bind header =
    Proto.fold response ~pos:0 ~len:(String.length response) ~init:None ~f:(fun header field value ->
        match field with 1 -> Result.map Option.some (Proto.slice field value) | _ -> Ok header)
  in
  let%bind cluster, refusal =
    Option.fold ~none:(Ok (0L, None))
      ~some:(fun (pos, len) ->
        Proto.fold response ~pos ~len ~init:(0L, None) ~f:(fun (cluster, refusal) field value ->
            match field with
            | 1 -> Result.map (fun cluster -> (cluster, refusal)) (Proto.number field value)
            | 2 ->
                let%bind pos, len = Proto.slice field value in
                let%bind kind, message =
                  Proto.fold response ~pos ~len ~init:(0L, "") ~f:(fun (kind, message) field value ->
                      match field with
                      | 1 -> Result.map (fun kind -> (kind, message)) (Proto.number field value)
                      | 2 -> Result.map (fun message -> (kind, message)) (Proto.contents response field value)
                      | _ -> Ok (kind, message))
                in
                Ok (cluster, if Int64.equal kind 0L then None else Some (Int64.to_int kind, message))
            | _ -> Ok (cluster, refusal)))
      header
  in
  match refusal with
  | Some (kind, message) -> Error (`Pd (Refused { kind; message }))
  | None -> Ok (cluster, response)

let fields text (pos, len) ~init ~f = Proto.fold text ~pos ~len ~init ~f

let connect ~timeout address =
  let stores = Stores.create ~timeout in
  let connected =
    let%bind first = Stores.connection stores address in
    let%bind cluster, members = call first ~cluster:0L ~name:"GetMembers" ignore in
    let%bind urls =
      Proto.fold members ~pos:0 ~len:(String.length members) ~init:[] ~f:(fun urls field value ->
          match field with
          | 3 ->
              let%bind leader = Proto.slice field value in
              fields members leader ~init:urls ~f:(fun urls field value ->
                  match field with 4 -> Result.map (fun url -> url :: urls) (Proto.contents members field value) | _ -> Ok urls)
          | _ -> Ok urls)
    in
    let%bind url = Option.to_result ~none:(`Pd No_leader) (Base.List.last urls) in
    let%bind leader =
      match Base.String.chop_prefix url ~prefix:"http://" with
      | None -> Error (`Pd (Url url))
      | Some hostport -> Stores.connection stores (Option.value ~default:hostport (Base.String.chop_suffix hostport ~suffix:"/"))
    in
    let%bind _, listed = call leader ~cluster ~name:"GetAllStores" (fun buffer -> Proto.uint buffer 2 1L) in
    let%bind stores_listed =
      Proto.fold listed ~pos:0 ~len:(String.length listed) ~init:[] ~f:(fun acc field value ->
          match field with
          | 2 ->
              let%bind store = Proto.slice field value in
              let%bind id, address, engine =
                fields listed store ~init:(0L, "", `Tikv) ~f:(fun (id, address, engine) field value ->
                    match field with
                    | 1 -> Result.map (fun id -> (id, address, engine)) (Proto.number field value)
                    | 2 -> Result.map (fun address -> (id, address, engine)) (Proto.contents listed field value)
                    | 4 ->
                        let%bind label = Proto.slice field value in
                        let%bind pair =
                          fields listed label ~init:("", "") ~f:(fun (key, text) field value ->
                              match field with
                              | 1 -> Result.map (fun key -> (key, text)) (Proto.contents listed field value)
                              | 2 -> Result.map (fun text -> (key, text)) (Proto.contents listed field value)
                              | _ -> Ok (key, text))
                        in
                        let engine = match pair with "engine", "tiflash" -> `Tiflash | _ -> engine in
                        Ok (id, address, engine)
                    | _ -> Ok (id, address, engine))
              in
              Ok ((id, address, engine) :: acc)
          | _ -> Ok acc)
    in
    let () = if first != leader then Http2.close first in
    let listed = List.rev stores_listed in
    Ok
      {
        connection = leader;
        cluster;
        stores = List.map (fun (id, address, _) -> (id, address)) listed;
        tikv = List.filter_map (fun (_, address, engine) -> match engine with `Tikv -> Some address | `Tiflash -> None) listed;
      }
  in
  let () = match connected with Ok _ -> () | Error _ -> Stores.close stores in
  connected

let regions t ~start ~stop =
  let encoded_stop = match (stop : Kv.bound) with Endless -> "" | Before key -> Codec.encode_bytes key in
  let past_stop key = match stop with Endless -> false | Before _ -> String.compare encoded_stop key <= 0 in
  let decoded region key =
    if key = "" then Ok "" else Result.map_error (fun (`Codec _) -> `Pd (Undecodable { region; key })) (Codec.decode_bytes key)
  in
  let peer text slice =
    fields text slice ~init:{ Kv.id = 0L; store = 0L } ~f:(fun (peer : Kv.peer) field value ->
        match field with
        | 1 -> Result.map (fun id -> { peer with id }) (Proto.number field value)
        | 2 -> Result.map (fun store -> { peer with store }) (Proto.number field value)
        | _ -> Ok peer)
  in
  let region text slice =
    let%bind meta, leader =
      fields text slice ~init:(None, None) ~f:(fun (meta, leader) field value ->
          match field with
          | 1 -> Result.map (fun slice -> (Some slice, leader)) (Proto.slice field value)
          | 2 -> Result.map (fun slice -> (meta, Some slice)) (Proto.slice field value)
          | _ -> Ok (meta, leader))
    in
    let%bind id, first, past, (conf_ver, version), peers =
      Option.fold ~none:(Ok (0L, "", "", (0L, 0L), []))
        ~some:(fun slice ->
          fields text slice ~init:(0L, "", "", (0L, 0L), []) ~f:(fun (id, first, past, epoch, peers) field value ->
              match field with
              | 1 -> Result.map (fun id -> (id, first, past, epoch, peers)) (Proto.number field value)
              | 2 -> Result.map (fun first -> (id, first, past, epoch, peers)) (Proto.contents text field value)
              | 3 -> Result.map (fun past -> (id, first, past, epoch, peers)) (Proto.contents text field value)
              | 4 ->
                  let%bind slice = Proto.slice field value in
                  let%bind epoch =
                    fields text slice ~init:(0L, 0L) ~f:(fun (conf_ver, version) field value ->
                        match field with
                        | 1 -> Result.map (fun conf_ver -> (conf_ver, version)) (Proto.number field value)
                        | 2 -> Result.map (fun version -> (conf_ver, version)) (Proto.number field value)
                        | _ -> Ok (conf_ver, version))
                  in
                  Ok (id, first, past, epoch, peers)
              | 5 ->
                  let%bind slice = Proto.slice field value in
                  Result.map (fun peer -> (id, first, past, epoch, peer :: peers)) (peer text slice)
              | _ -> Ok (id, first, past, epoch, peers)))
        meta
    in
    let%bind leader =
      Option.fold ~none:(Error (`Pd (Leaderless id))) ~some:(fun slice -> peer text slice) leader
    in
    let%bind () = if Int64.equal leader.id 0L then Error (`Pd (Leaderless id)) else Ok () in
    let address (peer : Kv.peer) =
      Option.to_result ~none:(`Pd (Unknown_store peer.store)) (List.assoc_opt peer.store t.stores)
    in
    let%bind replicas =
      Base.Result.all
        (List.map
           (fun (peer : Kv.peer) -> Result.map (fun address : Region.replica -> { peer; address }) (address peer))
           (List.sort (fun (a : Kv.peer) b -> Int64.compare a.id b.id) peers))
    in
    let%bind leader_address = address leader in
    let%bind start = decoded id first in
    let%bind stop = if past = "" then Ok Kv.Endless else Result.map (fun key -> Kv.Before key) (decoded id past) in
    Ok
      ( (first, past),
        ({
           context = { id; conf_ver; version; peer = leader.id; store = leader.store };
           start;
           stop;
           address = leader_address;
           replicas;
         }
          : Region.t) )
  in
  let rec go from acc =
    let%bind _, response =
      call t.connection ~cluster:t.cluster ~name:"ScanRegions" (fun buffer ->
          Proto.bytes buffer 2 from;
          Proto.uint buffer 3 128L;
          match stop with Endless -> () | Before _ -> Proto.bytes buffer 4 encoded_stop)
    in
    let%bind found =
      Proto.fold response ~pos:0 ~len:(String.length response) ~init:[] ~f:(fun found field value ->
          match field with
          | 4 ->
              let%bind slice = Proto.slice field value in
              Result.map (fun region -> region :: found) (region response slice)
          | _ -> Ok found)
    in
    let found = List.rev found in
    let%bind () =
      Result.map ignore
        (Base.List.fold_result found ~init:from ~f:(fun expected ((first, past), _) ->
             if (expected = from && String.compare first from <= 0) || first = expected then Ok past
             else Error (`Pd (Gap expected))))
    in
    let acc = List.rev_append (List.map snd found) acc in
    match Base.List.last found with
    | None -> if acc = [] then Error (`Pd (Gap from)) else Ok (List.rev acc)
    | Some ((_, past), _) -> if past = "" || past_stop past then Ok (List.rev acc) else go past acc
  in
  go (Codec.encode_bytes start) []

let scatter t regions =
  let%bind _, response =
    call t.connection ~cluster:t.cluster ~name:"ScatterRegion" (fun buffer ->
        Proto.bytes buffer 5 "quiver";
        List.iter (Proto.uint buffer 6) regions;
        Proto.uint buffer 7 3L)
  in
  Proto.fold response ~pos:0 ~len:(String.length response) ~init:[] ~f:(fun failed field value ->
      match field with 3 -> Result.map (fun ids -> List.rev_append ids failed) (Proto.numbers response field value) | _ -> Ok failed)

let stores t = t.tikv

let close t = Http2.close t.connection
