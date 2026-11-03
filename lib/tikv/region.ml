open Base.Result.Let_syntax

type replica = { peer : Kv.peer; address : string } [@@deriving sexp_of ~portable]

type t = { context : Kv.region; start : string; stop : Kv.bound; address : string; replicas : replica list }
[@@deriving sexp_of ~portable]

type error =
  | Malformed of string option list
  | Undecodable of { region : int64; key : string }
  | No_leader of int64
  | No_table of Quiver_core.Schema.Name.t
[@@deriving sexp_of ~portable]

let find regions key =
  List.find_opt
    (fun region ->
      String.compare region.start key <= 0
      && match region.stop with Endless -> true | Before stop -> String.compare key stop < 0)
    regions

let of_rows ~regions ~peers =
  let malformed row = Error (`Region (Malformed (Array.to_list row))) in
  let%bind peers =
    Base.Result.all
      (List.map
         (fun row ->
           match row with
           | [| Some region; Some id; Some store; Some (("0" | "1") as leads); Some address |] ->
               Option.fold ~none:(malformed row) ~some:Result.ok
                 (Option.bind (Int64.of_string_opt region) (fun region ->
                      Option.bind (Int64.of_string_opt id) (fun id ->
                          Option.map
                            (fun store -> (region, leads, { peer = { Kv.id; store }; address }))
                            (Int64.of_string_opt store))))
           | _ -> malformed row)
         peers)
  in
  let%bind parsed =
    Base.Result.all
      (List.map
         (fun row ->
           match row with
           | [| Some id; Some start; Some stop; Some conf_ver; Some version |] -> (
               match (Int64.of_string_opt id, Int64.of_string_opt conf_ver, Int64.of_string_opt version) with
               | Some id, Some conf_ver, Some version ->
                   let key hex =
                     if hex = "" then Ok ""
                     else
                       match Base.Option.try_with (fun () -> Hex_encode.from_hex hex) with
                       | None -> Error (`Region (Undecodable { region = id; key = hex }))
                       | Some encoded ->
                           Result.map_error (fun (`Codec _) -> `Region (Undecodable { region = id; key = hex })) (Codec.decode_bytes encoded)
                   in
                   let%bind start = key start in
                   let%bind stop = if stop = "" then Ok Kv.Endless else Result.map (fun key -> Kv.Before key) (key stop) in
                   let replicas =
                     List.sort
                       (fun a b -> Int64.compare a.peer.id b.peer.id)
                       (List.filter_map (fun (region, _, replica) -> if Int64.equal region id then Some replica else None) peers)
                   in
                   let%bind leader =
                     Option.to_result ~none:(`Region (No_leader id))
                       (List.find_map
                          (fun (region, leads, replica) -> if Int64.equal region id && leads = "1" then Some replica else None)
                          peers)
                   in
                   Ok
                     {
                       context = { id; conf_ver; version; peer = leader.peer.id; store = leader.peer.store };
                       start;
                       stop;
                       address = leader.address;
                       replicas;
                     }
               | _ -> malformed row)
           | _ -> malformed row)
         regions)
  in
  Ok
    (List.sort
       (fun a b -> String.compare a.start b.start)
       (List.sort_uniq (fun a b -> Int64.compare a.context.id b.context.id) parsed))

let table_id link name =
  let%bind ids = Quiver_mysql.Link.run_query link (Table_id name) in
  match ids with
  | [ [| Some id |] ] -> Option.to_result ~none:(`Region (Malformed [ Some id ])) (Int64.of_string_opt id)
  | _ -> Error (`Region (No_table name))

let of_table link name =
  let%bind table = table_id link name in
  let%bind regions = Quiver_mysql.Link.run_query link (Table_regions table) in
  let ids = List.sort_uniq Int64.compare (List.filter_map (fun row -> Option.bind row.(0) Int64.of_string_opt) regions) in
  match ids with
  | [] -> Ok (table, [])
  | first :: rest ->
      let%bind peers = Quiver_mysql.Link.run_query link (Region_peers (first, rest)) in
      Result.map (fun regions -> (table, regions)) (of_rows ~regions ~peers)
