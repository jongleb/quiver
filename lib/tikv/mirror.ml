open Base.Result.Let_syntax

type sizes = { scan : int; job : int; batch : int }
type stores = Imported | Untouched
type settings = { timeout : Core.Time_ns.Span.t; sizes : sizes; workers : int; stores : stores }
type copied = { keys : int; bytes : int; checksum : int64 } [@@deriving sexp_of ~portable]

type error =
  | Partitioned of Quiver_core.Schema.Name.t
  | Mismatch of { copied : copied; target : copied }
  | Allocator of string
  | Collations_differ of { source : string; target : string }
  | Definition of Quiver_core.Schema.Name.t
  | Ids_differ of { source : Status.ids; target : Status.ids }
[@@deriving sexp_of ~portable]

type failure =
  [ `Read of Read.error
  | `Ingest of Ingest.error
  | `Pd of Pd.error
  | `Region of Region.error
  | `Transport of Quiver_mysql.Transport.error
  | `Proto of Proto.error
  | `Grpc of Grpc.error
  | `Http2 of Http2.error
  | `Hpack of Hpack.error ]

let learned pd ~start ~stop =
  let rec go tries =
    match Pd.regions pd ~start ~stop with
    | Error (`Pd (Pd.Gap _ | Leaderless _)) when tries < Read.attempts ->
        Unix.sleepf (Float.min 1. (0.002 *. Float.pow 2. (Float.of_int tries)));
        go (tries + 1)
    | result -> result
  in
  go 0

type range = { start : string; stop : string; target_start : string; target_stop : string }

let ranges ~source ~into regions =
  let first, past = Codec.table source and target_first, target_past = Codec.table into in
  List.filter_map
    (fun (region : Region.t) ->
      let start = Base.String.max region.start first
      and stop = match region.stop with Endless -> past | Before stop -> Base.String.min stop past in
      if String.compare start stop >= 0 then None
      else
        let target_start = if start = first then target_first else Codec.retabled ~into start
        and target_stop = if stop = past then target_past else Codec.retabled ~into stop in
        Some { start; stop; target_start; target_stop })
    regions

let points ~first ~last ~parts =
  let common = Base.String.common_prefix2_length first last in
  let eight text =
    Base.List.fold (List.init 8 (fun i -> i)) ~init:0L ~f:(fun acc i ->
        let byte = if common + i < String.length text then Char.code text.[common + i] else 0 in
        Int64.logor (Int64.shift_left acc 8) (Int64.of_int byte))
  in
  let low = eight first and high = eight last in
  let step = Int64.unsigned_div (Int64.sub high low) (Int64.of_int (Int.max 1 parts)) in
  let prefix = String.sub first 0 common in
  let point i =
    let n = Int64.add low (Int64.mul step (Int64.of_int i)) in
    prefix ^ String.init 8 (fun j -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical n (8 * (7 - j))) 0xFFL)))
  in
  List.filter
    (fun point -> String.compare first point < 0 && String.compare point last <= 0)
    (List.sort_uniq String.compare (List.init (Int.max 0 (parts - 1)) (fun i -> point (i + 1))))

let divide ~into (range : range) points =
  let starts = range.start :: points and stops = points @ [ range.stop ] in
  List.map2
    (fun start stop ->
      let target_start = if start = range.start then range.target_start else Codec.retabled ~into start
      and target_stop = if stop = range.stop then range.target_stop else Codec.retabled ~into stop in
      { start; stop; target_start; target_stop })
    starts stops

let cuts regions keys =
  List.filter_map
    (fun (region : Region.t) ->
      let inside =
        List.filter
          (fun key ->
            String.compare region.start key < 0
            && match region.stop with Endless -> true | Before stop -> String.compare key stop < 0)
          keys
      in
      match inside with [] -> None | _ :: _ -> Some (region, inside))
    regions

let range ~source ~target ~timeout ~version ~commit_ts ~sizes ~into { start; stop; target_start; target_stop } =
  let%bind source_pd = Pd.connect ~timeout source in
  let%bind pd =
    Result.map_error
      (fun error ->
        Pd.close source_pd;
        error)
      (Pd.connect ~timeout target)
  in
  let stores = Stores.create ~timeout and random = Random.State.make_self_init () in
  let copied =
    let%bind source_regions = learned source_pd ~start ~stop:(Before stop) in
    let%bind target_regions = learned pd ~start:target_start ~stop:(Before target_stop) in
    let locate pd known ~start ~stop ~unheld (freshness : Read.freshness) key =
      let%bind () =
        match freshness with
        | Known -> Ok ()
        | Afresh -> Result.map (fun regions -> known := regions) (learned pd ~start ~stop:(Before stop))
      in
      Option.to_result ~none:(unheld key) (Region.find !known key)
    in
    let read : _ Read.store =
      {
        locate = locate source_pd (ref source_regions) ~start ~stop ~unheld:(fun key -> `Read (Read.Unheld key));
        scan =
          (fun region ~start ~stop ->
            let%bind connection = Stores.connection stores region.address in
            Kv.scan connection region.context ~direction:Forward ~start ~stop ~version ~limit:sizes.scan);
        pause = (fun span -> Unix.sleepf (Core.Time_ns.Span.to_sec span));
      }
    in
    let write : _ Ingest.store =
      {
        locate =
          locate pd (ref target_regions) ~start:target_start ~stop:target_stop ~unheld:(fun key -> `Ingest (Ingest.Unheld key));
        write =
          (fun region job ->
            let%bind connections =
              Base.Result.all (List.map (fun (replica : Region.replica) -> Stores.connection stores replica.address) region.replicas)
            in
            let%bind outcomes = Import.write connections job in
            match Base.List.zip region.replicas outcomes with Ok written -> Ok written | Unequal_lengths -> Ok []);
        ingest =
          (fun region ssts ->
            let%bind connection = Stores.connection stores region.address in
            Import.ingest connection region.context ssts);
        uuid = (fun () -> String.init 16 (fun _ -> Char.chr (Random.State.int random 256)));
        pause = (fun span -> Unix.sleepf (Core.Time_ns.Span.to_sec span));
      }
    in
    let gathered = Pairs.create () and size = ref 0 and keys = ref 0 and bytes = ref 0 and checksum = ref 0L in
    let flush () =
      let delivered = Ingest.deliver write ~commit_ts ~batch:sizes.batch (Pairs.all gathered) in
      Pairs.clear gathered;
      size := 0;
      delivered
    in
    let%bind () =
      Read.range read ~start ~stop:(Before stop) ~limit:sizes.scan ~f:(fun pairs ->
          List.iter
            (fun (pair : Kv.pair) ->
              let cost = String.length pair.key + String.length pair.value in
              let key = Codec.retabled ~into pair.key in
              Pairs.add gathered key pair.value;
              checksum := Int64.logxor !checksum (Crc64.pair key pair.value);
              size := !size + cost;
              incr keys;
              bytes := !bytes + cost)
            pairs;
          if !size >= sizes.job then flush () else Ok ())
    in
    let%bind () = flush () in
    Ok { keys = !keys; bytes = !bytes; checksum = !checksum }
  in
  Stores.close stores;
  Pd.close source_pd;
  Pd.close pd;
  copied

let table ~from ~into:onto ~source ~target ~version ~commit_ts { timeout; sizes; workers; stores = tuning } =
  let%bind source_id = Region.table_id from source in
  let%bind into = Region.table_id onto target in
  let pd_of link =
    let%bind instances = Quiver_mysql.Link.run_query link Pd_instances in
    match instances with
    | [| Some address |] :: _ -> Ok address
    | row :: _ -> Error (`Region (Region.Malformed (Array.to_list row)))
    | [] -> Error (`Region (Region.Malformed []))
  in
  let%bind source_address = pd_of from in
  let%bind target_address = pd_of onto in
  let first, past = Codec.table source_id in
  let%bind source_pd = Pd.connect ~timeout source_address in
  let%bind pd =
    Result.map_error
      (fun error ->
        Pd.close source_pd;
        error)
      (Pd.connect ~timeout target_address)
  in
  let%bind regions =
    Result.map_error
      (fun error ->
        Pd.close source_pd;
        Pd.close pd;
        error)
      (learned source_pd ~start:first ~stop:(Before past))
  in
  let whole = ranges ~source:source_id ~into regions in
  let parts = Int.max 1 (((4 * workers) + List.length whole - 1) / Int.max 1 (List.length whole)) in
  let stores = Stores.create ~timeout in
  let divided =
    List.concat_map
      (fun (part : range) ->
        let edge direction ~start ~stop =
          Option.bind (Region.find regions start) (fun (region : Region.t) ->
              match
                Result.bind (Stores.connection stores region.address) (fun connection ->
                    Kv.scan connection region.context ~direction ~start ~stop:(Before stop) ~version ~limit:1)
              with
              | Ok (Pairs [ pair ]) -> Some pair.key
              | Ok (Pairs _ | Region_error _ | Key_error _) | Error _ -> None)
        in
        let rec kinds from cuts =
          match edge Forward ~start:from ~stop:part.stop with
          | None -> cuts
          | Some first -> (
              let stop = Option.fold ~none:part.stop ~some:(Base.String.min part.stop) (Codec.kind_end first) in
              let inside = Option.fold ~none:[] ~some:(fun last -> points ~first ~last ~parts) (edge Backward ~start:first ~stop) in
              let cuts = List.rev_append inside (if String.compare part.start first < 0 then first :: cuts else cuts) in
              match String.compare stop part.stop with
              | 0 -> cuts
              | _ -> kinds stop (stop :: cuts))
        in
        divide ~into part (List.sort_uniq String.compare (List.filter (fun key -> String.compare part.start key < 0 && String.compare key part.stop < 0) (kinds part.start []))))
      whole
  in
  let split =
    let target_first, target_past = Codec.table into in
    let keys = List.sort_uniq String.compare (List.map (fun (part : range) -> part.target_start) divided) in
    let rec go keys tries made =
      let%bind regions = learned pd ~start:target_first ~stop:(Before target_past) in
      let%bind made, refused =
        Base.List.fold_result (cuts regions keys) ~init:(made, []) ~f:(fun (made, refused) ((region : Region.t), keys) ->
            let%bind connection = Stores.connection stores region.address in
            let%bind outcome = Kv.split connection region.context keys in
            match outcome with
            | Split ids -> Ok (ids @ made, refused)
            | Split_refused _ -> Ok (made, keys @ refused))
      in
      match refused with
      | [] -> Ok made
      | _ :: _ when tries >= Read.attempts -> Ok made
      | _ :: _ ->
          Unix.sleepf (Float.min 1. (0.002 *. Float.pow 2. (Float.of_int tries)));
          go (List.sort String.compare refused) (tries + 1) made
    in
    let%bind made = go keys 0 [] in
    match made with [] -> Ok () | _ :: _ -> Result.map ignore (Pd.scatter pd (List.sort_uniq Int64.compare made))
  in
  let tikv = Pd.stores pd in
  Stores.close stores;
  Pd.close source_pd;
  Pd.close pd;
  let%bind () = split in
  let work () =
    Quiver_core.Concurrent.spread ~workers
      (Iarray.of_list divided)
      (fun range_of_region ->
        (range ~source:source_address ~target:target_address ~timeout ~version ~commit_ts ~sizes ~into range_of_region
          : (copied, failure) result))
  in
  let%bind copied =
    match tuning with
    | Untouched -> Ok (work ())
    | Imported ->
        Import.held ~every:(Core.Time_ns.Span.of_int_sec 300)
          (fun mode ->
            let stores = Stores.create ~timeout in
            let switched =
              Base.List.fold_result tikv ~init:() ~f:(fun () address ->
                  let%bind connection = Stores.connection stores address in
                  Import.switch_mode connection mode)
            in
            Stores.close stores;
            switched)
          work
  in
  let total =
    Base.List.fold_result copied ~init:{ keys = 0; bytes = 0; checksum = 0L } ~f:(fun total copied ->
        Result.map
          (fun (copied : copied) ->
            { keys = total.keys + copied.keys; bytes = total.bytes + copied.bytes; checksum = Int64.logxor total.checksum copied.checksum })
          copied)
  in
  match total with Ok total -> Ok total | Error (#failure as error) -> Error error

type 'link cluster = Same_cluster | Other_cluster of 'link

type 'e side = {
  query : Quiver_mysql.Query.t -> (string option array list, 'e) result;
  execute : Quiver_mysql.Statement.t -> (unit, 'e) result;
  server : Quiver_mysql.Server.t;
  ids : Quiver_core.Schema.Name.t -> (Status.ids, 'e) result;
}

type 'e session = {
  source_side : 'e side;
  target_side : 'e side;
  cluster : unit cluster;
  name : unit -> string;
  keys : Quiver_core.Schema.Name.t -> version:int64 -> commit_ts:int64 -> (copied, 'e) result;
}

let through session ~source ~target =
  let { source_side; target_side; _ } = session in
  let malformed row = Error (`Region (Region.Malformed (Array.to_list row))) in
  let one side query =
    let%bind rows = side.query query in
    match rows with [ [| Some value |] ] -> Ok value | row :: _ -> malformed row | [] -> malformed [||]
  in
  let%bind partitions = one source_side (Partitioned source) in
  let%bind () = match partitions with "0" -> Ok () | _ -> Error (`Mirror (Partitioned source)) in
  let%bind () =
    match session.cluster with
    | Same_cluster -> Ok ()
    | Other_cluster () ->
        let%bind here = one source_side New_collations in
        let%bind there = one target_side New_collations in
        if String.equal (String.lowercase_ascii here) (String.lowercase_ascii there) then Ok ()
        else Error (`Mirror (Collations_differ { source = here; target = there }))
  in
  let tso side =
    let%bind rows = side.query (Binlog_status side.server) in
    match rows with
    | row :: _ when Array.length row > 1 ->
        Option.fold ~none:(malformed row) ~some:Result.ok (Option.bind row.(1) Int64.of_string_opt)
    | row :: _ -> malformed row
    | [] -> malformed [||]
  in
  let%bind service =
    let name = session.name () in
    Result.map
      (fun table : Quiver_core.Schema.Name.t -> { target with table })
      (Result.map_error (fun _ -> `Region (Region.Malformed [ Some name ])) (Quiver_core.Ident.of_string name))
  in
  let%bind version = tso source_side in
  let%bind () =
    match session.cluster with
    | Same_cluster -> target_side.execute (Create_like { table = service; like = source })
    | Other_cluster () -> (
        let%bind rows = source_side.query (Show_create (Table source)) in
        match rows with
        | [ [| _; Some definition |] ] ->
            Option.fold
              ~none:(Error (`Mirror (Definition source)))
              ~some:(fun body -> target_side.execute (Create_table { table = service; body }))
              (Base.String.chop_prefix definition ~prefix:("CREATE TABLE " ^ Quiver_core.Ident.backquoted source.table))
        | row :: _ -> malformed row
        | [] -> malformed [||])
  in
  let copied =
    let%bind () =
      match session.cluster with
      | Same_cluster -> Ok ()
      | Other_cluster () ->
          let%bind here = source_side.ids source in
          let%bind there = target_side.ids service in
          if here = there then Ok () else Error (`Mirror (Ids_differ { source = here; target = there }))
    in
    let%bind commit_ts = tso target_side in
    let%bind copied = session.keys service ~version ~commit_ts in
    let%bind sums = target_side.query (Checksum service) in
    let%bind () =
      match sums with
      | [ ([| _; _; Some checksum; Some keys; Some bytes |] as row) ] -> (
          match (Int64.of_string_opt ("0u" ^ checksum), int_of_string_opt keys, int_of_string_opt bytes) with
          | Some checksum, Some keys, Some bytes ->
              let target = { keys; bytes; checksum } in
              if target = copied then Ok () else Error (`Mirror (Mismatch { copied; target }))
          | _ -> malformed row)
      | row :: _ -> malformed row
      | [] -> malformed [||]
    in
    let%bind allocators = source_side.query (Next_row_ids source) in
    let%bind rebased =
      Base.Result.all
        (List.filter_map
           (fun row ->
             match row with
             | [| _; _; _; Some next; Some kind |] -> (
                 match (Int64.of_string_opt next, kind) with
                 | Some next, ("_TIDB_ROWID" | "AUTO_INCREMENT") -> Some (Ok (Quiver_mysql.Statement.Auto_increment, next))
                 | Some next, "AUTO_RANDOM" -> Some (Ok (Quiver_mysql.Statement.Auto_random, next))
                 | Some _, "SEQUENCE" -> None
                 | Some _, kind -> Some (Error (`Mirror (Allocator kind)))
                 | None, _ -> Some (malformed row))
             | row -> Some (malformed row))
           allocators)
    in
    let%bind () =
      Base.List.fold_result [ Quiver_mysql.Statement.Auto_increment; Auto_random ] ~init:() ~f:(fun () allocator ->
          let next =
            List.fold_left
              (fun most (kind, next) -> if kind = allocator then Some (Option.fold ~none:next ~some:(Int64.max next) most) else most)
              None rebased
          in
          Option.fold ~none:(Ok ()) ~some:(fun next -> target_side.execute (Rebase { table = service; allocator; next })) next)
    in
    let%bind () = target_side.execute (Rename_table { from = service; into = target }) in
    Ok copied
  in
  let () = match copied with Ok _ -> () | Error _ -> ignore (target_side.execute (Drop_table service)) in
  copied

let copy link ~into ~source ~target settings =
  let random = Random.State.make_self_init () in
  let side link =
    {
      query = Quiver_mysql.Link.run_query link;
      execute = Quiver_mysql.Link.run_statement link;
      server = Quiver_mysql.Link.server link;
      ids =
        (fun table ->
          let%bind rows = Quiver_mysql.Link.run_query link Tidb_status in
          match rows with
          | [| Some address |] :: _ -> Status.table ~timeout:settings.timeout address table
          | row :: _ -> Error (`Region (Region.Malformed (Array.to_list row)))
          | [] -> Error (`Region (Region.Malformed [])));
    }
  in
  let onto, cluster = match into with Same_cluster -> (link, Same_cluster) | Other_cluster onto -> (onto, Other_cluster ()) in
  through
    {
      source_side = side link;
      target_side = side onto;
      cluster;
      name = (fun () -> "q_" ^ String.init 16 (fun _ -> "0123456789abcdef".[Random.State.int random 16]));
      keys = (fun service ~version ~commit_ts -> table ~from:link ~into:onto ~source ~target:service ~version ~commit_ts settings);
    }
    ~source ~target
