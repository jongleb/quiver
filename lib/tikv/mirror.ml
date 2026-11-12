open Base.Result.Let_syntax

type sizes = { scan : int; job : int; batch : int }
type stores = Imported | Untouched
type regions = Held | Scheduled
type placement = Kept_placement | Default_placement [@@deriving sexp_of ~portable]

type settings = {
  timeout : Core.Time_ns.Span.t;
  sizes : sizes;
  workers : int;
  stores : stores;
  regions : regions;
  placement : placement;
}
type reached = { link : Quiver_mysql.Link.t; security : Http2.security }
type copied = { keys : int; bytes : int; checksum : int64 } [@@deriving sexp_of ~portable]

type moved = { source : Quiver_core.Schema.Name.t; target : Quiver_core.Schema.Name.t } [@@deriving sexp_of ~portable]
type mirrored = { version : int64; copies : (moved * copied) list }

type error =
  | Mismatch of { table : Quiver_core.Schema.Name.t; copied : copied; target : copied }
  | Allocator of string
  | Collations_differ of { source : string; target : string }
  | Definition of Quiver_core.Schema.Name.t
  | Undescribed of Quiver_core.Schema.Name.t
  | Unknown_policies of string * string list
  | Taken of Quiver_core.Schema.Name.t * Quiver_core.Schema.Name.t list
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
  | `Hpack of Hpack.error
  | `Renumber of Renumber.error ]

let learned pd ~start ~stop =
  let rec go tries =
    match Pd.regions pd ~start ~stop with
    | Error (`Pd (Pd.Gap _ | Leaderless _)) when tries < Read.attempts ->
        Unix.sleepf (Float.min 1. (0.002 *. Float.pow 2. (Float.of_int tries)));
        go (tries + 1)
    | result -> result
  in
  go 0

type range = { start : string; stop : string }

let ranges ~source regions =
  let first, past = Codec.table source in
  List.filter_map
    (fun (region : Region.t) ->
      let start = Base.String.max region.start first
      and stop = match region.stop with Endless -> past | Before stop -> Base.String.min stop past in
      if String.compare start stop >= 0 then None else Some { start; stop })
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

let divide (range : range) points = List.map2 (fun start stop -> { start; stop }) (range.start :: points) (points @ [ range.stop ])

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

let locate pd known ~start ~stop ~unheld (freshness : Read.freshness) key =
  if String.compare key start < 0 || String.compare stop key <= 0 then
    Result.bind (learned pd ~start:key ~stop:(Before (key ^ "\x00"))) (fun regions ->
        Option.to_result ~none:(unheld key) (Region.find regions key))
  else
    let%bind () =
      match freshness with
      | Known -> Ok ()
      | Afresh -> Result.map (fun regions -> known := regions) (learned pd ~start ~stop:(Before stop))
    in
    Option.to_result ~none:(unheld key) (Region.find !known key)

let reader pd stores ~start ~stop ~version ~limit =
  let%bind regions = learned pd ~start ~stop:(Before stop) in
  Ok
    ({
       locate = locate pd (ref regions) ~start ~stop ~unheld:(fun key -> `Read (Read.Unheld key));
       scan =
         (fun region settled ~start ~stop ->
           let%bind connection = Stores.connection stores region.address in
           Kv.scan connection region.context settled ~direction:Forward ~start ~stop ~version ~limit);
       check =
         (fun region check ->
           let%bind connection = Stores.connection stores region.address in
           Kv.check connection region.context check);
       secondaries =
         (fun region ~version keys ->
           let%bind connection = Stores.connection stores region.address in
           Kv.check_secondaries connection region.context ~version keys);
       now = (fun () -> Pd.tso pd);
       pause = (fun span -> Unix.sleepf (Core.Time_ns.Span.to_sec span));
     }
      : _ Read.store)

let range ~source ~target ~version ~commit_ts ~sizes ~plan (into, { start; stop }) =
  let source_address, source_dial = source and target_address, target_dial = target in
  let%bind source_pd = Pd.connect source_dial source_address in
  let%bind pd =
    Result.map_error
      (fun error ->
        Pd.close source_pd;
        error)
      (Pd.connect target_dial target_address)
  in
  let source_stores = Stores.create source_dial and stores = Stores.create target_dial and random = Random.State.make_self_init () in
  let copied =
    let%bind read = reader source_pd source_stores ~start ~stop ~version ~limit:sizes.scan in
    let target_start, target_stop = Codec.table into in
    let%bind target_regions = learned pd ~start:target_start ~stop:(Before target_stop) in
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
    let gathered = Pairs.create () and size = ref 0 and keys = ref 0 and bytes = ref 0 and checksum = ref 0L and last = ref "" in
    let flush () =
      let delivered = Ingest.deliver write ~commit_ts ~batch:sizes.batch (Pairs.all gathered) in
      Pairs.clear gathered;
      size := 0;
      last := "";
      delivered
    in
    let%bind () =
      Read.range read ~version ~start ~stop:(Before stop) ~limit:sizes.scan ~f:(fun pairs ->
          let%bind () =
            Base.List.fold_result pairs ~init:() ~f:(fun () (pair : Kv.pair) ->
                let%bind moved = Renumber.key plan pair.key in
                Option.fold ~none:(Ok ())
                  ~some:(fun key ->
                    let%bind value = Renumber.value plan ~key:pair.key pair.value in
                    let%bind () = if String.compare key !last < 0 then flush () else Ok () in
                    let cost = String.length key + String.length value in
                    Pairs.add gathered key value;
                    last := key;
                    checksum := Int64.logxor !checksum (Crc64.pair key value);
                    size := !size + cost;
                    incr keys;
                    bytes := !bytes + cost;
                    Ok ())
                  moved)
          in
          if !size >= sizes.job then flush () else Ok ())
    in
    let%bind () = flush () in
    Ok { keys = !keys; bytes = !bytes; checksum = !checksum }
  in
  Stores.close source_stores;
  Stores.close stores;
  Pd.close source_pd;
  Pd.close pd;
  copied

let pd_of link =
  let%bind instances = Quiver_mysql.Link.run_query link Pd_instances in
  match instances with
  | [| Some address |] :: _ -> Ok address
  | row :: _ -> Error (`Region (Region.Malformed (Array.to_list row)))
  | [] -> Error (`Region (Region.Malformed []))

let described link (name : Quiver_core.Schema.Name.t) ~at ~dial =
  let%bind table = Region.table_id link name in
  let%bind address = pd_of link in
  let%bind pd = Pd.connect dial address in
  let stores = Stores.create dial in
  let pairs ~start ~stop =
    let got = ref [] in
    let%bind read = reader pd stores ~start ~stop ~version:at ~limit:256 in
    let%bind () =
      Read.range read ~version:at ~start ~stop:(Before stop) ~limit:256 ~f:(fun pairs ->
          got := List.rev_append pairs !got;
          Ok ())
    in
    Ok (List.rev !got)
  in
  let info =
    let start, stop = Codec.databases in
    let%bind databases = pairs ~start ~stop in
    let wanted = String.lowercase_ascii (name.database :> string) in
    let%bind found =
      Base.List.fold_result databases ~init:None ~f:(fun found (pair : Kv.pair) ->
          let%bind database, id = Info.database pair.value in
          Ok (if String.equal database wanted then Some id else found))
    in
    let%bind database = Option.to_result ~none:(`Mirror (Undescribed name)) found in
    let key = Codec.described ~database ~table in
    let%bind described = pairs ~start:key ~stop:(key ^ "\x00") in
    match described with [ pair ] -> Info.of_json pair.value | _ -> Error (`Mirror (Undescribed name))
  in
  Stores.close stores;
  Pd.close pd;
  info

let table ~(from : reached) ~(into : reached) ~plan ~version ~commit_ts { timeout; sizes; workers; stores = tuning; regions = scheduling; placement = _ } =
  let tables = Renumber.tables plan in
  let source_dial : Http2.dial = { timeout; security = from.security }
  and target_dial : Http2.dial = { timeout; security = into.security } in
  let%bind source_address = pd_of from.link in
  let%bind target_address = pd_of into.link in
  let copy () =
    let%bind source_pd = Pd.connect source_dial source_address in
    let%bind pd =
      Result.map_error
        (fun error ->
          Pd.close source_pd;
          error)
        (Pd.connect target_dial target_address)
    in
    let%bind held =
      Result.map_error
        (fun error ->
          Pd.close source_pd;
          Pd.close pd;
          error)
        (Base.Result.all
           (List.map
              (fun (source, _) ->
                let first, past = Codec.table source in
                learned source_pd ~start:first ~stop:(Before past))
              tables))
    in
    let regions = List.concat held in
    let whole = List.concat (List.map2 (fun (source, into) regions -> List.map (fun part -> (into, part)) (ranges ~source regions)) tables held) in
    let parts = Int.max 1 (((4 * workers) + List.length whole - 1) / Int.max 1 (List.length whole)) in
    let source_stores = Stores.create source_dial and stores = Stores.create target_dial in
    let divided =
      List.concat_map
        (fun (into, (part : range)) ->
          let edge direction ~start ~stop =
            Option.bind (Region.find regions start) (fun (region : Region.t) ->
                match
                  Result.bind (Stores.connection source_stores region.address) (fun connection ->
                      Kv.scan connection region.context Kv.nothing_settled ~direction ~start ~stop:(Before stop) ~version ~limit:1)
                with
                | Ok (Reply (Pairs [ pair ])) -> Some pair.key
                | Ok (Reply (Pairs _ | Locks _) | Region_error _ | Key_error _) | Error _ -> None)
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
          List.map
            (fun range -> (into, range))
            (divide part (List.sort_uniq String.compare (List.filter (fun key -> String.compare part.start key < 0 && String.compare key part.stop < 0) (kinds part.start [])))))
        whole
    in
    let split =
      let keys =
        List.sort_uniq String.compare
          (List.concat_map
             (fun (_, into) ->
               let first, past = Codec.table into in
               [ first; past ])
             tables
          @ List.filter_map (fun (_, (part : range)) -> Option.join (Result.to_option (Renumber.key plan part.start))) divided)
      in
      let rec go keys tries made =
        let%bind held =
          Base.Result.all
            (List.map
               (fun (_, into) ->
                 let first, past = Codec.table into in
                 learned pd ~start:first ~stop:(Before past))
               tables)
        in
        let regions = List.sort_uniq (fun (a : Region.t) (b : Region.t) -> Int64.compare a.context.id b.context.id) (List.concat held) in
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
    Stores.close source_stores;
    Stores.close stores;
    Pd.close source_pd;
    Pd.close pd;
    let%bind () = split in
    let work () =
      Quiver_core.Concurrent.spread ~workers
        (Iarray.of_list divided)
        (fun part ->
          (range ~source:(source_address, source_dial) ~target:(target_address, target_dial) ~version ~commit_ts ~sizes ~plan part
            : (copied, failure) result))
    in
    let%bind copied =
      match tuning with
      | Untouched -> Ok (work ())
      | Imported ->
          Quiver_core.Concurrent.held ~every:(Core.Time_ns.Span.of_int_sec 300) ~on:Import.Import ~off:Normal
            (fun mode ->
              let stores = Stores.create target_dial in
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
  in
  match (scheduling, tables, Http2.address target_address) with
  | Scheduled, _, _ | Held, [], _ -> copy ()
  | Held, _ :: _, None -> Error (`Region (Region.Malformed [ Some target_address ]))
  | Held, (_, first) :: rest, Some (host, port) ->
      let rule =
        Label.deny ~name:[%string "quiver_mirror_%{commit_ts#Int64}"] (Codec.table first) (List.map (fun (_, into) -> Codec.table into) rest)
      in
      Result.join
        (Quiver_core.Concurrent.held ~every:(Core.Time_ns.Span.of_int_sec 60)
           ~on:(Pd.Hold_for (Core.Time_ns.Span.of_int_sec 180))
           ~off:Let_go (fun hold -> Label.keep target_dial ~host ~port rule hold) copy)

type 'link cluster = Same_cluster | Other_cluster of 'link

type 'e side = {
  query : Quiver_mysql.Query.t -> (string option array list, 'e) result;
  execute : Quiver_mysql.Statement.t -> (unit, 'e) result;
  server : Quiver_mysql.Server.t;
  described : Quiver_core.Schema.Name.t -> at:int64 -> (Info.t, 'e) result;
}

type 'e session = {
  source_side : 'e side;
  target_side : 'e side;
  cluster : unit cluster;
  placement : placement;
  name : unit -> string;
  keys : Renumber.t -> version:int64 -> commit_ts:int64 -> (copied, 'e) result;
  kept : 'a. version:int64 -> (unit -> ('a, 'e) result) -> ('a, 'e) result;
  finished : moved -> copied -> unit;
}

let through session tables =
  let { source_side; target_side; _ } = session in
  let malformed row = Error (`Region (Region.Malformed (Array.to_list row))) in
  let each l ~f = Result.map List.rev (Base.List.fold_result l ~init:[] ~f:(fun acc x -> Result.map (fun y -> y :: acc) (f x))) in
  let one side query =
    let%bind rows = side.query query in
    match rows with [ [| Some value |] ] -> Ok value | row :: _ -> malformed row | [] -> malformed [||]
  in
  let%bind () =
    match session.cluster with
    | Same_cluster -> Ok ()
    | Other_cluster () ->
        let%bind here = one source_side New_collations in
        let%bind there = one target_side New_collations in
        if String.equal (String.lowercase_ascii here) (String.lowercase_ascii there) then Ok ()
        else Error (`Mirror (Collations_differ { source = here; target = there }))
  in
  let%bind planned =
    each tables ~f:(fun moved ->
        let%bind service =
          let name = session.name () in
          Result.map
            (fun table : Quiver_core.Schema.Name.t -> { moved.target with table })
            (Result.map_error (fun _ -> `Region (Region.Malformed [ Some name ])) (Quiver_core.Ident.of_string name))
        in
        match session.cluster with
        | Same_cluster -> Ok (moved, service, Quiver_mysql.Statement.Create_like { table = service; like = moved.source }, [])
        | Other_cluster () -> (
            let%bind rows = source_side.query (Show_create (Table moved.source)) in
            match rows with
            | [ [| _; Some definition |] ] -> (
                match Base.String.chop_prefix definition ~prefix:("CREATE TABLE " ^ Quiver_core.Ident.backquoted moved.source.table) with
                | None -> Error (`Mirror (Definition moved.source))
                | Some body -> (
                    let%bind placed = Placement.read body in
                    match session.placement with
                    | Default_placement -> Ok (moved, service, Quiver_mysql.Statement.Create_table { table = service; body = Placement.without placed }, [])
                    | Kept_placement -> Ok (moved, service, Quiver_mysql.Statement.Create_table { table = service; body }, Placement.policies placed)))
            | row :: _ -> malformed row
            | [] -> malformed [||]))
  in
  let%bind () =
    match List.concat_map (fun (_, _, _, policies) -> policies) planned with
    | [] -> Ok ()
    | named -> (
        let%bind rows = target_side.query Placement_policies in
        let there = List.filter_map (fun row -> match row with [| Some name |] -> Some (String.lowercase_ascii name) | _ -> None) rows in
        let lacking =
          List.fold_left
            (fun lacking name ->
              if List.mem (String.lowercase_ascii name) there || List.mem name lacking then lacking else name :: lacking)
            [] named
        in
        match List.rev lacking with [] -> Ok () | first :: rest -> Error (`Mirror (Unknown_policies (first, rest))))
  in
  let databases = Base.List.dedup_and_sort ~compare:Quiver_core.Ident.compare (List.map (fun (moved : moved) -> moved.target.database) tables) in
  let%bind () =
    let folded (database : Quiver_core.Ident.t) table = (String.lowercase_ascii (database :> string), String.lowercase_ascii table) in
    let%bind there =
      each databases ~f:(fun database ->
          let%bind rows = target_side.query (Names database) in
          each rows ~f:(fun row -> match row with [| Some table |] -> Ok (folded database table) | row -> malformed row))
    in
    let taken, _ =
      List.fold_left
        (fun (taken, seen) (moved : moved) ->
          let name = folded moved.target.database (moved.target.table :> string) in
          if not (List.mem name seen) then (taken, name :: seen)
          else if List.mem moved.target taken then (taken, seen)
          else (moved.target :: taken, seen))
        ([], List.concat there) tables
    in
    match List.rev taken with [] -> Ok () | first :: rest -> Error (`Mirror (Taken (first, rest)))
  in
  let tso side =
    let%bind rows = side.query (Binlog_status side.server) in
    match rows with
    | row :: _ when Array.length row > 1 ->
        Option.fold ~none:(malformed row) ~some:Result.ok (Option.bind row.(1) Int64.of_string_opt)
    | row :: _ -> malformed row
    | [] -> malformed [||]
  in
  let%bind version = tso source_side in
  let%bind () =
    Base.List.fold_result databases ~init:() ~f:(fun () database -> target_side.execute (Create_database database))
  in
  let drop made = List.iter (fun (_, service) -> ignore (target_side.execute (Drop_table service))) made in
  let rec create made planned =
    match planned with
    | [] -> Ok (List.rev made)
    | (moved, service, statement, _) :: rest -> (
        match target_side.execute statement with
        | Ok () -> create ((moved, service) :: made) rest
        | Error _ as error ->
            drop (List.rev made);
            error)
  in
  let%bind made = create [] planned in
  let mirrored =
    let%bind copies =
      session.kept ~version (fun () ->
          each made ~f:(fun (moved, service) ->
              let%bind commit_ts = tso target_side in
              let%bind here = source_side.described moved.source ~at:version in
              let%bind there = target_side.described service ~at:commit_ts in
              let%bind plan = Renumber.make ~source:here ~target:there in
              let%bind copied = session.keys plan ~version ~commit_ts in
              session.finished moved copied;
              Ok (moved, service, copied)))
    in
    let%bind () =
      Base.List.fold_result copies ~init:() ~f:(fun () ((moved : moved), service, copied) ->
          let%bind sums = target_side.query (Checksum service) in
          let%bind () =
            match sums with
            | [ ([| _; _; Some checksum; Some keys; Some bytes |] as row) ] -> (
                match (Int64.of_string_opt ("0u" ^ checksum), int_of_string_opt keys, int_of_string_opt bytes) with
                | Some checksum, Some keys, Some bytes ->
                    let target = { keys; bytes; checksum } in
                    if target = copied then Ok () else Error (`Mirror (Mismatch { table = moved.source; copied; target }))
                | _ -> malformed row)
            | row :: _ -> malformed row
            | [] -> malformed [||]
          in
          let%bind allocators = source_side.query (Next_row_ids moved.source) in
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
          Base.List.fold_result [ Quiver_mysql.Statement.Auto_increment; Auto_random ] ~init:() ~f:(fun () allocator ->
              let next =
                List.fold_left
                  (fun most (kind, next) -> if kind = allocator then Some (Option.fold ~none:next ~some:(Int64.max next) most) else most)
                  None rebased
              in
              Option.fold ~none:(Ok ()) ~some:(fun next -> target_side.execute (Rebase { table = service; allocator; next })) next))
    in
    let%bind () =
      match List.map (fun ((moved : moved), service, _) : Quiver_mysql.Statement.renaming -> { from = service; into = moved.target }) copies with
      | [] -> Ok ()
      | first :: rest -> target_side.execute (Rename_tables (first, rest))
    in
    Ok { version; copies = List.map (fun (moved, _, copied) -> (moved, copied)) copies }
  in
  let () = match mirrored with Ok _ -> () | Error _ -> drop made in
  mirrored

let copy (from : reached) ~into ~finished tables settings =
  let random = Random.State.make_self_init () in
  let side ({ link; security } : reached) =
    {
      query = Quiver_mysql.Link.run_query link;
      execute = Quiver_mysql.Link.run_statement link;
      server = Quiver_mysql.Link.server link;
      described = (fun table ~at -> described link table ~at ~dial:{ timeout = settings.timeout; security });
    }
  in
  let onto, cluster = match into with Same_cluster -> (from, Same_cluster) | Other_cluster onto -> (onto, Other_cluster ()) in
  let hex () = String.init 16 (fun _ -> "0123456789abcdef".[Random.State.int random 16]) in
  through
    {
      source_side = side from;
      target_side = side onto;
      cluster;
      placement = settings.placement;
      name = (fun () -> "q_" ^ hex ());
      keys = (fun plan ~version ~commit_ts -> table ~from ~into:onto ~plan ~version ~commit_ts settings);
      kept =
        (fun ~version work ->
          let%bind address = pd_of from.link in
          let service = "quiver_mirror_" ^ hex () and dial : Http2.dial = { timeout = settings.timeout; security = from.security } in
          Pd.held ~every:(Core.Time_ns.Span.of_int_sec 60)
            (fun hold ->
              let%bind pd = Pd.connect dial address in
              let kept = Pd.keep pd ~service hold version in
              Pd.close pd;
              kept)
            work);
      finished;
    }
    tables
