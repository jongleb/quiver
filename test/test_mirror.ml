open Sexplib0.Sexp_conv
open Quiver_tikv

(* Where the model TiDB refuses. *)
type failing = Never | At_create | At_described | At_keys | At_checksum | At_rebase | At_rename [@@deriving sexp_of]

(* What TiDB's checksum of the copy says against what was copied. *)
type sums = Same | Fewer_keys | Other_crc [@@deriving sexp_of]

(* Whether the other cluster agrees with the source. *)
type agreement = Agree | Differ [@@deriving sexp_of]

(* Whether the other cluster has a placement policy the source's definition names. *)
type presence = Present | Present_otherwise_cased | Absent [@@deriving sexp_of]

(* Where the copy goes, and what the other cluster says. *)
type other = {
  collations : agreement;
  ids : agreement;
  definition : agreement;
  policies : (string * presence) list;
  placement : Mirror.placement;
}
[@@deriving sexp_of]

type place = Here | There of other [@@deriving sexp_of]

(* What the source's GC says of the copy's version: held, passed it already, or passed it meanwhile. *)
type holding = Held | Passed_before | Lapsed [@@deriving sexp_of]

type case = { place : place; changing : agreement; partitions : int; allocators : (string * int64) list; sums : sums; failing : failing; holding : holding }
[@@deriving sexp_of]

let gen =
  QCheck2.Gen.(
    let agreement = oneof_weighted [ (4, return Agree); (1, return Differ) ] in
    let* place =
      oneof
        [
          return Here;
          (let* collations = agreement and* ids = agreement and* definition = agreement in
           let* policies =
             list_size (int_range 0 3)
               (pair (oneof_list [ "p1"; "p 2"; "p`q" ]) (oneof_weighted [ (3, return Present); (1, return Present_otherwise_cased); (1, return Absent) ]))
           and* placement = oneof_list [ Mirror.Kept_placement; Default_placement ] in
           return (There { collations; ids; definition; policies; placement }));
        ]
    and* changing = oneof_weighted [ (6, return Agree); (1, return Differ) ]
    and* partitions = oneof_weighted [ (5, return 0); (1, int_range 1 4) ]
    and* allocators =
      list_size (int_range 0 4)
        (pair
           (oneof_weighted [ (3, return "_TIDB_ROWID"); (3, return "AUTO_INCREMENT"); (2, return "AUTO_RANDOM"); (1, return "SEQUENCE"); (1, return "WEIRD") ])
           (map Int64.of_int (int_range 1 1_000_000)))
    and* sums = oneof_weighted [ (4, return Same); (1, return Fewer_keys); (1, return Other_crc) ]
    and* failing =
      oneof_weighted
        [ (5, return Never); (1, return At_create); (1, return At_described); (1, return At_keys); (1, return At_checksum); (1, return At_rebase); (1, return At_rename) ]
    and* holding = oneof_weighted [ (6, return Held); (1, return Passed_before); (1, return Lapsed) ] in
    return { place; changing; partitions; allocators; sums; failing; holding })

let body = " (\n  `a` int COMMENT 'it''s \\\\ /*T![placement] PLACEMENT POLICY=`decoy` */'\n) ENGINE=InnoDB"

let clauses policies =
  String.concat ""
    (List.map
       (fun (name, _) -> " /*T![placement] PLACEMENT POLICY=`" ^ Base.String.substr_replace_all name ~pattern:"`" ~with_:"``" ^ "` */")
       policies)

let name database table : Quiver_core.Schema.Name.t = { database = Support.ident database; table = Support.ident table }
let source = name "d" "src" and target = name "e" "dst" and service = name "e" "q_model"
let copied : Mirror.copied = { keys = 10; bytes = 100; checksum = 0x8000_0000_0000_0001L }
let described ~id ~column ~state ~partitions : Info.t =
  let layout : Info.layout =
    match List.init partitions (fun i : Info.partition -> { name = Printf.sprintf "p%d" i; id = Int64.add id (Int64.of_int (i + 1)) }) with
    | first :: rest -> Partitioned (first, rest)
    | [] -> Whole
  in
  {
    id;
    columns = [ { name = "a"; id = column; state; missing = Unset; kind = Bytes } ];
    indexes = [ { name = "ka"; id = 1L; state = Public; scope = Local } ];
    handle = Integer_key;
    layout;
  }

(* Statements, each with the cluster it ran in. *)
type ran = Source_ran of Quiver_mysql.Statement.t | Target_ran of Quiver_mysql.Statement.t

let run case =
  let executed = ref [] and keyed = ref [] and asked = ref [] and holding = ref None and source_clock = ref 100L and target_clock = ref 5000L in
  let refused = Error (`Region (Region.Malformed [ Some "refused" ])) in
  let cell text = Some text in
  let there = match case.place with Here -> None | There there -> Some there in
  let side ~here ~clock ~collation ~column ~id ~allocators ~tables ~policies : _ Mirror.side =
    {
      query =
        (fun query ->
          match query with
          | New_collations -> Ok [ [| cell collation |] ]
          | Binlog_status _ ->
              clock := Int64.add !clock 7L;
              Ok [ [| cell "tidb-binlog"; cell (Int64.to_string !clock) |] ]
          | Show_create (Table _) ->
              let definition =
                match there with
                | Some { definition = Differ; _ } -> "CREATE VIEW `src` AS SELECT 1"
                | Some { policies; _ } -> "CREATE TABLE `src`" ^ body ^ clauses policies
                | None -> "CREATE TABLE `src`" ^ body
              in
              Ok [ [| cell "src"; cell definition |] ]
          | Placement_policies ->
              Ok
                (List.filter_map
                   (fun (name, presence) ->
                     match presence with Present -> Some [| cell name |] | Present_otherwise_cased -> Some [| cell (String.uppercase_ascii name) |] | Absent -> None)
                   policies)
          | Checksum table when not (List.mem table tables) -> refused
          | Checksum _ when case.failing = At_checksum -> refused
          | Checksum _ ->
              let keys = match case.sums with Fewer_keys -> copied.keys - 1 | Same | Other_crc -> copied.keys
              and crc = match case.sums with Other_crc -> 1L | Same | Fewer_keys -> copied.checksum in
              Ok [ [| cell "e"; cell "q_model"; cell (Printf.sprintf "%Lu" crc); cell (Int.to_string keys); cell (Int.to_string copied.bytes) |] ]
          | Next_row_ids _ -> Ok (List.map (fun (kind, next) -> [| cell "d"; cell "src"; cell "c"; cell (Int64.to_string next); cell kind |]) allocators)
          | _ -> refused);
      execute =
        (fun statement ->
          executed := here statement :: !executed;
          match (statement, case.failing) with
          | (Create_like _ | Create_table _), At_create | Rebase _, At_rebase | Rename_table _, At_rename -> refused
          | _ -> Ok ());
      server = Result.get_ok (Quiver_mysql.Server.of_version_string "8.0.11-TiDB-v8.5.8");
      described =
        (fun table ~at ->
          asked := (table, at, !holding) :: !asked;
          let state : Info.state = match case.changing with Agree -> Public | Differ -> Changing in
          match case.failing with At_described -> refused | _ -> Ok (described ~id ~column ~state ~partitions:case.partitions));
    }
  in
  let source_side = side ~here:(fun statement -> Source_ran statement) ~clock:source_clock ~collation:"True" ~column:1 ~id:75L ~allocators:case.allocators
      ~tables:(match there with None -> [ source; service ] | Some _ -> [ source ]) ~policies:[] in
  let target_side, cluster =
    match there with
    | None -> (source_side, Mirror.Same_cluster)
    | Some there ->
        let collation = match there.collations with Agree -> "TRUE" | Differ -> "False"
        and column = match there.ids with Agree -> 1 | Differ -> 2 in
        (side ~here:(fun statement -> Target_ran statement) ~clock:target_clock ~collation ~column ~id:300L ~allocators:[] ~tables:[ service ] ~policies:there.policies, Other_cluster ())
  in
  let session : _ Mirror.session =
    {
      source_side;
      target_side;
      cluster;
      placement = (match there with Some there -> there.placement | None -> Kept_placement);
      name = (fun () -> "q_model");
      keys =
        (fun plan ~version ~commit_ts ->
          keyed := (Renumber.tables plan, version, commit_ts, !holding) :: !keyed;
          match case.failing with At_keys -> refused | _ -> Ok copied);
      kept =
        (fun ~version work ->
          let passed = Error (`Pd (Pd.Gc_passed { version; least = Int64.succ version })) in
          match case.holding with
          | Passed_before -> passed
          | Held | Lapsed ->
              holding := Some version;
              let result = work () in
              holding := None;
              Result.bind result (fun result -> match case.holding with Lapsed -> passed | Held | Passed_before -> Ok result));
    }
  in
  let result = Mirror.through session ~source ~target in
  (result, List.rev !executed, List.rev !keyed, List.rev !asked)

let show statements =
  String.concat "; "
    (List.map
       (function
         | Source_ran statement -> "source: " ^ Quiver_mysql.Statement.sql statement
         | Target_ran statement -> "target: " ^ Quiver_mysql.Statement.sql statement)
       statements)

let copies =
  QCheck2.Test.make ~count:3000
    ~name:"a copy is made in its cluster, checked, moved past the source's ids and renamed, or the half-made table is dropped"
    ~print:(fun case -> Base.Sexp.to_string_hum (sexp_of_case case))
    gen
    (fun case ->
      let result, executed, keyed, asked = run case in
      let ran statement = match case.place with Here -> Source_ran statement | There _ -> Target_ran statement in
      let unknown = List.exists (fun (kind, _) -> kind = "WEIRD") case.allocators in
      let most kinds =
        List.fold_left (fun most (kind, next) -> if List.mem kind kinds then Some (Option.fold ~none:next ~some:(Int64.max next) most) else most) None case.allocators
      in
      let rebases =
        List.filter_map Fun.id
          [
            Option.map (fun next -> Quiver_mysql.Statement.Rebase { table = service; allocator = Auto_increment; next }) (most [ "_TIDB_ROWID"; "AUTO_INCREMENT" ]);
            Option.map (fun next -> Quiver_mysql.Statement.Rebase { table = service; allocator = Auto_random; next }) (most [ "AUTO_RANDOM" ]);
          ]
      in
      let create : Quiver_mysql.Statement.t =
        match case.place with
        | Here -> Create_like { table = service; like = source }
        | There { placement = Kept_placement; policies; _ } -> Create_table { table = service; body = body ^ clauses policies }
        | There { placement = Default_placement; _ } -> Create_table { table = service; body }
      in
      let missing =
        match case.place with
        | There { placement = Kept_placement; policies; _ } ->
            let present name = List.exists (fun (other, presence) -> other = name && presence <> Absent) policies in
            List.fold_left (fun missing (name, _) -> if present name || List.mem name missing then missing else missing @ [ name ]) [] policies
        | There { placement = Default_placement; _ } | Here -> []
      in
      let refused = Error (`Region (Region.Malformed [ Some "refused" ])) in
      let failing = match (case.failing, rebases) with At_rebase, [] -> Never | failing, _ -> failing in
      let into = match case.place with Here -> 75L | There _ -> 300L in
      let physical =
        match case.partitions with
        | 0 -> [ (75L, into) ]
        | n -> List.init n (fun i -> (Int64.of_int (76 + i), Int64.add into (Int64.of_int (i + 1))))
      in
      let ordered =
        match (keyed, case.place) with
        | [ (tables, version, commit_ts, held) ], Here -> tables = physical && Int64.compare version commit_ts < 0 && held = Some version
        | [ (tables, version, commit_ts, held) ], There _ ->
            tables = physical && Int64.compare version 1000L < 0 && Int64.compare commit_ts 5000L > 0 && held = Some version
        | _ -> false
      in
      let described_at =
        match (asked, keyed) with
        | [ (first, at, held); (second, later, _) ], [ (_, version, commit_ts, _) ] ->
            first = source && at = version && held = Some version && second = service && later = commit_ts
        | _, _ -> false
      in
      let dropped = match List.rev executed with last :: _ -> last = ran (Drop_table service) | [] -> false in
      match (result, case.place) with
      | _, There { collations = Differ; _ } ->
          result = Error (`Mirror (Mirror.Collations_differ { source = "True"; target = "False" })) && executed = [] && keyed = []
      | _, There { definition = Differ; _ } -> result = Error (`Mirror (Mirror.Definition source)) && executed = [] && keyed = []
      | _, There _ when missing <> [] ->
          result = Error (`Mirror (Mirror.Unknown_policies (List.hd missing, List.tl missing))) && executed = [] && keyed = []
      | _ when failing = At_create -> result = refused && executed = [ ran create ] && keyed = []
      | Error (`Pd (Pd.Gc_passed _)), _ when case.holding = Passed_before -> dropped && keyed = [] && asked = []
      | _ when case.holding = Passed_before -> false
      | _ when failing = At_described -> result = refused && dropped && keyed = []
      | _ when case.changing = Differ -> result = Error (`Renumber (Renumber.Changing "a")) && dropped && keyed = []
      | Ok got, _ ->
          case.holding = Held && failing = Never && case.sums = Same && (not unknown) && got = copied && ordered && described_at
          && show executed = show (List.map ran ((create :: rebases) @ [ Rename_table { from = service; into = target } ]))
      | Error error, _ ->
          let expected_error =
            match (failing, case.sums) with
            | At_keys, _ -> error = `Region (Region.Malformed [ Some "refused" ])
            | _ when case.holding = Lapsed -> ( match error with `Pd (Pd.Gc_passed _) -> true | _ -> false)
            | At_checksum, _ -> error = `Region (Region.Malformed [ Some "refused" ])
            | _, (Fewer_keys | Other_crc) ->
                let target : Mirror.copied =
                  { copied with keys = (if case.sums = Fewer_keys then copied.keys - 1 else copied.keys); checksum = (if case.sums = Other_crc then 1L else copied.checksum) }
                in
                error = `Mirror (Mirror.Mismatch { copied; target })
            | _ when unknown -> error = `Mirror (Mirror.Allocator "WEIRD")
            | (At_rebase | At_rename), Same -> error = `Region (Region.Malformed [ Some "refused" ])
            | (Never | At_create | At_described), Same -> false
          in
          expected_error && ordered && dropped
          && List.for_all (fun ran -> match (ran, case.place) with Source_ran _, There _ -> false | _ -> true) executed
          && not (List.exists (function Source_ran (Rename_table _) | Target_ran (Rename_table _) -> failing <> At_rename | _ -> false) executed))

let tests = [ QCheck_alcotest.to_alcotest copies ]
