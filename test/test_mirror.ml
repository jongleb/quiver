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

(* Whether the name of the copy of the table [at] is free in the target. *)
type taken = Free | Taken | Taken_otherwise_cased | Repeated [@@deriving sexp_of]

(* What the source's GC says of the copy's version: held, passed it already, or passed it meanwhile. *)
type holding = Held | Passed_before | Lapsed [@@deriving sexp_of]

type case = {
  place : place;
  tables : int;
  at : int;  (* The table the refusals, the bad definition and the mismatched sums fall on. *)
  changing : agreement;
  partitions : int;
  allocators : (string * int64) list;
  sums : sums;
  failing : failing;
  holding : holding;
  taken : taken;
}
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
    and* tables = int_range 1 3
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
    let* at = int_range 0 (tables - 1)
    and* taken =
      oneof_weighted ((8, return Free) :: (1, return Taken) :: (1, return Taken_otherwise_cased) :: (if tables > 1 then [ (1, return Repeated) ] else []))
    in
    return { place; tables; at; changing; partitions; allocators; sums; failing; holding; taken })

let body = " (\n  `a` int COMMENT 'it''s \\\\ /*T![placement] PLACEMENT POLICY=`decoy` */'\n) ENGINE=InnoDB"

let clauses policies =
  String.concat ""
    (List.map
       (fun (name, _) -> " /*T![placement] PLACEMENT POLICY=`" ^ Base.String.substr_replace_all name ~pattern:"`" ~with_:"``" ^ "` */")
       policies)

let name database table : Quiver_core.Schema.Name.t = { database = Support.ident database; table = Support.ident table }
let source i = name "d" (Printf.sprintf "src%d" i)
let target i = name "e" (Printf.sprintf "dst%d" i)
let service i = name "e" (Printf.sprintf "q_model%d" i)
let copied : Mirror.copied = { keys = 10; bytes = 100; checksum = 0x8000_0000_0000_0001L }
let refused = Error (`Region (Region.Malformed [ Some "refused" ]))

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

let numbered case = List.init case.tables Fun.id
let partner case = if case.at = 0 then 1 else case.at - 1
let repeated case i = if case.taken = Repeated && i = case.at then partner case else i

let run case =
  let executed = ref [] and keyed = ref [] and asked = ref [] and finished = ref [] and holding = ref None and named = ref 0
  and source_clock = ref 100L
  and target_clock = ref 5000L in
  let cell text = Some text in
  let there = match case.place with Here -> None | There there -> Some there in
  let index of_ table = List.find_opt (fun i -> of_ i = table) (numbered case) in
  let side ~here ~clock ~collation ~column ~id ~allocators ~tables ~policies : _ Mirror.side =
    {
      query =
        (fun query ->
          match query with
          | New_collations -> Ok [ [| cell collation |] ]
          | Binlog_status _ ->
              clock := Int64.add !clock 7L;
              Ok [ [| cell "tidb-binlog"; cell (Int64.to_string !clock) |] ]
          | Show_create (Table table) ->
              Option.fold ~none:refused
                ~some:(fun i ->
                  let named = Printf.sprintf "CREATE TABLE `src%d`" i in
                  let definition =
                    match there with
                    | Some { definition = Differ; _ } when i = case.at -> "CREATE VIEW `src` AS SELECT 1"
                    | Some { policies; _ } -> named ^ body ^ clauses policies
                    | None -> named ^ body
                  in
                  Ok [ [| cell (Printf.sprintf "src%d" i); cell definition |] ])
                (index source table)
          | Names database when database = Support.ident "e" ->
              let named = Printf.sprintf "dst%d" case.at in
              Ok
                (List.map
                   (fun name -> [| cell name |])
                   ("unrelated"
                   :: (match case.taken with Taken -> [ named ] | Taken_otherwise_cased -> [ String.uppercase_ascii named ] | Free | Repeated -> [])))
          | Names _ -> Ok []
          | Placement_policies ->
              Ok
                (List.filter_map
                   (fun (name, presence) ->
                     match presence with Present -> Some [| cell name |] | Present_otherwise_cased -> Some [| cell (String.uppercase_ascii name) |] | Absent -> None)
                   policies)
          | Checksum table when not (List.mem table tables) -> refused
          | Checksum table when case.failing = At_checksum && table = service case.at -> refused
          | Checksum table ->
              let sums = if table = service case.at then case.sums else Same in
              let keys = match sums with Fewer_keys -> copied.keys - 1 | Same | Other_crc -> copied.keys
              and crc = match sums with Other_crc -> 1L | Same | Fewer_keys -> copied.checksum in
              Ok [ [| cell "e"; cell "q_model"; cell (Printf.sprintf "%Lu" crc); cell (Int.to_string keys); cell (Int.to_string copied.bytes) |] ]
          | Next_row_ids _ -> Ok (List.map (fun (kind, next) -> [| cell "d"; cell "src"; cell "c"; cell (Int64.to_string next); cell kind |]) allocators)
          | _ -> refused);
      execute =
        (fun statement ->
          executed := here statement :: !executed;
          let failing = service case.at in
          match (statement, case.failing) with
          | (Create_like { table; _ } | Create_table { table; _ }), At_create when table = failing -> refused
          | Rebase { table; _ }, At_rebase when table = failing -> refused
          | Rename_tables (first, rest), At_rename when List.exists (fun (renaming : Quiver_mysql.Statement.renaming) -> renaming.from = failing) (first :: rest) -> refused
          | _ -> Ok ());
      server = Result.get_ok (Quiver_mysql.Server.of_version_string "8.0.11-TiDB-v8.5.8");
      described =
        (fun table ~at ->
          asked := (table, at, !holding) :: !asked;
          let state : Info.state = match case.changing with Agree -> Public | Differ -> Changing in
          if case.failing = At_described && table = source case.at then refused else Ok (described ~id ~column ~state ~partitions:case.partitions));
    }
  in
  let sources = List.map source (numbered case) and services = List.map service (numbered case) in
  let source_side =
    side ~here:(fun statement -> Source_ran statement) ~clock:source_clock ~collation:"True" ~column:1 ~id:75L ~allocators:case.allocators
      ~tables:(match there with None -> sources @ services | Some _ -> sources) ~policies:[]
  in
  let target_side, cluster =
    match there with
    | None -> (source_side, Mirror.Same_cluster)
    | Some there ->
        let collation = match there.collations with Agree -> "TRUE" | Differ -> "False"
        and column = match there.ids with Agree -> 1 | Differ -> 2 in
        ( side ~here:(fun statement -> Target_ran statement) ~clock:target_clock ~collation ~column ~id:300L ~allocators:[] ~tables:services
            ~policies:there.policies,
          Other_cluster () )
  in
  let session : _ Mirror.session =
    {
      source_side;
      target_side;
      cluster;
      placement = (match there with Some there -> there.placement | None -> Kept_placement);
      name =
        (fun () ->
          let name = Printf.sprintf "q_model%d" !named in
          incr named;
          name);
      keys =
        (fun plan ~version ~commit_ts ->
          let count = List.length !keyed in
          keyed := (Renumber.tables plan, version, commit_ts, !holding) :: !keyed;
          if case.failing = At_keys && count = case.at then refused else Ok copied);
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
      finished = (fun moved copied -> finished := (moved, copied) :: !finished);
    }
  in
  let moved = List.map (fun i : Mirror.moved -> { source = source i; target = target (repeated case i) }) (numbered case) in
  let result = Mirror.through session moved in
  (result, List.rev !executed, List.rev !keyed, List.rev !asked, List.rev !finished)

let expected case =
  let tables = numbered case in
  let ran statement = match case.place with Here -> Source_ran statement | There _ -> Target_ran statement in
  let moved i : Mirror.moved = { source = source i; target = target i } in
  let create i : Quiver_mysql.Statement.t =
    match case.place with
    | Here -> Create_like { table = service i; like = source i }
    | There { placement = Kept_placement; policies; _ } -> Create_table { table = service i; body = body ^ clauses policies }
    | There { placement = Default_placement; _ } -> Create_table { table = service i; body }
  in
  let lacking =
    match case.place with
    | There { placement = Kept_placement; policies; _ } ->
        let present name = List.exists (fun (other, presence) -> other = name && presence <> Absent) policies in
        List.fold_left (fun lacking (name, _) -> if present name || List.mem name lacking then lacking else lacking @ [ name ]) [] policies
    | There { placement = Default_placement; _ } | Here -> []
  in
  let most kinds =
    List.fold_left (fun most (kind, next) -> if List.mem kind kinds then Some (Option.fold ~none:next ~some:(Int64.max next) most) else most) None case.allocators
  in
  let rebases i =
    List.filter_map Fun.id
      [
        Option.map (fun next -> Quiver_mysql.Statement.Rebase { table = service i; allocator = Auto_increment; next }) (most [ "_TIDB_ROWID"; "AUTO_INCREMENT" ]);
        Option.map (fun next -> Quiver_mysql.Statement.Rebase { table = service i; allocator = Auto_random; next }) (most [ "AUTO_RANDOM" ]);
      ]
  in
  let drops upto = List.map (fun i -> ran (Drop_table (service i))) (List.filter (fun i -> i < upto) tables) in
  let passed = Error (`Pd (Pd.Gc_passed { version = 0L; least = 0L })) in
  let at i failing = case.failing = failing && i = case.at in
  match case.place with
  | There { collations = Differ; _ } -> (Error (`Mirror (Mirror.Collations_differ { source = "True"; target = "False" })), [], 0)
  | There { definition = Differ; _ } -> (Error (`Mirror (Mirror.Definition (source case.at))), [], 0)
  | There _ when lacking <> [] -> (Error (`Mirror (Mirror.Unknown_policies (List.hd lacking, List.tl lacking))), [], 0)
  | (Here | There _) when case.taken <> Free -> (Error (`Mirror (Mirror.Taken (target (repeated case case.at), []))), [], 0)
  | Here | There _ -> (
      let made = ran (Create_database (Support.ident "e")) :: List.map (fun i -> ran (create i)) tables in
      match List.find_opt (fun i -> at i At_create) tables with
      | Some k -> (refused, List.filteri (fun j _ -> j <= k + 1) made @ drops k, 0)
      | None -> (
          let all = drops case.tables in
          let rec keys i =
            if i = case.tables then None
            else if at i At_described then Some (refused, i)
            else if case.changing = Differ then Some (Error (`Renumber (Renumber.Changing "a")), i)
            else if at i At_keys then Some (refused, i + 1)
            else keys (i + 1)
          in
          match case.holding with
          | Passed_before -> (passed, made @ all, 0)
          | Held | Lapsed -> (
              match (keys 0, case.holding) with
              | Some (error, count), _ -> (error, made @ all, count)
              | None, Lapsed -> (passed, made @ all, case.tables)
              | None, (Held | Passed_before) -> (
                  let rec checked i statements =
                    if i = case.tables then Ok (List.rev statements)
                    else if at i At_checksum then Error (refused, List.rev statements)
                    else if i = case.at && case.sums <> Same then
                      let target : Mirror.copied =
                        {
                          copied with
                          keys = (if case.sums = Fewer_keys then copied.keys - 1 else copied.keys);
                          checksum = (if case.sums = Other_crc then 1L else copied.checksum);
                        }
                      in
                      Error (Error (`Mirror (Mirror.Mismatch { table = source i; copied; target })), List.rev statements)
                    else if List.exists (fun (kind, _) -> kind = "WEIRD") case.allocators then
                      Error (Error (`Mirror (Mirror.Allocator "WEIRD")), List.rev statements)
                    else
                      match (at i At_rebase, rebases i) with
                      | true, first :: _ -> Error (refused, List.rev (ran first :: statements))
                      | _, rebased -> checked (i + 1) (List.rev_append (List.map ran rebased) statements)
                  in
                  let renamed =
                    let statement =
                      ran (Rename_tables ({ from = service 0; into = target 0 }, List.init (case.tables - 1) (fun i -> { Quiver_mysql.Statement.from = service (i + 1); into = target (i + 1) })))
                    in
                    if List.exists (fun i -> at i At_rename) tables then Error (refused, [ statement ]) else Ok [ statement ]
                  in
                  match checked 0 [] with
                  | Error (error, statements) -> (error, made @ statements @ all, case.tables)
                  | Ok rebased -> (
                      match renamed with
                      | Error (error, statements) -> (error, made @ rebased @ statements @ all, case.tables)
                      | Ok renames ->
                          (Ok { Mirror.version = 0L; copies = List.map (fun i -> (moved i, copied)) tables }, made @ rebased @ renames, case.tables))))))

let show statements =
  String.concat "; "
    (List.map
       (function
         | Source_ran statement -> "source: " ^ Quiver_mysql.Statement.sql statement
         | Target_ran statement -> "target: " ^ Quiver_mysql.Statement.sql statement)
       statements)

let copies =
  QCheck2.Test.make ~count:3000
    ~name:"tables are copied at one version of the source, made only once every check passed, checked, moved past the source's ids and renamed, or the half-made ones dropped"
    ~print:(fun case -> Base.Sexp.to_string_hum (sexp_of_case case))
    gen
    (fun case ->
      let result, executed, keyed, asked, finished = run case in
      let wanted, statements, count = expected case in
      let version = match asked with (_, version, _) :: _ -> Some version | [] -> None in
      let same =
        match (result, wanted) with
        | Ok got, Ok wanted -> Some got.version = version && got.copies = wanted.copies
        | Error (`Pd (Pd.Gc_passed _)), Error (`Pd (Pd.Gc_passed _)) -> true
        | got, wanted -> got = wanted
      in
      let into = match case.place with Here -> 75L | There _ -> 300L in
      let physical =
        match case.partitions with
        | 0 -> [ (75L, into) ]
        | n -> List.init n (fun i -> (Int64.of_int (76 + i), Int64.add into (Int64.of_int (i + 1))))
      in
      let late commit_ts =
        match case.place with
        | Here -> Option.fold ~none:false ~some:(fun version -> Int64.compare version commit_ts < 0) version
        | There _ -> Int64.compare commit_ts 5000L > 0 && Option.fold ~none:false ~some:(fun version -> Int64.compare version 1000L < 0) version
      in
      let keys_held =
        List.length keyed = count
        && List.for_all (fun (tables, at, commit_ts, held) -> tables = physical && Some at = version && held = version && late commit_ts) keyed
      in
      let rec described_at i asked keyed =
        match (asked, keyed) with
        | (first, at, held) :: (second, later, again) :: asked, (_, _, commit_ts, _) :: keyed ->
            first = source i && Some at = version && held = version && second = service i && later = commit_ts && again = version
            && described_at (i + 1) asked keyed
        | [ (first, at, held) ], [] -> case.failing = At_described && i = case.at && first = source i && Some at = version && held = version
        | [ (first, at, held); (second, later, again) ], [] ->
            case.changing = Differ && first = source i && second = service i && Some at = version && held = version && again = version && late later
        | [], [] -> true
        | _ -> false
      in
      let told =
        finished = List.filteri (fun i _ -> i < List.length keyed && not (case.failing = At_keys && i = case.at)) (List.map (fun i : (Mirror.moved * Mirror.copied) -> ({ source = source i; target = target i }, copied)) (numbered case))
      in
      if same && show executed = show statements && keys_held && described_at 0 asked keyed && told then true
      else
        QCheck2.Test.fail_reportf "got %s@.wanted %s@.ran %s@.should %s@.keyed %d, asked %d, told %d"
          (match result with Ok _ -> "ok" | Error _ -> "error")
          (match wanted with Ok _ -> "ok" | Error _ -> "error")
          (show executed) (show statements) (List.length keyed) (List.length asked) (List.length finished))

let tests = [ QCheck_alcotest.to_alcotest copies ]
