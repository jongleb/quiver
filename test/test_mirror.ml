open Sexplib0.Sexp_conv
open Quiver_tikv

(* Where the model TiDB refuses. *)
type failing = Never | At_create | At_keys | At_checksum | At_rebase | At_rename [@@deriving sexp_of]

(* What TiDB's checksum of the copy says against what was copied. *)
type sums = Same | Fewer_keys | Other_crc [@@deriving sexp_of]

type case = { partitions : int; allocators : (string * int64) list; sums : sums; failing : failing } [@@deriving sexp_of]

let gen =
  QCheck2.Gen.(
    let* partitions = oneof_weighted [ (5, return 0); (1, int_range 1 4) ]
    and* allocators =
      list_size (int_range 0 4)
        (pair
           (oneof_weighted [ (3, return "_TIDB_ROWID"); (3, return "AUTO_INCREMENT"); (2, return "AUTO_RANDOM"); (1, return "SEQUENCE"); (1, return "WEIRD") ])
           (map Int64.of_int (int_range 1 1_000_000)))
    and* sums = oneof_weighted [ (4, return Same); (1, return Fewer_keys); (1, return Other_crc) ]
    and* failing = oneof_weighted [ (5, return Never); (1, return At_create); (1, return At_keys); (1, return At_checksum); (1, return At_rebase); (1, return At_rename) ] in
    return { partitions; allocators; sums; failing })

let name database table : Quiver_core.Schema.Name.t = { database = Support.ident database; table = Support.ident table }
let source = name "d" "src" and target = name "d" "dst" and service = name "d" "q_model"
let copied : Mirror.copied = { keys = 10; bytes = 100; checksum = 0x8000_0000_0000_0001L }

let run case =
  let executed = ref [] and keyed = ref [] and clock = ref 100L in
  let refused = Error (`Region (Region.Malformed [ Some "refused" ])) in
  let session : _ Mirror.session =
    {
      query =
        (fun query ->
          let cell text = Some text in
          match query with
          | Partitioned _ -> Ok [ [| cell (Int.to_string case.partitions) |] ]
          | Binlog_status _ ->
              clock := Int64.add !clock 7L;
              Ok [ [| cell "tidb-binlog"; cell (Int64.to_string !clock) |] ]
          | Checksum _ when case.failing = At_checksum -> refused
          | Checksum _ ->
              let keys = match case.sums with Fewer_keys -> copied.keys - 1 | Same | Other_crc -> copied.keys
              and crc = match case.sums with Other_crc -> 1L | Same | Fewer_keys -> copied.checksum in
              Ok [ [| cell "d"; cell "q_model"; cell (Printf.sprintf "%Lu" crc); cell (Int.to_string keys); cell (Int.to_string copied.bytes) |] ]
          | Next_row_ids _ -> Ok (List.map (fun (kind, next) -> [| cell "d"; cell "src"; cell "c"; cell (Int64.to_string next); cell kind |]) case.allocators)
          | _ -> refused);
      execute =
        (fun statement ->
          executed := statement :: !executed;
          match (statement, case.failing) with
          | Create_like _, At_create | Rebase _, At_rebase | Rename_table _, At_rename -> refused
          | _ -> Ok ());
      server = Result.get_ok (Quiver_mysql.Server.of_version_string "8.0.11-TiDB-v8.5.8");
      name = (fun () -> "q_model");
      keys =
        (fun table ~version ~commit_ts ->
          keyed := (table, version, commit_ts) :: !keyed;
          match case.failing with At_keys -> refused | Never | At_create | At_checksum | At_rebase | At_rename -> Ok copied);
    }
  in
  let result = Mirror.through session ~source ~target in
  (result, List.rev !executed, List.rev !keyed)

let show statements = String.concat "; " (List.map Quiver_mysql.Statement.sql statements)

let copies =
  QCheck2.Test.make ~count:3000
    ~name:"a copy is made, checked, moved past the source's ids and renamed, or the half-made table is dropped"
    ~print:(fun case -> Base.Sexp.to_string_hum (sexp_of_case case))
    gen
    (fun case ->
      let result, executed, keyed = run case in
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
      let create = Quiver_mysql.Statement.Create_like { table = service; like = source } in
      let failing = match (case.failing, rebases) with At_rebase, [] -> Never | failing, _ -> failing in
      let ordered = match keyed with [ (table, version, commit_ts) ] -> table = service && Int64.compare version commit_ts < 0 | _ -> false in
      match result with
      | _ when case.partitions > 0 -> result = Error (`Mirror (Mirror.Partitioned source)) && executed = [] && keyed = []
      | _ when failing = At_create -> Result.is_error result && executed = [ create ] && keyed = []
      | Ok got ->
          failing = Never && case.sums = Same && (not unknown) && got = copied && ordered
          && show executed = show ((create :: rebases) @ [ Rename_table { from = service; into = target } ])
      | Error error ->
          let expected_error =
            match (failing, case.sums) with
            | At_keys, _ | At_checksum, _ -> error = `Region (Region.Malformed [ Some "refused" ])
            | _, (Fewer_keys | Other_crc) ->
                let target : Mirror.copied =
                  { copied with keys = (if case.sums = Fewer_keys then copied.keys - 1 else copied.keys); checksum = (if case.sums = Other_crc then 1L else copied.checksum) }
                in
                error = `Mirror (Mirror.Mismatch { copied; target })
            | _ when unknown -> error = `Mirror (Mirror.Allocator "WEIRD")
            | (At_rebase | At_rename), Same -> error = `Region (Region.Malformed [ Some "refused" ])
            | (Never | At_create), Same -> false
          in
          expected_error && ordered
          && (match List.rev executed with Drop_table dropped :: _ -> dropped = service | _ -> false)
          && not (List.exists (function Quiver_mysql.Statement.Rename_table _ -> failing <> At_rename | _ -> false) executed))

let tests = [ QCheck_alcotest.to_alcotest copies ]
