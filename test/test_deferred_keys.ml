let table = Result.get_ok (Quiver_core.Ident.of_string "t")

let definition =
  QCheck2.Gen.(
    let* column = int_range 0 4 in
    let c = Printf.sprintf "`c%d`" column in
    let* n = int_range 0 99 in
    oneof_list
      [
        Printf.sprintf "  KEY `k%d` (%s)" n c;
        Printf.sprintf "  UNIQUE KEY `u%d` (%s)" n c;
        Printf.sprintf "  FULLTEXT KEY `f%d` (%s)" n c;
        Printf.sprintf
          "  CONSTRAINT `fk%d` FOREIGN KEY (%s) REFERENCES `p` (`id`)" n c;
        Printf.sprintf "  CONSTRAINT `chk%d` CHECK ((%s > 0))" n c;
      ])

let statement =
  QCheck2.Gen.(
    let* auto = option (int_range 0 4) in
    let* primary = option (int_range 0 4) in
    let+ keys = list_size (int_range 0 8) definition in
    let columns =
      List.init 5 (fun i ->
          Printf.sprintf "  `c%d` int NOT NULL%s" i
            (match auto with Some a when a = i -> " AUTO_INCREMENT" | _ -> ""))
    and primary =
      Option.to_list
        (Option.map (Printf.sprintf "  PRIMARY KEY (`c%d`)") primary)
    in
    ( auto,
      primary,
      columns @ primary @ Base.List.stable_dedup keys ~compare:String.compare ))

let lines create =
  match String.split_on_char '\n' create with
  | _ :: rest ->
      List.map
        (fun line ->
          String.trim (Base.String.rstrip ~drop:(Char.equal ',') line))
        (List.filter
           (fun line -> not (Base.String.is_prefix line ~prefix:")"))
           rest)
  | [] -> []

let added later =
  List.concat_map
    (fun alter ->
      let body =
        Option.get
          (Base.String.chop_prefix alter ~prefix:"ALTER TABLE `t` ADD ")
      in
      Base.String.Search_pattern.(split_on (create ", ADD ") body))
    later

let deferred line =
  List.exists
    (fun prefix -> Base.String.is_prefix line ~prefix)
    [ "KEY "; "UNIQUE KEY "; "FULLTEXT KEY " ]
  || Base.String.is_substring line ~substring:" FOREIGN KEY "

let tests =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:2000
         ~name:"deferred keys: nothing lost or doubled, only keys deferred"
         ~print:(fun (_, _, definitions) -> String.concat "\n" definitions)
         statement
         (fun (auto, primary, definitions) ->
           let text =
             "CREATE TABLE `t` (\n"
             ^ String.concat ",\n" definitions
             ^ "\n) ENGINE=InnoDB"
           in
           let { Quiver_load.Deferred_keys.create; later; foreign; _ } =
             Quiver_load.Deferred_keys.split All_keys table text
           and {
             Quiver_load.Deferred_keys.create = with_keys;
             later = none_later;
             foreign = only_foreign;
             _;
           } =
             Quiver_load.Deferred_keys.split Foreign_keys table text
           in
           let kept = lines create and back = added (later @ foreign) in
           let sorted list = List.sort String.compare list in
           let auto_key =
             match (auto, primary) with
             | Some a, [ p ] when p = Printf.sprintf "  PRIMARY KEY (`c%d`)" a
               ->
                 None
             | Some a, _ ->
                 List.find_opt
                   (fun line ->
                     (Base.String.is_prefix line ~prefix:"KEY "
                     || Base.String.is_prefix line ~prefix:"UNIQUE KEY ")
                     && Base.String.is_substring line
                          ~substring:(Printf.sprintf "(`c%d`)" a))
                   (List.map String.trim definitions)
             | None, _ -> None
           in
           sorted (kept @ back) = sorted (List.map String.trim definitions)
           && List.for_all
                (fun line -> (not (deferred line)) || Some line = auto_key)
                kept
           && Option.fold ~none:true
                ~some:(fun key -> List.mem key kept)
                auto_key
           && none_later = [] && only_foreign = foreign
           && List.for_all
                (fun line -> Base.String.is_substring line ~substring:" FOREIGN KEY ")
                (added foreign)
           && sorted (lines with_keys @ added only_foreign)
              = sorted (List.map String.trim definitions)
           && List.for_all
                (fun line -> not (Base.String.is_substring line ~substring:" FOREIGN KEY "))
                (lines with_keys)));
    Alcotest.test_case "text that is not a CREATE TABLE stays whole" `Quick
      (fun () ->
        List.iter
          (fun text ->
            let { Quiver_load.Deferred_keys.create; later; foreign; _ } =
              Quiver_load.Deferred_keys.split All_keys table text
            in
            Alcotest.(check string) "create" text create;
            Alcotest.(check (list string)) "later" [] later;
            Alcotest.(check (list string)) "foreign" [] foreign)
          [
            "SET NAMES utf8mb4";
            "CREATE TABLE `t` (`a` int)";
            "CREATE TABLE `t` (\n  `a` int,\n  KEY `k` (`a`)";
          ]);
  ]

let tests =
  tests
  @ [
      QCheck_alcotest.to_alcotest
        (QCheck2.Test.make ~count:500
           ~name:"keys go back in their places, and plain keys after a spatial one wait with it"
           ~print:(fun (definitions, seed) -> Printf.sprintf "seed %d\n%s" seed (String.concat "\n" definitions))
           QCheck2.Gen.(
             let* _, _, lines = statement in
             let* spatial = option (pair (int_range 0 (List.length lines)) (int_range 0 99)) in
             let+ seed = int in
             let lines =
               Option.fold spatial ~none:lines ~some:(fun (at, n) ->
                   Base.List.take lines at @ [ Printf.sprintf "  SPATIAL KEY `s%d` (`c0`)" n ] @ Base.List.drop lines at)
             in
             let named line = List.nth_opt (String.split_on_char '`' line) 1 in
             (Base.List.stable_dedup lines ~compare:(fun a b -> Option.compare String.compare (named a) (named b)), seed))
           (fun (definitions, seed) ->
             let text = "CREATE TABLE `t` (\n" ^ String.concat ",\n" definitions ^ "\n) ENGINE=InnoDB" in
             let t = Quiver_load.Deferred_keys.split All_keys table text in
             let random = Random.State.make [| seed |] in
             let chosen = List.filter (fun _ -> Random.State.bool random) t.keys in
             let names = List.map (fun (key : Quiver_load.Deferred_keys.key) -> key.definition) chosen in
             let kept = lines t.create in
             let original = List.map String.trim definitions in
             let expected = List.filter (fun line -> List.mem line kept || List.mem line names) original in
             let keys = List.map (fun (key : Quiver_load.Deferred_keys.key) -> key.definition) t.keys in
             let spatial =
               List.find_index
                 (fun line -> Base.String.is_prefix line ~prefix:"SPATIAL KEY " && not (List.mem line kept))
                 original
             in
             let position line = Option.get (List.find_index (String.equal line) original) in
             let deferred_keys =
               List.filter
                 (fun line ->
                   (not (List.mem line kept))
                   && List.exists (fun prefix -> Base.String.is_prefix line ~prefix) [ "KEY "; "UNIQUE KEY "; "SPATIAL KEY "; "FULLTEXT KEY " ])
                 original
             in
             let sorted = List.sort compare in
             lines (Quiver_load.Deferred_keys.with_keys t chosen) = expected
             && sorted (keys @ added t.special) = sorted deferred_keys
             && List.for_all
                  (fun (key : Quiver_load.Deferred_keys.key) ->
                    match (key.group, spatial) with
                    | Unique, _ | Plain, None -> true
                    | Plain, Some first -> position key.definition < first)
                  t.keys));
    ]

(** The index InnoDB clusters a table by, as MySQL picks it: the primary key, else the first
    [UNIQUE] key over [NOT NULL] columns only, else a hidden row id. *)
let clustered create =
  let lines = lines create in
  let not_null column =
    List.exists
      (fun line ->
        Base.String.is_prefix line ~prefix:("`" ^ column ^ "` ") && Base.String.is_substring line ~substring:"NOT NULL")
      lines
  in
  let columns line =
    match String.split_on_char '(' line with
    | _ :: rest ->
        List.filter_map
          (fun part -> List.nth_opt (String.split_on_char '`' part) 1)
          (String.split_on_char ',' (String.concat "(" rest))
    | [] -> []
  in
  if List.exists (fun line -> Base.String.is_prefix line ~prefix:"PRIMARY KEY ") lines then "PRIMARY"
  else
    Option.value ~default:"GEN_CLUST_INDEX"
      (List.find_map
         (fun line ->
           if Base.String.is_prefix line ~prefix:"UNIQUE KEY " && List.for_all not_null (columns line) then
             List.nth_opt (String.split_on_char '`' line) 1
           else None)
         lines)

let keyed_table =
  QCheck2.Gen.(
    let* nullable = list_size (return 5) bool in
    let* auto = option (int_range 0 4) in
    let* primary = option (int_range 0 4) in
    let+ keys =
      list_size (int_range 0 6)
        (pair (oneof_list [ "KEY"; "UNIQUE KEY" ]) (list_size (int_range 1 2) (int_range 0 4)))
    in
    let not_null i = Option.equal Int.equal (Some i) primary || Option.equal Int.equal (Some i) auto || not (List.nth nullable i) in
    let columns =
      List.init 5 (fun i ->
          Printf.sprintf "  `c%d` int %s%s" i
            (if not_null i then "NOT NULL" else "DEFAULT NULL")
            (match auto with Some a when a = i -> " AUTO_INCREMENT" | _ -> ""))
    in
    let key j (kind, cols) =
      let cols = List.sort_uniq compare cols in
      let name = (if kind = "KEY" then "k" else "u") ^ string_of_int j in
      ( kind,
        Printf.sprintf "  %s `%s` (%s)" kind name (String.concat "," (List.map (Printf.sprintf "`c%d`") cols)) )
    in
    let keys = List.mapi key keys in
    let unique = List.filter_map (fun (kind, line) -> if kind = "UNIQUE KEY" then Some line else None) keys
    and plain = List.filter_map (fun (kind, line) -> if kind = "KEY" then Some line else None) keys in
    columns @ Option.to_list (Option.map (Printf.sprintf "  PRIMARY KEY (`c%d`)") primary) @ unique @ plain)

let tests =
  tests
  @ [
      QCheck_alcotest.to_alcotest
        (QCheck2.Test.make ~count:1000 ~name:"keys put back never change which index clusters the table, and only unique keys stay out"
           ~print:(String.concat "\n")
           keyed_table
           (fun definitions ->
             let text = "CREATE TABLE `t` (\n" ^ String.concat ",\n" definitions ^ "\n) ENGINE=InnoDB" in
             let t = Quiver_load.Deferred_keys.split All_keys table text in
             let staying = lines t.create in
             let named prefix =
               List.filter_map
                 (fun line ->
                   if Base.String.is_prefix line ~prefix && not (List.mem line staying) then
                     List.nth_opt (String.split_on_char '`' line) 1
                   else None)
                 (List.map String.trim definitions)
             in
             let kept group =
               List.filter_map
                 (fun (key : Quiver_load.Deferred_keys.key) -> if key.group = group then Some key.name else None)
                 t.keys
             in
             let unique = named "UNIQUE KEY " in
             clustered (Quiver_load.Deferred_keys.with_keys t t.keys) = clustered t.create
             && kept Plain = named "KEY "
             && Base.List.take unique (List.length (kept Unique)) = kept Unique));
    ]

let tests =
  tests
  @ [
      Alcotest.test_case "a statement deadlocked fewer than 8 times in a row goes through, other errors at once" `Quick
        (fun () ->
          let refused code = `Link (Quiver_mysql.Link.Refused { code; sqlstate = "40001"; message = "" }) in
          List.iter
            (fun (failures, code, expected, runs) ->
              let count = ref 0 and started = Unix.gettimeofday () in
              let outcome =
                Quiver_load.Load.retried ~cause:Option.some (fun () ->
                    incr count;
                    if !count <= failures then Error (refused code) else Ok ())
              in
              let shown = match outcome with Ok () -> "ok" | Error _ -> "error" in
              if runs = 8 && Unix.gettimeofday () -. started < 0.05 then Alcotest.failf "%d failures without pauses" failures;
              Alcotest.(check (pair string int))
                (Printf.sprintf "%d failures with %d" failures code)
                (expected, runs) (shown, !count))
            [
              (0, 1213, "ok", 1);
              (3, 1213, "ok", 4);
              (7, 1205, "ok", 8);
              (8, 1213, "error", 8);
              (20, 1205, "error", 8);
              (1, 1062, "error", 1);
            ]);
    ]
