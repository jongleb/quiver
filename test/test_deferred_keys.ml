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
