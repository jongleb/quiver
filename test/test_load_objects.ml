open Sexplib0.Sexp_conv

let ident = Support.ident

(** An object of the dumped database, its text as a server shows it. *)
type made =
  | View of string
  | Trigger of string
  | Procedure of string
  | Function of string
  | Event of string
[@@deriving sexp_of]

type case = { made : made list; kinds : Quiver_core.Filter.kind list } [@@deriving sexp_of]

let gen =
  QCheck2.Gen.(
    let name = oneof_list [ "a"; "event"; "TABLE"; "p`q"; "trigger x"; "view" ] in
    let quoted name = "`" ^ Base.String.substr_replace_all name ~pattern:"`" ~with_:"``" ^ "`" in
    let definer = oneof_list [ ""; "DEFINER=`root`@`localhost` "; "DEFINER=`event`@`%` "; "DEFINER=`proc`@`view` " ] in
    let body =
      oneof_list
        [
          "SELECT 1";
          "BEGIN\n  DROP EVENT IF EXISTS `e2`;\n  CREATE TEMPORARY TABLE `tmp` (a int);\n  SELECT 'TRIGGER `t`';\nEND";
          "BEGIN\n  CREATE VIEW `w` AS SELECT 1;\n  DROP PROCEDURE IF EXISTS `p2`;\nEND";
          "CALL `p`()";
          "INSERT INTO `log` VALUES ('FUNCTION `f`')";
        ]
    in
    let made =
      let* name and* definer and* body in
      oneof_list
        [
          View
            ("CREATE ALGORITHM=UNDEFINED " ^ definer ^ "SQL SECURITY DEFINER VIEW " ^ quoted name
           ^ " AS select 1 AS `a`");
          Trigger ("CREATE " ^ definer ^ "TRIGGER " ^ quoted name ^ " BEFORE INSERT ON `t` FOR EACH ROW " ^ body);
          Procedure ("CREATE " ^ definer ^ "PROCEDURE " ^ quoted name ^ "(IN a INT)\n" ^ body);
          Function ("CREATE " ^ definer ^ "FUNCTION " ^ quoted name ^ "(a int) RETURNS int\n    DETERMINISTIC\nRETURN a + 1");
          Event
            ("CREATE " ^ definer ^ "EVENT " ^ quoted name
           ^ " ON SCHEDULE EVERY 1 DAY STARTS '2026-01-01 00:00:00' ON COMPLETION NOT PRESERVE ENABLE DO "
           ^ body);
        ]
    in
    let+ made = list_size (int_range 0 8) made
    and+ kinds = list_size (int_range 0 4) (oneof_list Quiver_core.Filter.[ Views; Triggers; Routines; Events ]) in
    { made; kinds = List.sort_uniq compare kinds })

let definitions made =
  List.mapi
    (fun i made ->
      let kind, text, time_zone =
        match made with
        | View text -> (Quiver_core.Definition.View { columns = (ident "a", []) }, text, None)
        | Trigger text -> (Trigger { table = ident "t" }, text, None)
        | Procedure text -> (Procedure, text, None)
        | Function text -> (Function, text, None)
        | Event text -> (Event, text, Some "+00:00")
      in
      {
        Quiver_core.Definition.database = ident "d";
        name = ident (Printf.sprintf "o%d" i);
        kind;
        context =
          {
            charset = ident "utf8mb4";
            collation = ident "utf8mb4_0900_ai_ci";
            sql_mode = Some "STRICT_TRANS_TABLES";
            time_zone;
          };
        text;
      })
    made

let read text =
  let pos = ref 0 in
  fun bytes ~pos:at ~len ->
    let length = min len (String.length text - !pos) in
    Bytes.blit_string text !pos bytes at length;
    pos := !pos + length;
    Ok length

let ran objects (script : Quiver_sink.Layout.script) =
  let statements = ref [] in
  let text = String.concat "" (List.map (fun statement -> statement ^ ";\n") script.statements) in
  match
    Quiver_load.Statements.iter (read text) (fun statement ->
        Ok (Option.iter (fun statement -> statements := statement :: !statements) (Quiver_load.Load.taken_statement objects statement)))
  with
  | Ok () -> List.rev !statements
  | Error (`Statements error) -> Alcotest.failf "%a" Sexplib0.Sexp.pp_hum (Quiver_load.Statements.sexp_of_error error)

let filtered =
  QCheck2.Test.make ~count:2000
    ~name:
      "a load taking some kinds of objects runs from the triggers and routines files every \
       statement a dump of those kinds holds and no other one but a SET"
    ~print:(fun case -> Sexplib0.Sexp.to_string_hum (sexp_of_case case))
    gen
    (fun { made; kinds } ->
      let objects : Quiver_core.Filter.objects =
        match kinds with [] -> Without_objects | first :: rest -> With_objects (first, rest)
      in
      let scripts definitions = Result.get_ok (Quiver_sink.Layout.scripts (ident "d") definitions) in
      let all = scripts (definitions made) in
      let dumped =
        scripts
          (List.filter
             (fun ({ kind; _ } : Quiver_core.Definition.t) ->
               let kind : Quiver_core.Filter.kind =
                 match kind with View _ -> Views | Trigger _ -> Triggers | Procedure | Function -> Routines | Event -> Events
               in
               List.mem kind kinds)
             (definitions made))
      in
      let set statement = Base.String.is_prefix statement ~prefix:"SET " in
      let file scripts name =
        Option.value ~default:[]
          (Option.map (fun (script : Quiver_sink.Layout.script) -> script.statements)
             (List.find_opt (fun ({ name = other; _ } : Quiver_sink.Layout.script) -> String.equal other name) scripts))
      in
      List.for_all
        (fun name ->
          let script = { Quiver_sink.Layout.name; statements = file all name } in
          let run = ran objects script in
          List.equal String.equal (List.filter (Fun.negate set) run) (List.filter (Fun.negate set) (file dumped name))
          && List.equal String.equal (List.filter set run) (List.filter set script.statements))
        [ "d-schema-triggers"; "d-schema-post" ])

let chosen =
  QCheck2.Test.make ~count:2000
    ~name:
      "a load taking some kinds of objects runs every view's stand-in on its own, then the views', \
       triggers' and routines' files that are there, each only when a kind of it is taken"
    ~print:(fun (case, extension) -> Sexplib0.Sexp.to_string_hum (sexp_of_case case) ^ " " ^ extension)
    QCheck2.Gen.(pair gen (oneof_list [ ".sql"; ".sql.zst" ]))
    (fun ({ made; kinds }, extension) ->
      let objects : Quiver_core.Filter.objects =
        match kinds with [] -> Without_objects | first :: rest -> With_objects (first, rest)
      in
      let names database made =
        List.map
          (fun ({ name; _ } : Quiver_sink.Layout.script) -> name ^ extension)
          (Result.get_ok (Quiver_sink.Layout.scripts (ident database) (definitions made)))
      in
      let entries =
        Base.Set.of_list
          (module Base.String)
          (names "d" made
          @ names "e" [ View "CREATE VIEW `v` AS select 1 AS `a`"; Trigger "CREATE TRIGGER `t` BEFORE INSERT ON `t` FOR EACH ROW SET @a = 1" ]
          @ [ "d-schema-create.sql"; "d.t-schema.sql"; "d.t.00000.sql"; "d.t-schema-view.sql.gz"; "quiver.json"; "metadata" ])
      in
      let taken kind = List.mem kind kinds in
      let views =
        List.sort String.compare
          (Base.List.filter_mapi made ~f:(fun i -> function
             | View _ -> Some (Printf.sprintf "d.o%d" i)
             | Trigger _ | Procedure _ | Function _ | Event _ -> None))
      in
      let file base = File_path.Part.of_string (base ^ extension) in
      let triggers = List.exists (function Trigger _ -> true | View _ | Procedure _ | Function _ | Event _ -> false) made
      and post = List.exists (function Procedure _ | Function _ | Event _ -> true | View _ | Trigger _ -> false) made in
      let expected : Quiver_load.Load.object_files =
        {
          stubs = List.filter (fun _ -> taken Views) (List.map (fun view -> file (view ^ "-schema")) views);
          runs =
            List.filter
              (Fun.negate Base.List.is_empty)
              [
                List.filter (fun _ -> taken Views) (List.map (fun view -> file (view ^ "-schema-view")) views);
                List.filter (fun _ -> taken Triggers && triggers) [ file "d-schema-triggers" ];
                List.filter (fun _ -> (taken Routines || taken Events) && post) [ file "d-schema-post" ];
              ];
        }
      in
      let shown (files : Quiver_load.Load.object_files) =
        [%sexp_of: string list * string list list]
          ( List.map File_path.Part.to_string files.stubs,
            List.map (List.map File_path.Part.to_string) files.runs )
      in
      match Quiver_load.Load.object_files objects ~entries (ident "d") with
      | Ok files when Sexplib0.Sexp.equal (shown files) (shown expected) -> true
      | Ok files ->
          QCheck2.Test.fail_reportf "chose %a, not %a" Sexplib0.Sexp.pp_hum (shown files) Sexplib0.Sexp.pp_hum (shown expected)
      | Error (`Load error) -> QCheck2.Test.fail_reportf "%a" Sexplib0.Sexp.pp_hum (Quiver_load.Load.sexp_of_error error))

let tests =
  [
    QCheck_alcotest.to_alcotest filtered;
    QCheck_alcotest.to_alcotest chosen;
    Alcotest.test_case "what a view's files make and drop" `Quick (fun () ->
        Alcotest.(check (list (option string)))
          "kinds"
          [ None; None; Some "views"; Some "views"; None; None ]
          (List.map
             (fun statement ->
               Option.map
                 (function
                   | Quiver_core.Filter.Views -> "views"
                   | Triggers -> "triggers"
                   | Routines -> "routines"
                   | Events -> "events")
                 (Quiver_load.Load.object_kind statement))
             [
               "CREATE TABLE IF NOT EXISTS `v`(\n`a` int\n) ENGINE=MEMORY";
               "DROP TABLE IF EXISTS `v`";
               "DROP VIEW IF EXISTS `v`";
               "CREATE ALGORITHM=UNDEFINED DEFINER=`root`@`%` SQL SECURITY DEFINER VIEW `v` AS select 1 AS `a`";
               "SET character_set_client = utf8mb4";
               "SET SQL_MODE='NO_AUTO_VALUE_ON_ZERO'";
             ]));
  ]
