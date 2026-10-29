open Sexplib0.Sexp_conv

let commands = [ "dump"; "copy" ]

let parsed text =
  match Quiver_quiverfile.Quiverfile.of_string ~commands ~scoped:[ "database" ] text with
  | Ok t -> t
  | Error (`Quiverfile error) ->
      Alcotest.fail (Sexplib0.Sexp.to_string (Quiver_quiverfile.Quiverfile.sexp_of_error error))

let refused text =
  match Quiver_quiverfile.Quiverfile.of_string ~commands ~scoped:[ "database" ] text with
  | Ok _ -> Alcotest.failf "accepted %S" text
  | Error (`Quiverfile error) -> error

let shown settings =
  List.map
    (fun (name, value) ->
      match (value : Quiver_quiverfile.Quiverfile.value) with
      | Text text -> name ^ "=" ^ text
      | Texts texts -> name ^ "=[" ^ String.concat "," texts ^ "]")
    settings

(** Options and sections as data: a section's options, then the common ones it does not set. *)
type file = { common : (string * int) list; sections : (string * (string * int) list) list } [@@deriving sexp_of]

let tests =
  [
    Alcotest.test_case "a command's own options win over the common ones" `Quick (fun () ->
        let t =
          parsed
            "# connection\nhost: db1\nthreads: 4\ntls: disabled\ncopy:\n  threads: 8\n  data-table: [a.b, c.d]\n  max-rate: 10.5\n  no-data: true\n  zero: null\ndump:\n"
        in
        Alcotest.(check (list string))
          "copy"
          [ "threads=8"; "data-table=[a.b,c.d]"; "max-rate=10.5"; "no-data=true"; "host=db1"; "tls=disabled" ]
          (shown (Quiver_quiverfile.Quiverfile.settings t ~command:"copy"));
        Alcotest.(check (list string))
          "dump" [ "host=db1"; "threads=4"; "tls=disabled" ]
          (shown (Quiver_quiverfile.Quiverfile.settings t ~command:"dump"));
        Alcotest.(check (list string)) "empty" [] (shown (Quiver_quiverfile.Quiverfile.settings (parsed "") ~command:"copy")));
    Alcotest.test_case "files that are not settings are refused" `Quick (fun () ->
        List.iter
          (fun (text, expected) ->
            let error = refused text in
            let kind =
              match error with
              | Unreadable _ -> "unreadable"
              | Not_settings _ -> "not settings"
              | Unusable _ -> "unusable"
              | Scoped _ -> "scoped"
            in
            Alcotest.(check string) text expected kind)
          [
            ("- a\n- b\n", "not settings");
            ("copy: [1, 2]\n", "not settings");
            ("host: {a: [[1]]}\n", "unusable");
            ("host: {[1]: 2}\n", "unusable");
            ("host: [[1]]\n", "unusable");
            ("database: shop\n", "scoped");
            ("host: a\nhost: b\n", "unreadable");
            ("tls: yes\n", "unreadable");
            ("a: 1\n---\nb: 2\n", "unreadable");
            ("host: [unclosed\n", "unreadable");
          ];
        Alcotest.(check (list string))
          "a map gives KEY=VALUE once per leaf, its keys joined by dots"
          [
            "mask=[shop.users.name=shape,shop.users.phone=expr:(phone value),*.*.email=email,shop.orders.note=null]";
            "after-load=[shop.orders=UPDATE {table} SET a = 1,shop.orders=DELETE FROM {table} WHERE b]";
          ]
          (shown
             (Quiver_quiverfile.Quiverfile.settings
                (parsed
                   "copy:\n\
                   \  mask:\n\
                   \    shop.users:\n\
                   \      name: shape\n\
                   \      phone: \"expr:(phone value)\"\n\
                   \    \"*.*.email\": email\n\
                   \    shop:\n\
                   \      orders.note: \"null\"\n\
                   \  after-load:\n\
                   \    shop.orders:\n\
                   \      - \"UPDATE {table} SET a = 1\"\n\
                   \      - \"DELETE FROM {table} WHERE b\"\n")
                ~command:"copy"));
        Alcotest.(check (list string))
          "scoped in a section" [ "database=shop" ]
          (shown (Quiver_quiverfile.Quiverfile.settings (parsed "copy:\n  database: shop\n") ~command:"copy")));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:300 ~name:"settings are a section's options, then the common ones it lacks"
         ~print:(fun file -> Sexplib0.Sexp.to_string (sexp_of_file file))
         QCheck2.Gen.(
           let option = pair (oneof_list [ "host"; "port"; "threads"; "batch-bytes"; "user" ]) (int_range 0 99) in
           let options = map (Base.List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)) (list_size (int_range 0 5) option) in
           let+ common = options
           and+ sections = map (Base.List.dedup_and_sort ~compare:(fun (a, _) (b, _) -> String.compare a b)) (list_size (int_range 0 2) (pair (oneof_list commands) options)) in
           { common; sections })
         (fun { common; sections } ->
           let lines prefix options = List.map (fun (name, n) -> Printf.sprintf "%s%s: %d" prefix name n) options in
           let text =
             String.concat "\n"
               (lines "" common @ List.concat_map (fun (command, options) -> (command ^ ":") :: lines "  " options) sections)
           in
           let t = parsed text in
           List.for_all
             (fun command ->
               let own = Option.value (List.assoc_opt command sections) ~default:[] in
               let expected = own @ List.filter (fun (name, _) -> not (List.mem_assoc name own)) common in
               shown (Quiver_quiverfile.Quiverfile.settings t ~command)
               = List.map (fun (name, n) -> Printf.sprintf "%s=%d" name n) expected)
             commands));
  ]
