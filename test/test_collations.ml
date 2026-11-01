open Sexplib0.Sexp_conv
open Quiver_load

let from = "utf8mb4_uca1400_ai_ci"
and into = "utf8mb4_0900_ai_ci"

let mapped = Result.get_ok (Collations.of_rules [ "UTF8MB4_uca1400_ai_ci=utf8mb4_0900_ai_ci" ])

(** How the server reads a backslash inside quotes, for the text generated. *)
type mode = Escaping | Raw [@@deriving sexp_of]

(** A piece of text: a code word may be rewritten, anything else stays as it is. *)
type token =
  | Word of string
  | Quoted of { quote : char; content : string; escaped : bool }
  | Comment of string  (** [/* ... */] *)
  | Line of { hash : bool; text : string }  (** [# ...] or [-- ...] up to the end of the line. *)
  | Versioned of string list  (** [/*!50100 ... */] around code words. *)
[@@deriving sexp_of]

let render mode tokens =
  let quoted quote content escaped =
    let body =
      String.concat ""
        (List.map
           (fun c ->
             match (c, mode, quote) with
             | c, _, _ when c = quote -> if escaped && quote <> '`' && mode = Escaping then Printf.sprintf "\\%c" c else Printf.sprintf "%c%c" c c
             | '\\', Escaping, ('\'' | '"') -> "\\\\"
             | c, _, _ -> String.make 1 c)
           (List.of_seq (String.to_seq content)))
    in
    Printf.sprintf "%c%s%c" quote body quote
  in
  List.map
    (function
      | Word word -> word
      | Quoted { quote; content; escaped } -> quoted quote content escaped
      | Comment text -> "/*" ^ text ^ "*/"
      | Line { hash; text } -> (if hash then "#" else "-- ") ^ text ^ "\n"
      | Versioned words -> "/*!50100 " ^ String.concat " " words ^ " */")
    tokens

let gen =
  QCheck2.Gen.(
    let name = oneof_list [ from; String.uppercase_ascii from; "Utf8mb4_UCA1400_ai_ci"; "utf8mb4_bin"; "COLLATE"; "x1" ] in
    let content =
      map (String.concat "")
        (list_size (int_range 0 6) (oneof_list [ "a"; " "; "'"; "\""; "`"; "\\"; from; "*/"; "\n" ]))
    in
    let plain = map (String.concat " ") (list_size (int_range 0 3) (oneof_list [ "a"; from; "'"; "\"" ])) in
    let token =
      oneof_weighted
        [
          (4, map (fun word -> Word word) name);
          ( 3,
            let+ quote = oneof_list [ '\''; '"'; '`' ] and+ content and+ escaped = bool in
            Quoted { quote; content; escaped } );
          (1, map (fun text -> Comment text) plain);
          (1, map2 (fun hash text -> Line { hash; text }) bool plain);
          (1, map (fun words -> Versioned words) (list_size (int_range 0 3) name));
        ]
    in
    let+ mode = oneof_list [ Escaping; Raw ] and+ tokens = list_size (int_range 0 12) token in
    (mode, tokens))

let rewritten =
  QCheck2.Test.make ~count:3000
    ~name:
      "a collation is renamed only in code, never inside quotes or a plain comment, and \
       everywhere in code when no backslash makes quotes ambiguous"
    ~print:(fun (mode, tokens) ->
      Sexplib0.Sexp.to_string_hum ([%sexp_of: mode * token list] (mode, tokens))
      ^ "\n" ^ String.concat " " (render mode tokens))
    gen
    (fun (mode, tokens) ->
      let pieces = render mode tokens in
      let text = String.concat " " pieces in
      let out = Collations.rewrite mapped text in
      let ambiguous =
        List.exists2
          (fun token piece ->
            match token with
            | Quoted { quote = '\'' | '"'; _ } -> String.contains piece '\\'
            | Quoted _ | Word _ | Comment _ | Line _ | Versioned _ -> false)
          tokens pieces
      in
      let renamed word = if String.equal (String.lowercase_ascii word) from then into else word in
      let rec walk pos tokens pieces =
        let after piece =
          let length = String.length piece in
          if pos + length <= String.length out && String.equal (String.sub out pos length) piece then
            Some (pos + length)
          else None
        in
        let gap pos = if pos < String.length out && out.[pos] = ' ' then Some (pos + 1) else Some pos in
        match (tokens, pieces) with
        | [], [] -> pos = String.length out
        | token :: tokens, piece :: pieces -> (
            let rec choices = function
              | [] -> [ [] ]
              | word :: rest ->
                  List.concat_map
                    (fun tail -> if ambiguous then [ word :: tail; renamed word :: tail ] else [ renamed word :: tail ])
                    (choices rest)
            in
            let options =
              match token with
              | Word word -> List.map (String.concat " ") (choices [ word ])
              | Versioned words ->
                  List.map (fun words -> "/*!50100 " ^ String.concat " " words ^ " */") (choices words)
              | Quoted _ | Comment _ | Line _ -> [ piece ]
            in
            List.exists
              (fun option ->
                Option.fold (after option) ~none:false ~some:(fun next ->
                    match pieces with
                    | [] -> walk next tokens pieces
                    | _ -> Option.fold (gap next) ~none:false ~some:(fun next -> walk next tokens pieces)))
              (List.sort_uniq String.compare options))
        | _ -> false
      in
      walk 0 tokens pieces)

let unknown =
  QCheck2.Test.make ~count:500
    ~name:"the collation a server refuses as unknown is named for the hint, no other refusal names one"
    ~print:(fun (code, name) -> Printf.sprintf "%d %S" code name)
    QCheck2.Gen.(pair (oneof_list [ 1273; 1064; 1146 ]) (string_size ~gen:(oneof_list [ 'a'; '_'; '4'; 'x' ]) (int_range 1 30)))
    (fun (code, name) ->
      let cause =
        `Link (Quiver_mysql.Link.Refused { code; sqlstate = "HY000"; message = "Unknown collation: '" ^ name ^ "'" })
      in
      let expected = if code = 1273 then Some name else None in
      Option.equal String.equal (Load.unknown_collation cause) expected)

let tests =
  [
    QCheck_alcotest.to_alcotest rewritten;
    QCheck_alcotest.to_alcotest unknown;
    Alcotest.test_case "rules: two names of one character set, each once" `Quick (fun () ->
        let refused rules =
          match Collations.of_rules rules with
          | Ok _ -> "taken"
          | Error (`Collations error) -> Sexplib0.Sexp.to_string (Collations.sexp_of_error error)
        in
        Alcotest.(check (list string))
          "refusals"
          [
            "(Malformed utf8mb4_bin)";
            "(Malformed a=b=c)";
            "(Malformed utf8mb4_bin=)";
            "(Other_charset(from latin1_swedish_ci)(into utf8mb4_bin))";
            "(Repeated utf8mb4_bin)";
            "taken";
          ]
          [
            refused [ "utf8mb4_bin" ];
            refused [ "a=b=c" ];
            refused [ "utf8mb4_bin=" ];
            refused [ "latin1_swedish_ci=utf8mb4_bin" ];
            refused [ "utf8mb4_bin=utf8mb4_general_ci"; "UTF8MB4_BIN=utf8mb4_0900_bin" ];
            refused [ "utf8mb4_bin=utf8mb4_general_ci"; "latin1_bin=latin1_general_ci" ];
          ]);
    Alcotest.test_case "a definition's text and the collation it was made under" `Quick (fun () ->
        let ident = Support.ident in
        let definition =
          Collations.definition mapped
            {
              Quiver_core.Definition.database = ident "d";
              name = ident "v";
              kind = View { columns = (ident "a", []) };
              context = { charset = ident "utf8mb4"; collation = ident from; sql_mode = None; time_zone = None };
              text = "CREATE VIEW `v` AS SELECT `a` COLLATE utf8mb4_uca1400_ai_ci AS `a`, 'utf8mb4_uca1400_ai_ci' FROM `t`";
            }
        in
        Alcotest.(check (pair string string))
          "rewritten"
          ( into,
            "CREATE VIEW `v` AS SELECT `a` COLLATE utf8mb4_0900_ai_ci AS `a`, 'utf8mb4_uca1400_ai_ci' FROM `t`" )
          ((definition.context.collation :> string), definition.text));
    Alcotest.test_case "a table as servers show it" `Quick (fun () ->
        Alcotest.(check string)
          "table"
          "CREATE TABLE `t` (\n  `a` varchar(10) COLLATE utf8mb4_0900_ai_ci DEFAULT 'utf8mb4_uca1400_ai_ci',\n  `b` text COMMENT 'uca1400 '' utf8mb4_uca1400_ai_ci'\n) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_0900_ai_ci /*!40100 COLLATE utf8mb4_0900_ai_ci */"
          (Collations.rewrite mapped
             "CREATE TABLE `t` (\n  `a` varchar(10) COLLATE utf8mb4_uca1400_ai_ci DEFAULT 'utf8mb4_uca1400_ai_ci',\n  `b` text COMMENT 'uca1400 '' utf8mb4_uca1400_ai_ci'\n) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4 COLLATE=utf8mb4_uca1400_ai_ci /*!40100 COLLATE utf8mb4_uca1400_ai_ci */"));
  ]
