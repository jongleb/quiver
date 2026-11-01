open Sexplib0.Sexp_conv
open Quiver_load

let ident = Support.ident

let renamed =
  Result.get_ok
    (Qualifier.make ~from:(ident "shop") ~into:(ident "shop2") ~names:[ ident "orders"; ident "ids"; ident "a`b" ])

(** A piece of text: only a reference to a name of the database through its own may change. *)
type token =
  | Reference of { own : bool; quoted : bool; spaced : bool; name : string }
  | Dangling of string  (** [shop.] and a literal before a name: no reference. *)
  | Literal of { quote : char; escaped : bool; content : string }
  | Comment of string
  | Versioned of token list  (** [/*!50100 ... */] around references. *)
  | Word of string
[@@deriving sexp_of]

let render_name quoted name =
  if quoted || String.contains name '`' then "`" ^ Base.String.substr_replace_all name ~pattern:"`" ~with_:"``" ^ "`"
  else name

let rec render escaping = function
  | Reference { own; quoted; spaced; name } ->
      let dot = if spaced then " .\n " else "." in
      render_name quoted (if own then "shop" else "other") ^ dot ^ render_name quoted name
  | Dangling name -> "shop.'x' " ^ name
  | Literal { quote; escaped; content } ->
      let body =
        String.concat ""
          (List.map
             (fun c ->
               if c = quote then if escaped && escaping then Printf.sprintf "\\%c" c else Printf.sprintf "%c%c" c c
               else if c = '\\' && escaping then "\\\\"
               else String.make 1 c)
             (List.of_seq (String.to_seq content)))
      in
      Printf.sprintf "%c%s%c" quote body quote
  | Comment text -> "/*" ^ text ^ "*/"
  | Versioned tokens -> "/*!50100 " ^ String.concat " " (List.map (render escaping) tokens) ^ " */"
  | Word word -> word

let gen =
  QCheck2.Gen.(
    let reference =
      let+ own = bool and+ quoted = bool and+ spaced = bool and+ name = oneof_list [ "orders"; "ids"; "a`b"; "elsewhere"; "shop" ] in
      Reference { own; quoted; spaced; name }
    in
    let content = map (String.concat "") (list_size (int_range 0 4) (oneof_list [ "`shop`.`orders`"; "shop.ids"; "'"; "\""; "\\"; " " ])) in
    let token =
      oneof_weighted
        [
          (4, reference);
          ( 2,
            let+ quote = oneof_list [ '\''; '"' ] and+ escaped = bool and+ content in
            Literal { quote; escaped; content } );
          (1, map (fun text -> Comment text) (oneof_list [ "`shop`.`orders`"; "shop.ids x" ]));
          (1, map (fun tokens -> Versioned tokens) (list_size (int_range 0 2) reference));
          (1, map (fun word -> Word word) (oneof_list [ "SELECT"; "FROM"; "shop"; "orders"; "," ]));
          (1, map (fun name -> Dangling name) (oneof_list [ "orders"; "`ids`" ]));
        ]
    in
    pair bool (list_size (int_range 0 10) token))

let rewritten =
  QCheck2.Test.make ~count:3000
    ~name:
      "a database's new name replaces it only where it qualifies one of its names in code, never \
       inside quotes, a plain comment or before another name"
    ~print:(fun (escaping, tokens) ->
      String.concat " " (List.map (render escaping) tokens)
      ^ "\n" ^ Sexplib0.Sexp.to_string_hum ([%sexp_of: bool * token list] (escaping, tokens)))
    gen
    (fun (escaping, tokens) ->
      let pieces = List.map (render escaping) tokens in
      let ambiguous = List.exists2 (fun token piece -> match token with Literal _ -> String.contains piece '\\' | _ -> false) tokens pieces in
      let out = Qualifier.rewrite renamed (String.concat " " pieces) in
      let rec options = function
        | Reference { own = true; quoted; name; _ } as reference when List.mem name [ "orders"; "ids"; "a`b" ] ->
            let original = render escaping reference in
            let changed = "`shop2`" ^ String.sub original (String.length (render_name quoted "shop")) (String.length original - String.length (render_name quoted "shop")) in
            if ambiguous then [ changed; original ] else [ changed ]
        | Versioned tokens ->
            let rec choices = function
              | [] -> [ [] ]
              | token :: rest -> List.concat_map (fun tail -> List.map (fun one -> one :: tail) (options token)) (choices rest)
            in
            List.map (fun pieces -> "/*!50100 " ^ String.concat " " pieces ^ " */") (choices tokens)
        | token -> [ render escaping token ]
      in
      let rec walk pos = function
        | [] -> pos = String.length out
        | token :: rest ->
            List.exists
              (fun option ->
                let length = String.length option in
                pos + length <= String.length out
                && String.equal (String.sub out pos length) option
                &&
                match rest with
                | [] -> walk (pos + length) rest
                | _ -> pos + length < String.length out && out.[pos + length] = ' ' && walk (pos + length + 1) rest)
              (options token)
      in
      walk 0 tokens)

let created =
  QCheck2.Test.make ~count:3000
    ~name:
      "the name a CREATE statement makes is found after its kind, whatever comes before it and \
       whatever its body names"
    ~print:(fun (statement, name) -> Printf.sprintf "%S -> %S" statement name)
    QCheck2.Gen.(
      let name =
        map (String.concat "")
          (list_size (int_range 1 4) (oneof_list [ "t"; "TABLE"; "view"; "`"; " "; "'"; "p1"; "\\" ]))
      in
      let quoted name = "`" ^ Base.String.substr_replace_all name ~pattern:"`" ~with_:"``" ^ "`" in
      let* name in
      let* kind = oneof_list [ "TABLE"; "table"; "VIEW"; "SEQUENCE"; "PROCEDURE"; "FUNCTION"; "EVENT"; "TRIGGER" ] in
      let* before =
        oneof_list
          [
            "";
            "DEFINER=`root`@`localhost` ";
            "ALGORITHM=UNDEFINED DEFINER=`a``b`@`%` SQL SECURITY DEFINER ";
            "/*!50017 DEFINER=`root`@`localhost`*/ ";
            "OR REPLACE ";
          ]
      and* guard = oneof_list [ ""; "IF NOT EXISTS " ]
      and* body =
        oneof_list
          [
            "";
            " (`id` int) ENGINE=InnoDB";
            "() BEGIN CREATE TEMPORARY TABLE `tmp` (a int); SELECT 'TABLE `x`'; END";
            " AS SELECT `t`.`a` AS `a` FROM `d`.`t`";
            " BEFORE INSERT ON `t` FOR EACH ROW SET NEW.a = 1";
            " start with 1 minvalue 1 maxvalue 9 increment by 1 cache 1000 nocycle ENGINE=InnoDB";
          ]
      in
      return ("CREATE " ^ before ^ kind ^ " " ^ guard ^ quoted name ^ body, name))
    (fun (statement, name) -> Option.equal String.equal (Quiver_load.Sql_text.created statement) (Some name))

let tests =
  [
    QCheck_alcotest.to_alcotest rewritten;
    Alcotest.test_case "a database bearing the name of one of its tables is refused" `Quick (fun () ->
        match Qualifier.make ~from:(ident "shop") ~into:(ident "x") ~names:[ ident "t"; ident "shop" ] with
        | Error (`Qualifier (Qualifier.Ambiguous name)) when String.equal (name :> string) "shop" -> ()
        | Error (`Qualifier _) | Ok _ -> Alcotest.fail "taken");
    Alcotest.test_case "definitions as TiDB and MariaDB show them" `Quick (fun () ->
        let renamed =
          Result.get_ok (Qualifier.make ~from:(ident "sq") ~into:(ident "sq_copy") ~names:[ ident "t"; ident "used_one"; ident "p" ])
        in
        Alcotest.(check (list string))
          "rewritten"
          [
            "CREATE TABLE `t` (\n  `id` int NOT NULL DEFAULT (nextval(`sq_copy`.`used_one`)),\n  `c` varchar(9) DEFAULT 'sq.t'\n)";
            "CREATE VIEW `v` (`id`) AS SELECT `t`.`id` AS `id` FROM `sq_copy`.`t` JOIN `sq2`.`t` WHERE `p`.`x` = 1";
          ]
          (List.map (Qualifier.rewrite renamed)
             [
               "CREATE TABLE `t` (\n  `id` int NOT NULL DEFAULT (nextval(`sq`.`used_one`)),\n  `c` varchar(9) DEFAULT 'sq.t'\n)";
               "CREATE VIEW `v` (`id`) AS SELECT `t`.`id` AS `id` FROM `sq`.`t` JOIN `sq2`.`t` WHERE `p`.`x` = 1";
             ]));
      QCheck_alcotest.to_alcotest created;
      Alcotest.test_case "statements that make nothing name nothing" `Quick (fun () ->
          Alcotest.(check (list (option string)))
            "none"
            [ None; None; None ]
            (List.map Quiver_load.Sql_text.created
               [ "SET character_set_client = utf8mb4"; "DO SETVAL(`s`, 1)"; "/*!40101 SET NAMES utf8mb4*/" ]));
  ]
