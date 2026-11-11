open Quiver_tikv

(* A piece of a definition: kept text, or a placement comment naming a policy. *)
type token = Kept of string | Clause of string

let gen =
  QCheck2.Gen.(
    let name = oneof_list [ "p1"; "p 2"; "p`q"; "east``"; "Δ" ] in
    let marker = "/*T![placement] PLACEMENT POLICY=`fake` */" in
    let quoted quote body =
      let escaped =
        String.concat ""
          (List.map
             (fun c ->
               match c with
               | c when c = quote -> String.make 2 quote
               | '\\' -> "\\\\"
               | c -> String.make 1 c)
             (List.init (String.length body) (String.get body)))
      in
      String.make 1 quote ^ escaped ^ String.make 1 quote
    in
    let body = oneof_list [ "it's"; marker; "a\\b"; "\"x\""; " "; "*/"; "`" ] in
    list_size (int_range 0 12)
      (oneof_weighted
         [
           (3, map (fun word -> Kept word) (oneof_list [ "CREATE TABLE "; " ("; "`a` int"; ", "; ") ENGINE=InnoDB"; "\n"; " " ]));
           (2, map (fun text -> Kept (" COMMENT " ^ quoted '\'' text)) body);
           (1, map (fun text -> Kept (" DEFAULT " ^ quoted '"' text)) body);
           (1, map (fun text -> Kept (" " ^ quoted '`' text)) (oneof_list [ "id"; "we`ird"; marker ]));
           (1, return (Kept " /*T![clustered_index] CLUSTERED */"));
           (2, map (fun name -> Clause name) name);
         ]))

let text tokens =
  String.concat ""
    (List.map
       (function
         | Kept text -> text
         | Clause name -> " /*T![placement] PLACEMENT POLICY=`" ^ Base.String.substr_replace_all name ~pattern:"`" ~with_:"``" ^ "` */")
       tokens)

let read =
  QCheck2.Test.make ~count:3000 ~name:"placement comments read outside quotes, the rest left as it was"
    ~print:(fun tokens -> text tokens)
    gen
    (fun tokens ->
      match Placement.read (text tokens) with
      | Error _ -> false
      | Ok placed ->
          let named = List.filter_map (function Clause name -> Some name | Kept _ -> None) tokens in
          let first = List.fold_left (fun seen name -> if List.mem name seen then seen else seen @ [ name ]) [] named in
          Placement.policies placed = first
          && Placement.without placed = String.concat "" (List.filter_map (function Kept text -> Some text | Clause _ -> None) tokens))

let refused () =
  let shown text =
    match Placement.read text with
    | Ok placed -> String.concat "," (Placement.policies placed)
    | Error (`Placement error) -> Base.Sexp.to_string (Placement.sexp_of_error error)
  in
  Alcotest.(check string) "TiDB's own" "p1,p 2"
    (shown
       "CREATE TABLE `pl` (\n  `id` int NOT NULL,\n  PRIMARY KEY (`id`) /*T![clustered_index] CLUSTERED */\n) ENGINE=InnoDB /*T![placement] PLACEMENT POLICY=`p1` */\nPARTITION BY RANGE (`id`)\n(PARTITION `p0` VALUES LESS THAN (10) /*T![placement] PLACEMENT POLICY=`p 2` */,\n PARTITION `p1` VALUES LESS THAN (100))");
  Alcotest.(check string) "a comment naming no policy" {|(Unreadable"/*T![placement] PRIMARY_REGION=\"x\" */")|}
    (shown {|) /*T![placement] PRIMARY_REGION="x" */|});
  Alcotest.(check string) "a name never closed" {|(Unreadable"/*T![placement] PLACEMENT POLICY=`p */")|} (shown ") /*T![placement] PLACEMENT POLICY=`p */");
  Alcotest.(check string) "text after the name" {|(Unreadable"/*T![placement] PLACEMENT POLICY=`p` x */")|}
    (shown ") /*T![placement] PLACEMENT POLICY=`p` x */")

let tests = [ QCheck_alcotest.to_alcotest read; Alcotest.test_case "refusals" `Quick refused ]
