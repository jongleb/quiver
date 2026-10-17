(** A piece of a routine body; quoted pieces hold their unescaped content. *)
type token =
  | Code of string
  | Single of string
  | Double of string
  | Backtick of string
  | Line of string
  | Block of string

let quoted escapes quote content =
  let body =
    String.concat ""
      (List.map
         (fun c ->
           if c = quote then
             match escapes with
             | Quiver_sink.Myloader.Backslash_escapes when quote <> '`' ->
                 "\\" ^ String.make 1 quote
             | Backslash_escapes | No_backslash_escapes -> String.make 2 quote
           else if c = '\\' then
             match escapes with
             | Quiver_sink.Myloader.Backslash_escapes when quote <> '`' ->
                 "\\\\"
             | Backslash_escapes | No_backslash_escapes -> "\\"
           else String.make 1 c)
         (Base.String.to_list content))
  in
  String.make 1 quote ^ body ^ String.make 1 quote

let text escapes tokens =
  String.concat ""
    (List.map
       (function
         | Code text -> text
         | Single content -> quoted escapes '\'' content
         | Double content -> quoted escapes '"' content
         | Backtick content -> quoted escapes '`' content
         | Line content -> "#" ^ content ^ "\n"
         | Block content -> "/*" ^ content ^ "*/")
       tokens)

let has text = Base.String.is_substring text ~substring:";\n"

let token =
  QCheck2.Gen.(
    let chars alphabet =
      string_size ~gen:(oneof_list alphabet) (int_range 0 8)
    in
    oneof
      [
        map (fun s -> Code s) (chars [ 'a'; ';'; '\n'; ' '; '(' ]);
        map (fun s -> Single s) (chars [ 'a'; ';'; '\n'; '\''; '\\'; '"' ]);
        map (fun s -> Double s) (chars [ 'a'; ';'; '\n'; '"'; '\\'; '\'' ]);
        map (fun s -> Backtick s) (chars [ 'a'; ';'; '\n'; '`' ]);
        map (fun s -> Line s) (chars [ 'a'; ';'; ' ' ]);
        map (fun s -> Block s) (chars [ 'a'; ';'; '\n'; ' ' ]);
      ])

let property escapes =
  QCheck2.Test.make ~count:2000
    ~name:
      (match escapes with
      | Quiver_sink.Myloader.Backslash_escapes ->
          "myloader text, backslash escapes"
      | No_backslash_escapes -> "myloader text, NO_BACKSLASH_ESCAPES")
    ~print:(fun tokens -> String.escaped (text escapes tokens))
    QCheck2.Gen.(list_size (int_range 0 12) token)
    (fun tokens ->
      let input = text escapes tokens in
      let in_literal =
        List.exists
          (function
            | (Single _ | Double _ | Backtick _) as literal ->
                has (text escapes [ literal ])
            | Code _ | Line _ | Block _ -> false)
          tokens
      and normal text =
        Base.String.substr_replace_all text ~pattern:"; \n" ~with_:";\n"
      in
      match (Quiver_sink.Myloader.statement escapes input, in_literal) with
      | None, true -> true
      | Some output, false ->
          (not (has output)) && String.equal (normal output) (normal input)
      | None, false | Some _, true -> false)

let tests =
  List.map QCheck_alcotest.to_alcotest
    [ property Backslash_escapes; property No_backslash_escapes ]
