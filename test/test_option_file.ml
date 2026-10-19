let name =
  QCheck2.Gen.(
    let+ first = char_range 'a' 'z'
    and+ rest =
      string_size
        ~gen:(oneof_list (Base.String.to_list "abcdefghijklmnopqrstuvwxyz0123456789-"))
        (int_range 0 12)
    in
    let name = String.make 1 first ^ rest in
    if String.starts_with ~prefix:"loose-" name then "x" ^ name else name)

let restyled ~dashes text =
  QCheck2.Gen.(
    let+ flips = list_size (return (String.length text)) bool in
    String.concat ""
      (List.mapi
         (fun i flip ->
           match (text.[i], flip) with
           | '-', true -> ( match dashes with `Either -> "_" | `Kept -> "-")
           | c, true -> String.make 1 (Char.uppercase_ascii c)
           | c, false -> String.make 1 c)
         flips))

let space = QCheck2.Gen.(oneof_list [ ""; " "; "  "; "\t" ])

let comment =
  QCheck2.Gen.(
    map
      (fun text -> String.map (function '\n' | '\r' -> ' ' | c -> c) text)
      (string_size ~gen:printable (int_range 0 10)))

let escaped quote value =
  String.concat ""
    (List.mapi
       (fun i c ->
         match (c, quote) with
         | '\\', _ -> {|\\|}
         | '\n', _ -> {|\n|}
         | '\t', _ -> {|\t|}
         | '\r', _ -> {|\r|}
         | '\b', _ -> {|\b|}
         | ('"' | '\''), None -> "\\" ^ String.make 1 c
         | c, Some quote when Char.equal c quote -> "\\" ^ String.make 1 c
         | ' ', None when i = 0 || i = String.length value - 1 -> {|\s|}
         | c, (Some (_ : char) | None) -> String.make 1 c)
       (Base.String.to_list value))

let setting =
  QCheck2.Gen.(
    let* name = name
    and* value =
      option
        (string_size
           ~gen:(oneof [ printable; oneof_list [ '\t'; '\n'; '\r'; '\b'; '#'; '"'; '\'' ] ])
           (int_range 0 20))
    and* lead = space
    and* around = space
    and* loose = bool
    and* quote = oneof_list [ None; Some '"'; Some '\'' ]
    and* comment = option comment in
    let+ styled = restyled ~dashes:`Either name in
    let line = Quiver_mysql.Option_file.Option { name; value } in
    let named = lead ^ (if loose then "LoOse_" else "") ^ styled ^ around in
    let quote =
      match (quote, value) with
      | None, Some value when String.contains value '#' -> Some '"'
      | quote, (Some (_ : string) | None) -> quote
    in
    let ending =
      match (comment, quote, value) with
      | Some comment, Some (_ : char), (Some (_ : string) | None) -> " # " ^ comment
      | Some comment, None, Some value
        when not (String.contains value '"' || String.contains value '\'') ->
          " # " ^ comment
      | Some comment, None, None -> " # " ^ comment
      | (Some (_ : string) | None), (Some (_ : char) | None), (Some (_ : string) | None) -> ""
    in
    ( line,
      named
      ^ Option.fold ~none:""
          ~some:(fun value ->
            "=" ^ around
            ^ Option.fold ~none:(escaped None value)
                ~some:(fun quote ->
                  String.make 1 quote ^ escaped (Some quote) value ^ String.make 1 quote)
                quote)
          value
      ^ ending ))

let group =
  QCheck2.Gen.(
    let* name = name
    and* lead = space
    and* inside = space
    and* after = space
    and* comment = option comment in
    let+ styled = restyled ~dashes:`Kept name in
    ( Some (Quiver_mysql.Option_file.Group name),
      lead ^ "[" ^ styled ^ inside ^ "]" ^ after
      ^ Option.fold ~none:"" ~some:(fun comment -> "# " ^ comment) comment ))

let include_ =
  QCheck2.Gen.(
    let+ path =
      string_size
        ~gen:(oneof_list (Base.String.to_list "abcXYZ019/._-"))
        (int_range 1 20)
    and+ directory = bool
    and+ lead = space
    and+ after = space in
    if directory then
      (Some (Quiver_mysql.Option_file.Include_dir path), lead ^ "!includedir " ^ path ^ after)
    else (Some (Quiver_mysql.Option_file.Include path), lead ^ "!include  " ^ path ^ after))

let noise =
  QCheck2.Gen.(
    let+ lead = space
    and+ text = comment
    and+ mark = oneof_list [ "#"; ";"; "" ] in
    (None, lead ^ match mark with "" -> "" | mark -> mark ^ text))

let tests =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:2000
      ~name:"an option file's lines read back as written, however they are styled"
      ~print:(fun (lines, text) ->
        Sexplib0.Sexp.to_string_hum
          (Sexplib0.Sexp_conv.sexp_of_list Quiver_mysql.Option_file.sexp_of_line lines)
        ^ "\n" ^ text)
      QCheck2.Gen.(
        let+ entries =
          list_size (int_range 0 30)
            (oneof_weighted
               [
                 (5, map (fun (line, text) -> (Some line, text)) setting);
                 (2, group);
                 (1, include_);
                 (2, noise);
               ])
        and+ ending = oneof_list [ "\n"; "\r\n" ] in
        ( List.filter_map fst entries,
          String.concat ending (List.map snd entries) ))
      (fun (lines, text) ->
        List.equal Quiver_mysql.Option_file.equal_line lines
          (Quiver_mysql.Option_file.lines text)));
  ]
