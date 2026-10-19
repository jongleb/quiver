type line =
  | Group of string
  | Option of { name : string; value : string option }
  | Include of string
  | Include_dir of string
[@@deriving sexp_of, equal]

let lines text =
  let at text i = if i < String.length text then Some text.[i] else None in
  let uncommented line =
    let rec scan i quote escaped =
      if i = String.length line then line
      else
        let c = line.[i] in
        match (c, quote) with
        | ('\'' | '"'), None when not escaped -> scan (i + 1) (Some c) false
        | ('\'' | '"'), Some open_quote when Char.equal c open_quote && not escaped ->
            scan (i + 1) None false
        | '#', None -> String.sub line 0 i
        | _, Some (_ : char) -> scan (i + 1) quote (Char.equal c '\\' && not escaped)
        | _, None -> scan (i + 1) None false
    in
    scan 0 None false
  and unescaped value =
    let buffer = Buffer.create (String.length value) in
    let rec go i =
      if i < String.length value then
        match (value.[i], at value (i + 1)) with
        | '\\', Some escaped ->
            let () =
              match escaped with
              | 'b' -> Buffer.add_char buffer '\b'
              | 't' -> Buffer.add_char buffer '\t'
              | 'n' -> Buffer.add_char buffer '\n'
              | 'r' -> Buffer.add_char buffer '\r'
              | 's' -> Buffer.add_char buffer ' '
              | '"' | '\'' | '\\' -> Buffer.add_char buffer escaped
              | other ->
                  Buffer.add_char buffer '\\';
                  Buffer.add_char buffer other
            in
            go (i + 2)
        | c, (Some (_ : char) | None) ->
            Buffer.add_char buffer c;
            go (i + 1)
    in
    go 0;
    Buffer.contents buffer
  and unquoted value =
    match Base.String.length value with
    | length
      when length >= 2
           && (Char.equal value.[0] '"' || Char.equal value.[0] '\'')
           && Char.equal value.[0] value.[length - 1] ->
        String.sub value 1 (length - 2)
    | (_ : int) -> value
  and directive keyword line =
    Option.map String.trim (Base.String.chop_prefix line ~prefix:keyword)
  in
  List.filter_map
    (fun raw ->
      let line = String.trim raw in
      match at line 0 with
      | None | Some ('#' | ';') -> None
      | Some '[' ->
          Option.map
            (fun close ->
              Group (String.lowercase_ascii (String.trim (String.sub line 1 (close - 1)))))
            (String.index_opt line ']')
      | Some '!' -> (
          match directive "!includedir" line with
          | Some directory -> Some (Include_dir directory)
          | None -> Option.map (fun file -> Include file) (directive "!include" line))
      | Some (_ : char) ->
          let option = uncommented line in
          let name, value =
            match Base.String.lsplit2 option ~on:'=' with
            | None -> (option, None)
            | Some (name, value) -> (name, Some (unescaped (unquoted (String.trim value))))
          in
          let name =
            Base.String.tr ~target:'_' ~replacement:'-' (String.lowercase_ascii (String.trim name))
          in
          Some (Option { name = Base.String.chop_prefix_if_exists name ~prefix:"loose-"; value }))
    (String.split_on_char '\n' text)
