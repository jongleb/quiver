type t = { create : string; later : string list }

(** What one line of a table definition declares, as far as loading cares. *)
type definition = Kept | Plain_key | Full_text_key | Foreign_key

let split table statement =
  let whole = { create = statement; later = [] } in
  let identifier text from =
    let length = String.length text in
    let buffer = Buffer.create 16 in
    let rec go i =
      if i >= length then None
      else
        match text.[i] with
        | '`' when i + 1 < length && text.[i + 1] = '`' ->
            Buffer.add_char buffer '`';
            go (i + 2)
        | '`' -> Some (Buffer.contents buffer)
        | c ->
            Buffer.add_char buffer c;
            go (i + 1)
    in
    if from < length && text.[from] = '`' then go (from + 1) else None
  in
  let first_column line =
    Option.bind (Base.String.substr_index line ~pattern:"(`") (fun at ->
        identifier line (at + 1))
  in
  match String.split_on_char '\n' statement with
  | first :: rest
    when Base.String.is_prefix first ~prefix:"CREATE TABLE "
         && Base.String.is_suffix first ~suffix:" (" -> (
      let body, tail =
        Base.List.split_while rest ~f:(fun line ->
            not (Base.String.is_prefix line ~prefix:")"))
      in
      match tail with
      | [] -> whole
      | _ -> (
          let lines =
            List.map
              (fun line -> Base.String.rstrip ~drop:(Char.equal ',') line)
              body
          in
          let classify line =
            if
              List.exists
                (fun prefix -> Base.String.is_prefix line ~prefix)
                [ "  KEY "; "  UNIQUE KEY "; "  SPATIAL KEY " ]
            then Plain_key
            else if Base.String.is_prefix line ~prefix:"  FULLTEXT KEY " then
              Full_text_key
            else if
              Base.String.is_prefix line ~prefix:"  CONSTRAINT "
              && Base.String.is_substring line ~substring:" FOREIGN KEY "
            then Foreign_key
            else Kept
          in
          let auto =
            List.find_map
              (fun line ->
                match
                  ( Base.String.is_prefix line ~prefix:"  `",
                    Base.String.is_substring line ~substring:" AUTO_INCREMENT"
                  )
                with
                | true, true -> identifier line 2
                | (true | false), _ -> None)
              lines
          and primary =
            List.find_map
              (fun line ->
                match Base.String.is_prefix line ~prefix:"  PRIMARY KEY " with
                | true -> first_column line
                | false -> None)
              lines
          in
          let needed =
            match (auto, primary) with
            | Some column, Some leading when String.equal column leading -> None
            | Some column, (Some _ | None) ->
                List.find_index
                  (fun line ->
                    match classify line with
                    | Plain_key ->
                        Option.equal String.equal (first_column line)
                          (Some column)
                    | Kept | Full_text_key | Foreign_key -> false)
                  lines
            | None, (Some _ | None) -> None
          in
          let definitions =
            List.mapi
              (fun index line ->
                ( (match needed with
                  | Some kept when kept = index -> Kept
                  | Some _ | None -> classify line),
                  String.trim line ))
              lines
          in
          let of_kind wanted =
            List.filter_map
              (fun (kind, line) ->
                match kind = wanted with true -> Some line | false -> None)
              definitions
          in
          match of_kind Kept with
          | [] -> whole
          | kept ->
              let alter additions =
                "ALTER TABLE "
                ^ Quiver_mysql.Query.quote table
                ^ " "
                ^ String.concat ", "
                    (List.map (fun added -> "ADD " ^ added) additions)
              in
              let group additions =
                match additions with [] -> [] | _ -> [ alter additions ]
              in
              {
                create =
                  first ^ "\n"
                  ^ String.concat ",\n"
                      (List.map (fun line -> "  " ^ line) kept)
                  ^ "\n" ^ String.concat "\n" tail;
                later =
                  group (of_kind Plain_key)
                  @ List.map (fun key -> alter [ key ]) (of_kind Full_text_key)
                  @ group (of_kind Foreign_key);
              }))
  | _ -> whole
