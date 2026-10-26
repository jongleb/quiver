type deferral = All_keys | Foreign_keys
type group = Unique | Plain
type key = { name : string; definition : string; group : group }

(** A line the table keeps, or a key [with_keys] may put back in its place. *)
type line = Fixed of string | Deferred of key

type outline : immutable_data = Whole_text | Lines of { first : string; lines : line list; tail : string list }

type t = {
  create : string;
  later : string list;
  foreign : string list;
  keys : key list;
  special : string list;
  outline : outline;
}

(** What one line of a table definition declares, as far as loading cares. *)
type definition = Kept | Plain_key | Spatial_key | Full_text_key | Foreign_key

let alter table additions =
  match additions with
  | [] -> []
  | _ ->
      [
        "ALTER TABLE " ^ Quiver_core.Ident.backquoted table ^ " "
        ^ String.concat ", " (List.map (fun added -> "ADD " ^ added) additions);
      ]

let split deferral table statement =
  let whole = { create = statement; later = []; foreign = []; keys = []; special = []; outline = Whole_text } in
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
          let declared line =
            if
              List.exists
                (fun prefix -> Base.String.is_prefix line ~prefix)
                [ "  KEY "; "  UNIQUE KEY " ]
            then Plain_key
            else if Base.String.is_prefix line ~prefix:"  SPATIAL KEY " then Spatial_key
            else if Base.String.is_prefix line ~prefix:"  FULLTEXT KEY " then
              Full_text_key
            else if
              Base.String.is_prefix line ~prefix:"  CONSTRAINT "
              && Base.String.is_substring line ~substring:" FOREIGN KEY "
            then Foreign_key
            else Kept
          in
          let classify line =
            match (deferral, declared line) with
            | Foreign_keys, (Plain_key | Spatial_key | Full_text_key) -> Kept
            | (All_keys | Foreign_keys), kind -> kind
          in
          let auto =
            List.find_map
              (fun line ->
                if
                  Base.String.is_prefix line ~prefix:"  `"
                  && Base.String.is_substring line ~substring:" AUTO_INCREMENT"
                then identifier line 2
                else None)
              lines
          and primary =
            List.find_map
              (fun line ->
                if Base.String.is_prefix line ~prefix:"  PRIMARY KEY " then first_column line
                else None)
              lines
          in
          let needed =
            match (auto, primary) with
            | Some column, Some leading when String.equal column leading -> None
            | Some column, (Some _ | None) ->
                List.find_index
                  (fun line ->
                    match classify line with
                    | Plain_key | Spatial_key ->
                        Option.equal String.equal (first_column line)
                          (Some column)
                    | Kept | Full_text_key | Foreign_key -> false)
                  lines
            | None, (Some _ | None) -> None
          in
          let definitions =
            List.mapi
              (fun index line ->
                let kind =
                  match needed with
                  | Some kept when kept = index -> Kept
                  | Some _ | None -> classify line
                in
                (kind, String.trim line))
              lines
          in
          let of_kind wanted =
            List.filter_map
              (fun (kind, line) -> if kind = wanted then Some line else None)
              definitions
          in
          match of_kind Kept with
          | [] -> whole
          | kept ->
              let full_text = List.concat_map (fun key -> alter table [ key ]) (of_kind Full_text_key) in
              let named definition =
                Option.bind (Base.String.substr_index definition ~pattern:"KEY `") (fun at -> identifier definition (at + 4))
              in
              let spatial = List.find_index (fun (kind, _) -> kind = Spatial_key) definitions in
              let after_spatial index = Option.fold spatial ~none:false ~some:(fun first -> index > first) in
              let lines =
                List.concat
                  (List.mapi
                     (fun index (kind, line) ->
                       match (kind, named line) with
                       | Kept, _ -> [ Fixed line ]
                       | Plain_key, Some name -> (
                           let group = if Base.String.is_prefix line ~prefix:"UNIQUE KEY " then Unique else Plain in
                           match (group, after_spatial index) with
                           | Unique, _ | Plain, false -> [ Deferred { name; definition = line; group } ]
                           | Plain, true -> [])
                       | (Plain_key | Spatial_key | Full_text_key | Foreign_key), _ -> [])
                     definitions)
              in
              let keys =
                Option.fold primary ~none:[] ~some:(fun _ ->
                    List.filter_map (function Deferred key -> Some key | Fixed _ -> None) lines)
              in
              {
                create =
                  first ^ "\n"
                  ^ String.concat ",\n"
                      (List.map (fun line -> "  " ^ line) kept)
                  ^ "\n" ^ String.concat "\n" tail;
                later =
                  List.filter_map
                    (fun (kind, line) ->
                      match kind with Plain_key | Spatial_key -> Some line | Kept | Full_text_key | Foreign_key -> None)
                    definitions
                  |> alter table
                  |> Fun.flip ( @ ) full_text;
                foreign = alter table (of_kind Foreign_key);
                keys;
                special =
                  alter table
                    (List.filter_map
                       (fun (kind, line) ->
                         match kind with
                         | Spatial_key | Plain_key
                           when not (List.exists (fun (key : key) -> String.equal key.definition line) keys) ->
                             Some line
                         | Spatial_key | Plain_key | Kept | Full_text_key | Foreign_key -> None)
                       definitions)
                  @ full_text;
                outline = Lines { first; lines; tail };
              }))
  | _ -> whole

let with_keys t keys =
  match t.outline with
  | Whole_text -> t.create
  | Lines { first; lines; tail } ->
      first ^ "\n"
      ^ String.concat ",\n"
          (List.filter_map
             (function
               | Fixed line -> Some ("  " ^ line)
               | Deferred key ->
                   if List.exists (fun (other : key) -> String.equal other.name key.name) keys then Some ("  " ^ key.definition)
                   else None)
             lines)
      ^ "\n" ^ String.concat "\n" tail

let adding table keys = alter table (List.map (fun key -> key.definition) keys)
