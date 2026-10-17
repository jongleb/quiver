type error = Malformed of string | Tag_unsupported of Server.t
[@@deriving sexp_of ~portable]

module Uuid = struct
  type t = string [@@deriving sexp_of ~portable, equal ~portable, compare ~portable]
end

module Tag = struct
  type t = string [@@deriving sexp_of ~portable, equal ~portable, compare ~portable]
end

module Interval = struct
  type t = { first : int64; last : int64 } [@@deriving sexp_of ~portable, equal ~portable]
end

type mysql_set = {
  uuid : Uuid.t;
  tag : Tag.t option;
  intervals : Interval.t list;
}
[@@deriving sexp_of ~portable, equal ~portable]

type mariadb_position = { domain : int; server : int; sequence : int64 }
[@@deriving sexp_of ~portable, equal ~portable]

type t = Mysql of mysql_set list | Mariadb of mariadb_position list
[@@deriving sexp_of ~portable, equal ~portable]

let digits = Angstrom.take_while1 (function '0' .. '9' -> true | _ -> false)

let white =
  Angstrom.skip_while (function ' ' | '\t' | '\n' | '\r' -> true | _ -> false)

let parse_mysql ~tags raw =
  let open Angstrom.Let_syntax in
  let number =
    let%bind text = digits in
    match Int64.of_string_opt text with
    | Some n when Int64.compare n 1L >= 0 -> Angstrom.return n
    | Some _ | None -> Angstrom.fail "transaction number"
  in
  let interval =
    let%bind first = number in
    let%bind last = Angstrom.(char '-' *> number <|> return first) in
    if Int64.compare first last <= 0 then
      Angstrom.return (`Interval { Interval.first; last })
    else Angstrom.fail "interval"
  in
  let tag =
    let%map first =
      Angstrom.satisfy (function
        | 'a' .. 'z' | 'A' .. 'Z' | '_' -> true
        | _ -> false)
    and rest =
      Angstrom.take_while (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
        | _ -> false)
    in
    `Tag (String.lowercase_ascii (String.make 1 first ^ rest))
  in
  let uuid =
    let%bind text = Angstrom.take 36 in
    Option.fold ~none:(Angstrom.fail "uuid")
      ~some:(fun uuid -> Angstrom.return (Uuidm.to_string uuid))
      (Uuidm.of_string text)
  in
  let source =
    Angstrom.(
      white *> both uuid (many (char ':' *> (interval <|> tag))) <* white)
  in
  let sets (uuid, parts) =
    let flush tag intervals sets =
      match intervals with
      | [] -> sets
      | _ -> { uuid; tag; intervals = List.rev intervals } :: sets
    in
    let tag, intervals, sets =
      List.fold_left
        (fun (tag, intervals, sets) -> function
          | `Interval interval -> (tag, interval :: intervals, sets)
          | `Tag next -> (Some next, [], flush tag intervals sets))
        (None, [], []) parts
    in
    List.rev (flush tag intervals sets)
  in
  match
    Angstrom.(
      parse_string ~consume:All
        (white *> sep_by (char ',') source <* end_of_input)
        raw)
  with
  | Error _ -> Error (`Gtid (Malformed raw))
  | Ok sources -> (
      match
        ( tags,
          List.find_map
            (fun (_, parts) ->
              List.find_map
                (function `Tag tag -> Some tag | `Interval _ -> None)
                parts)
            sources )
      with
      | `Forbidden server, Some _ -> Error (`Gtid (Tag_unsupported server))
      | `Forbidden _, None | `Allowed, (Some _ | None) ->
          Ok (Mysql (List.concat_map sets sources)))

let parse_mariadb raw =
  let open Angstrom.Let_syntax in
  let small =
    let%bind text = digits in
    Option.fold ~none:(Angstrom.fail "number") ~some:Angstrom.return
      (int_of_string_opt text)
  in
  let position =
    let%map domain = Angstrom.(white *> small <* char '-')
    and server = Angstrom.(small <* char '-')
    and sequence =
      let%bind text = Angstrom.(digits <* white) in
      Option.fold ~none:(Angstrom.fail "sequence") ~some:Angstrom.return
        (Int64.of_string_opt ("0u" ^ text))
    in
    { domain; server; sequence }
  in
  Result.map_error
    (fun _ -> `Gtid (Malformed raw))
    (Result.map
       (fun positions -> Mariadb positions)
       Angstrom.(
         parse_string ~consume:All
           (white *> sep_by (char ',') position <* end_of_input)
           raw))

let of_server_string (server : Server.t) raw =
  match server with
  | Mysql_8_0 _ -> parse_mysql ~tags:(`Forbidden server) raw
  | Mysql_8_4 _ -> parse_mysql ~tags:`Allowed raw
  | Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _ -> parse_mariadb raw

let to_string = function
  | Mysql sets ->
      String.concat ","
        (List.map
           (fun { uuid; tag; intervals } ->
             String.concat ":"
               ((uuid :: Option.to_list tag)
               @ List.map
                   (fun { Interval.first; last } ->
                     if Int64.equal first last then Int64.to_string first
                     else Printf.sprintf "%Ld-%Ld" first last)
                   intervals))
           sets)
  | Mariadb positions ->
      String.concat ","
        (List.map
           (fun { domain; server; sequence } ->
             Printf.sprintf "%d-%d-%Lu" domain server sequence)
           positions)

let yojson_of_t = function
  | Mysql _ as gtid -> `Assoc [ ("mysql", `String (to_string gtid)) ]
  | Mariadb _ as gtid -> `Assoc [ ("mariadb", `String (to_string gtid)) ]

let t_of_yojson json =
  match
    match json with
    | `Assoc [ ("mysql", `String raw) ] -> parse_mysql ~tags:`Allowed raw
    | `Assoc [ ("mariadb", `String raw) ] -> parse_mariadb raw
    | _ -> Error (`Gtid (Malformed (Yojson.Safe.to_string json)))
  with
  | Ok gtid -> gtid
  | Error (`Gtid error) ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
        (Sexplib0.Sexp.to_string (sexp_of_error error))
        json
