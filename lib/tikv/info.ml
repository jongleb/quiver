open Base.Result.Let_syntax

type error = Malformed of string [@@deriving sexp_of ~portable]
type handle = Integer_key | Common_key | Row_number [@@deriving sexp_of ~portable]
type state = Public | Changing [@@deriving sexp_of ~portable]
type missing = Unset | Origin of string [@@deriving sexp_of ~portable]
type sign = Signed | Unsigned [@@deriving sexp_of ~portable]
type clock = Utc | Server [@@deriving sexp_of ~portable]

type kind =
  | Integer of sign
  | Year
  | Float
  | Double
  | Decimal of { precision : int; scale : int }
  | Char
  | Bytes
  | Date
  | Datetime of { fsp : int }
  | Timestamp of { clock : clock; fsp : int }
  | Time of { fsp : int }
  | Enum of string list
  | Set of string list
  | Bit
  | Json
  | Unknown
[@@deriving sexp_of ~portable]

type column = { name : string; id : int; state : state; missing : missing; kind : kind } [@@deriving sexp_of ~portable]
type scope = Local | Global [@@deriving sexp_of ~portable]
type index = { name : string; id : int64; state : state; scope : scope } [@@deriving sexp_of ~portable]
type partition = { name : string; id : int64 } [@@deriving sexp_of ~portable]
type layout = Whole | Partitioned of partition * partition list | Rearranging [@@deriving sexp_of ~portable]

type t = { id : int64; columns : column list; indexes : index list; handle : handle; layout : layout }
[@@deriving sexp_of ~portable]

let malformed what = Error (`Info (Malformed what))

let fields text =
  match Yojson.Safe.from_string text with
  | exception Yojson.Json_error message -> malformed message
  | `Assoc fields -> Ok fields
  | _ -> malformed "not an object"

let lower field item =
  match List.assoc_opt field item with
  | Some (`Assoc name) -> ( match List.assoc_opt "L" name with Some (`String lower) -> Some lower | Some _ | None -> None)
  | Some _ | None -> None

let of_json text =
  let%bind fields = fields text in
  let listed key parse =
    match List.assoc_opt key fields with
    | Some (`List items) ->
        Base.Result.all
          (List.map (fun item -> match item with `Assoc item -> parse item | _ -> malformed (key ^ ": an item that is not an object")) items)
    | Some `Null | None -> Ok []
    | Some _ -> malformed (key ^ " is not a list")
  in
  let state key item =
    match List.assoc_opt "state" item with
    | Some (`Int 5) -> Ok Public
    | Some (`Int _) -> Ok Changing
    | Some _ | None -> malformed (key ^ ": an item without its state")
  in
  let%bind columns =
    listed "cols" (fun item ->
        let%bind state = state "cols" item in
        let%bind origin =
          let decoded text =
            let sextet c =
              match c with
              | 'A' .. 'Z' -> Some (Char.code c - Char.code 'A')
              | 'a' .. 'z' -> Some (Char.code c - Char.code 'a' + 26)
              | '0' .. '9' -> Some (Char.code c - Char.code '0' + 52)
              | '+' -> Some 62
              | '/' -> Some 63
              | _ -> None
            in
            let body = Base.String.rstrip text ~drop:(Char.equal '=') in
            let bytes = Buffer.create (String.length body * 3 / 4) in
            let rec go i bits held =
              if i >= String.length body then Ok (Buffer.contents bytes)
              else
                match sextet body.[i] with
                | None -> malformed "cols: a default of bits not in base64"
                | Some six ->
                    let bits = (bits lsl 6) lor six and held = held + 6 in
                    if held >= 8 then (
                      Buffer.add_char bytes (Char.chr ((bits lsr (held - 8)) land 0xFF));
                      go (i + 1) (bits land ((1 lsl (held - 8)) - 1)) (held - 8))
                    else go (i + 1) bits held
            in
            go 0 0 0
          in
          match (List.assoc_opt "origin_default" item, List.assoc_opt "origin_default_bit" item) with
          | _, Some (`String bits) -> Result.map (fun bytes -> Origin bytes) (decoded bits)
          | Some (`String text), (Some `Null | None) -> Ok (Origin text)
          | (Some `Null | None), (Some `Null | None) -> Ok Unset
          | _ -> malformed "cols: a default that is not text"
        in
        let missing =
          match (List.assoc_opt "generated_expr_string" item, List.assoc_opt "generated_stored" item) with
          | Some (`String expression), (Some (`Bool false) | None) when expression <> "" -> Unset
          | _ -> origin
        in
        let kind =
          match List.assoc_opt "type" item with
          | Some (`Assoc tp) -> (
              let number key = match List.assoc_opt key tp with Some (`Int n) -> Some n | Some _ | None -> None in
              let charset =
                match List.assoc_opt "Charset" tp with
                | Some (`String "binary") -> `Binary
                | Some (`String ("utf8mb4" | "utf8" | "ascii" | "latin1")) -> `Text
                | Some _ | None -> `Recoded
              and elements =
                let plain = function
                  | Some (`List flags) -> List.for_all (( = ) (`Bool false)) flags
                  | Some `Null | None -> true
                  | Some _ -> false
                in
                match List.assoc_opt "Elems" tp with
                | Some (`List elements) when plain (List.assoc_opt "ElemsIsBinaryLit" tp) ->
                    Base.Option.all (List.map (function `String element -> Some element | _ -> None) elements)
                | Some _ | None -> None
              in
              match (number "Tp", number "Flag", charset) with
              | Some (1 | 2 | 3 | 8 | 9), Some flag, _ when flag land 32 = 0 -> Integer Signed
              | Some (1 | 2 | 3 | 8 | 9), Some _, _ -> Integer Unsigned
              | Some 13, _, _ -> Year
              | Some 4, _, _ -> Float
              | Some 5, _, _ -> Double
              | Some 246, _, _ ->
                  Base.Option.value ~default:Unknown
                    (Base.Option.map2 (number "Flen") (number "Decimal") ~f:(fun precision scale -> Decimal { precision; scale }))
              | Some 254, _, `Binary -> Bytes
              | Some 254, _, `Text -> Char
              | Some (15 | 249 | 250 | 251 | 252 | 253), _, (`Binary | `Text) -> Bytes
              | Some 10, _, _ -> Date
              | Some 12, _, _ -> Option.fold ~none:Unknown ~some:(fun fsp -> Datetime { fsp }) (number "Decimal")
              | Some 7, _, _ ->
                  let clock = match List.assoc_opt "version" item with Some (`Int version) when version >= 1 -> Utc | Some _ | None -> Server in
                  Option.fold ~none:Unknown ~some:(fun fsp -> Timestamp { clock; fsp }) (number "Decimal")
              | Some 11, _, _ -> Option.fold ~none:Unknown ~some:(fun fsp -> Time { fsp }) (number "Decimal")
              | Some 247, _, _ -> Option.fold ~none:Unknown ~some:(fun elements -> Enum elements) elements
              | Some 248, _, _ -> Option.fold ~none:Unknown ~some:(fun elements -> Set elements) elements
              | Some 16, _, _ -> Bit
              | Some 245, _, _ -> Json
              | _ -> Unknown)
          | Some _ | None -> Unknown
        in
        match (lower "name" item, List.assoc_opt "id" item) with
        | Some name, Some (`Int id) -> Ok { name; id; state; missing; kind }
        | _ -> malformed "cols: an item without its name or id")
  in
  let%bind indexes =
    listed "index_info" (fun item ->
        let%bind state = state "index_info" item in
        let%bind scope =
          match List.assoc_opt "is_global" item with
          | Some (`Bool true) -> Ok Global
          | Some (`Bool false) | None -> Ok Local
          | Some _ -> malformed "index_info: is_global that is not true or false"
        in
        match (lower "idx_name" item, List.assoc_opt "id" item) with
        | Some name, Some (`Int id) -> Ok { name; id = Int64.of_int id; state; scope }
        | _ -> malformed "index_info: an item without its name or id")
  in
  let%bind layout =
    let settled value = match value with None | Some (`Null | `List [] | `Int 0) -> true | Some _ -> false in
    let definition item =
      match item with
      | `Assoc item -> (
          match (lower "name" item, List.assoc_opt "id" item) with
          | Some name, Some (`Int id) -> Ok { name; id = Int64.of_int id }
          | _ -> malformed "partition: a definition without its name or id")
      | _ -> malformed "partition: a definition that is not an object"
    in
    let changing partition =
      List.exists
        (fun key -> not (settled (List.assoc_opt key partition)))
        [ "adding_definitions"; "dropping_definitions"; "new_partition_ids"; "ddl_state"; "ddl_action"; "new_table_id" ]
    in
    match List.assoc_opt "partition" fields with
    | _ when not (settled (List.assoc_opt "exchange_partition_info" fields)) -> Ok Rearranging
    | Some `Null | None -> Ok Whole
    | Some (`Assoc partition) when List.assoc_opt "enable" partition = Some (`Bool false) -> Ok Whole
    | Some (`Assoc partition) when changing partition -> Ok Rearranging
    | Some (`Assoc partition) -> (
        match List.assoc_opt "definitions" partition with
        | Some (`List items) -> (
            let%bind definitions = Base.Result.all (List.map definition items) in
            match definitions with first :: rest -> Ok (Partitioned (first, rest)) | [] -> malformed "partition: no definitions")
        | Some _ | None -> malformed "partition: no list of definitions")
    | Some _ -> malformed "partition is not an object"
  in
  let%bind handle =
    match (List.assoc_opt "pk_is_handle" fields, List.assoc_opt "is_common_handle" fields) with
    | Some (`Bool true), _ -> Ok Integer_key
    | _, Some (`Bool true) -> Ok Common_key
    | (Some (`Bool false) | None), (Some (`Bool false) | None) -> Ok Row_number
    | _ -> malformed "a handle that is not true or false"
  in
  match List.assoc_opt "id" fields with
  | Some (`Int id) -> Ok { id = Int64.of_int id; columns; indexes; handle; layout }
  | Some _ | None -> malformed "a table without its id"

let database text =
  let%bind fields = fields text in
  match (lower "db_name" fields, List.assoc_opt "id" fields) with
  | Some name, Some (`Int id) -> Ok (name, Int64.of_int id)
  | _ -> malformed "a database without its name or id"
