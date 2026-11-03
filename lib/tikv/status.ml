open Base.Result.Let_syntax

type error = Refused of { code : int; body : string } | Malformed of string | Address of string [@@deriving sexp_of ~portable]
type handle = Integer_key | Common_key | Row_number [@@deriving sexp_of ~portable]
type ids = { columns : (string * int) list; indexes : (string * int) list; handle : handle } [@@deriving sexp_of ~portable]

let body text =
  let malformed what = Error (`Status (Malformed what)) in
  match Base.String.substr_index text ~pattern:"\r\n\r\n" with
  | None -> malformed "no end of the headers"
  | Some ending -> (
      let status, lines =
        match String.split_on_char '\n' (String.sub text 0 ending) |> List.map (fun line -> Base.String.rstrip line ~drop:(Char.equal '\r')) with
        | status :: lines -> (status, lines)
        | [] -> ("", [])
      in
      let rest = String.sub text (ending + 4) (String.length text - ending - 4) in
      let headers =
        List.filter_map
          (fun line ->
            Option.map
              (fun (name, value) -> (String.lowercase_ascii (String.trim name), String.trim value))
              (Base.String.lsplit2 line ~on:':'))
          lines
      in
      let%bind code =
        match String.split_on_char ' ' status with
        | version :: code :: _ when String.starts_with ~prefix:"HTTP/1." version ->
            Option.to_result ~none:(`Status (Malformed "a status that is not a number")) (int_of_string_opt code)
        | _ -> malformed "no status line"
      in
      let%bind body =
        match (List.assoc_opt "transfer-encoding" headers, Option.bind (List.assoc_opt "content-length" headers) int_of_string_opt) with
        | Some coding, _ when String.lowercase_ascii coding = "chunked" ->
            let buffer = Buffer.create (String.length rest) in
            let rec chunks pos =
              match Base.String.substr_index rest ~pos ~pattern:"\r\n" with
              | None -> malformed "a chunk without its size"
              | Some line_end -> (
                  let line = String.sub rest pos (line_end - pos) in
                  let size = String.trim (Option.fold ~none:line ~some:fst (Base.String.lsplit2 line ~on:';')) in
                  match int_of_string_opt ("0x" ^ size) with
                  | Some 0 -> Ok (Buffer.contents buffer)
                  | Some size when size > 0 && line_end + 2 + size + 2 <= String.length rest ->
                      Buffer.add_substring buffer rest (line_end + 2) size;
                      chunks (line_end + 2 + size + 2)
                  | Some _ | None -> malformed "a chunk cut short or of no size")
            in
            chunks 0
        | _, Some length when length >= 0 && length <= String.length rest -> Ok (String.sub rest 0 length)
        | _, Some _ -> malformed "a body shorter than its length"
        | _, None -> Ok rest
      in
      match code with 200 -> Ok body | code -> Error (`Status (Refused { code; body })))

let ids text =
  let malformed what = Error (`Status (Malformed what)) in
  match Yojson.Safe.from_string text with
  | exception Yojson.Json_error message -> malformed message
  | `Assoc fields ->
      let named field name =
        match List.assoc_opt field name with
        | Some (`Assoc name) -> (
            match List.assoc_opt "L" name with Some (`String lower) -> Some lower | Some _ | None -> None)
        | Some _ | None -> None
      in
      let listed key name_field =
        match List.assoc_opt key fields with
        | Some (`List items) ->
            Base.Result.all
              (List.map
                 (fun item ->
                   match item with
                   | `Assoc item -> (
                       match (named name_field item, List.assoc_opt "id" item) with
                       | Some name, Some (`Int id) -> Ok (name, id)
                       | _ -> malformed (key ^ ": an item without its name or id"))
                   | _ -> malformed (key ^ ": an item that is not an object"))
                 items)
        | Some `Null | None -> Ok []
        | Some _ -> malformed (key ^ " is not a list")
      in
      let%bind columns = listed "cols" "name" in
      let%bind indexes = listed "index_info" "idx_name" in
      let%bind handle =
        match (List.assoc_opt "pk_is_handle" fields, List.assoc_opt "is_common_handle" fields) with
        | Some (`Bool true), _ -> Ok Integer_key
        | _, Some (`Bool true) -> Ok Common_key
        | (Some (`Bool false) | None), (Some (`Bool false) | None) -> Ok Row_number
        | _ -> malformed "a handle that is not true or false"
      in
      Ok { columns; indexes; handle }
  | _ -> malformed "not an object"

let path (name : Quiver_core.Schema.Name.t) =
  let escaped text =
    String.concat ""
      (List.map
         (fun c ->
           match c with
           | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '.' | '~' -> String.make 1 c
           | c -> Printf.sprintf "%%%02X" (Char.code c))
         (List.init (String.length text) (String.get text)))
  in
  "/schema/" ^ escaped (name.database :> string) ^ "/" ^ escaped (name.table :> string)

let table ~timeout address name =
  let%bind host, port =
    match Base.String.rsplit2 address ~on:':' with
    | Some (host, port) -> (
        let host = Option.value ~default:host (Option.bind (Base.String.chop_prefix host ~prefix:"[") (Base.String.chop_suffix ~suffix:"]")) in
        match int_of_string_opt port with Some port -> Ok (host, port) | None -> Error (`Status (Address address)))
    | None -> Error (`Status (Address address))
  in
  let%bind transport = Quiver_mysql.Transport.connect ~timeout (Tcp { host; port }) in
  let received =
    let%bind () =
      Quiver_mysql.Transport.write transport
        [%string "GET %{path name} HTTP/1.1\r\nHost: %{address}\r\nAccept: application/json\r\nConnection: close\r\n\r\n"]
    in
    let chunk = Base_bigstring.create 65536 and buffer = Buffer.create 65536 in
    let rec read () =
      match Quiver_mysql.Transport.read transport chunk ~off:0 ~len:65536 with
      | 0 -> Option.fold ~none:(Ok (Buffer.contents buffer)) ~some:(fun error -> Error (`Transport error)) (Quiver_mysql.Transport.failure transport)
      | count ->
          Buffer.add_string buffer (Base_bigstring.To_string.sub chunk ~pos:0 ~len:count);
          read ()
    in
    read ()
  in
  Quiver_mysql.Transport.close transport;
  let%bind text = received in
  let%bind body = body text in
  ids body
