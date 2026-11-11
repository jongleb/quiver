open Base.Result.Let_syntax

type error = Refused of { code : int; body : string } | Malformed of string | Untrusted of string
[@@deriving sexp_of ~portable]

type request = Post of { path : string; json : string } | Delete of string

let text ~host ~port request =
  let authority = if String.contains host ':' then [%string "[%{host}]:%{port#Int}"] else [%string "%{host}:%{port#Int}"] in
  let head verb path = [%string "%{verb} %{path} HTTP/1.1\r\nHost: %{authority}\r\nAccept: application/json\r\nConnection: close\r\n"] in
  match request with
  | Post { path; json } ->
      head "POST" path ^ [%string "Content-Type: application/json\r\nContent-Length: %{String.length json#Int}\r\n\r\n%{json}"]
  | Delete path -> head "DELETE" path ^ "Content-Length: 0\r\n\r\n"

let parse text =
  let malformed what = Error (`Http1 (Malformed what)) in
  match Base.String.substr_index text ~pattern:"\r\n\r\n" with
  | None -> malformed "no end of the headers"
  | Some ending -> (
      let status, lines =
        match List.map (fun line -> Base.String.rstrip line ~drop:(Char.equal '\r')) (String.split_on_char '\n' (String.sub text 0 ending)) with
        | status :: lines -> (status, lines)
        | [] -> ("", [])
      in
      let rest = String.sub text (ending + 4) (String.length text - ending - 4) in
      let headers =
        List.filter_map
          (fun line ->
            Option.map (fun (name, value) -> (String.lowercase_ascii (String.trim name), String.trim value)) (Base.String.lsplit2 line ~on:':'))
          lines
      in
      let%bind code =
        match String.split_on_char ' ' status with
        | version :: code :: _ when String.starts_with ~prefix:"HTTP/1." version ->
            Option.to_result ~none:(`Http1 (Malformed "a status that is not a number")) (int_of_string_opt code)
        | _ -> malformed "no status line"
      in
      let%bind framing, body =
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
                  | Some 0 -> Ok (`Framed, Buffer.contents buffer)
                  | Some size when size > 0 && line_end + 2 + size + 2 <= String.length rest ->
                      Buffer.add_substring buffer rest (line_end + 2) size;
                      chunks (line_end + 2 + size + 2)
                  | Some _ | None -> malformed "a chunk cut short or of no size")
            in
            chunks 0
        | _, Some length when length >= 0 && length <= String.length rest -> Ok (`Framed, String.sub rest 0 length)
        | _, Some _ -> malformed "a body shorter than its length"
        | _, None -> Ok (`Up_to_the_end, rest)
      in
      Ok (code, framing, body))

let body text =
  Result.bind (parse text) (fun (code, _, body) ->
      if code >= 200 && code < 300 then Ok body else Error (`Http1 (Refused { code; body })))

let request ({ timeout; security } : Http2.dial) ~host ~port request =
  let%bind transport = Quiver_mysql.Transport.connect ~timeout (Tcp { host; port }) in
  let received =
    let%bind () =
      match security with
      | Cleartext -> Ok ()
      | Secured context -> (
          let%bind trust = Quiver_mysql.Transport.secure transport ~host ~protocol:"http/1.1" context in
          match trust with `Trusted -> Ok () | `Unproven reason -> Error (`Http1 (Untrusted reason)))
    in
    let%bind () = Quiver_mysql.Transport.write transport (text ~host ~port request) in
    let chunk = Base_bigstring.create 65536 and buffer = Buffer.create 4096 in
    let rec read () =
      match Quiver_mysql.Transport.read transport chunk ~off:0 ~len:65536 with
      | 0 -> (
          let text = Buffer.contents buffer in
          match (Quiver_mysql.Transport.failure transport, parse text) with
          | None, _ | Some _, Ok (_, `Framed, _) -> Ok text
          | Some error, (Ok (_, `Up_to_the_end, _) | Error _) -> Error (`Transport error))
      | count ->
          Buffer.add_string buffer (Base_bigstring.To_string.sub chunk ~pos:0 ~len:count);
          read ()
    in
    read ()
  in
  Quiver_mysql.Transport.close transport;
  Result.bind received body
