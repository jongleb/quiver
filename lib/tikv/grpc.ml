open Base.Result.Let_syntax

type error =
  | Http of int
  | Not_grpc of string option
  | No_status
  | Status of { code : int; message : string }
  | Compressed
  | Framing of int
[@@deriving sexp_of ~portable]

let start connection ~path =
  Http2.start connection ~path ~headers:[ ("content-type", "application/grpc"); ("te", "trailers") ]

let send stream message =
  let length = String.length message in
  Http2.send stream
    [
      String.init 5 (fun i ->
          let byte = if i = 0 then 0 else (length lsr (8 * (4 - i))) land 0xFF in
          Char.chr byte);
      message;
    ]

let finish stream =
  let%bind response = Http2.finish stream in
  let field name = Option.fold ~none:(List.assoc_opt name response.headers) ~some:Option.some (List.assoc_opt name response.trailers) in
  let content_type = List.assoc_opt "content-type" response.headers in
  let percent_decoded text =
    let hex c = match c with '0' .. '9' -> Some (Char.code c - 48) | 'a' .. 'f' -> Some (Char.code c - 87) | 'A' .. 'F' -> Some (Char.code c - 55) | _ -> None in
    let buffer = Buffer.create (String.length text) in
    let rec go i =
      if i >= String.length text then Buffer.contents buffer
      else
        let escaped =
          if i + 2 < String.length text then
            Option.bind (hex text.[i + 1]) (fun high -> Option.map (fun low -> (16 * high) + low) (hex text.[i + 2]))
          else None
        in
        match (text.[i], escaped) with
        | '%', Some byte ->
            Buffer.add_char buffer (Char.chr byte);
            go (i + 3)
        | c, _ ->
            Buffer.add_char buffer c;
            go (i + 1)
    in
    go 0
  in
  if response.status <> 200 then Error (`Grpc (Http response.status))
  else
    match (content_type, Option.bind (field "grpc-status") int_of_string_opt) with
    | Some kind, _
      when not
             (kind = "application/grpc"
             || String.starts_with ~prefix:"application/grpc+" kind
             || String.starts_with ~prefix:"application/grpc;" kind) ->
        Error (`Grpc (Not_grpc content_type))
    | None, _ -> Error (`Grpc (Not_grpc content_type))
    | _, None -> Error (`Grpc No_status)
    | _, Some code when code <> 0 ->
        Error (`Grpc (Status { code; message = Option.fold ~none:"" ~some:percent_decoded (field "grpc-message") }))
    | _, Some _ ->
        let body = response.body in
        let size = String.length body in
        if size < 5 then Error (`Grpc (Framing size))
        else
          let declared = Base.String.foldi (String.sub body 1 4) ~init:0 ~f:(fun _ acc c -> (acc lsl 8) lor Char.code c) in
          if body.[0] <> '\x00' then Error (`Grpc Compressed)
          else if declared <> size - 5 then Error (`Grpc (Framing size))
          else Ok (String.sub body 5 declared)

let call connection ~path message =
  let%bind stream = start connection ~path in
  let%bind () = send stream message in
  finish stream
