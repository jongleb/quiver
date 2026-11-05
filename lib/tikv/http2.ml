open Base.Result.Let_syntax

type error =
  | Closed
  | Frame_size of { kind : int; length : int }
  | Unexpected of { kind : int; stream : int }
  | Setting of { id : int; value : int }
  | Window
  | Flow_control
  | Padding
  | Going_away of { last : int; code : int; debug : string }
  | Reset of int
  | No_status
  | Exhausted
  | Untrusted of string
  | Not_h2 of string option
[@@deriving sexp_of ~portable]

type security = Cleartext | Secured of Quiver_openssl.Tls.context
type dial = { timeout : Core.Time_ns.Span.t; security : security }

type t = {
  transport : Quiver_mysql.Transport.t;
  scheme : string;
  authority : string;
  input : Base_bigstring.t;
  mutable start : int;
  mutable stop : int;
  decoder : Hpack.decoder;
  mutable next_stream : int;
  mutable send_window : int;
  mutable stream_window : int;
  mutable initial_window : int;
  mutable max_frame : int;
  mutable received : int;
  mutable going_away : (int * int * string) option;
}

type response = {
  status : int;
  headers : (string * string) list;
  body : string;
  trailers : (string * string) list;
}

let receive_frame = 1 lsl 20
let stream_receive_window = 1 lsl 24
let connection_receive_window = 1 lsl 25

let header ~kind ~flags ~stream length =
  String.init 9 (fun i ->
      let byte =
        match i with
        | 0 -> (length lsr 16) land 0xFF
        | 1 -> (length lsr 8) land 0xFF
        | 2 -> length land 0xFF
        | 3 -> kind
        | 4 -> flags
        | i -> (stream lsr (8 * (8 - i))) land 0xFF
      in
      Char.chr byte)

let frame ~kind ~flags ~stream payload = header ~kind ~flags ~stream (String.length payload) ^ payload

let u32 n = String.init 4 (fun i -> Char.chr ((n lsr (8 * (3 - i))) land 0xFF))

let connect { timeout; security } ~host ~port =
  let%bind transport = Quiver_mysql.Transport.connect ~timeout (Tcp { host; port }) in
  let secured =
    match security with
    | Cleartext -> Ok "http"
    | Secured context -> (
        let%bind trust = Quiver_mysql.Transport.secure transport ~host context in
        match (trust, Quiver_mysql.Transport.protocol transport) with
        | `Unproven reason, _ -> Error (`Http2 (Untrusted reason))
        | `Trusted, Some "h2" -> Ok "https"
        | `Trusted, chosen -> Error (`Http2 (Not_h2 chosen)))
  in
  let settings =
    String.concat ""
      (List.map
         (fun (id, value) -> String.sub (u32 id) 2 2 ^ u32 value)
         [ (2, 0); (4, stream_receive_window); (5, receive_frame) ])
  in
  let preface =
    "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
    ^ frame ~kind:4 ~flags:0 ~stream:0 settings
    ^ frame ~kind:8 ~flags:0 ~stream:0 (u32 (connection_receive_window - 65_535))
  in
  match Result.bind secured (fun scheme -> Result.map (fun () -> scheme) (Quiver_mysql.Transport.write transport preface)) with
  | Error _ as error ->
      Quiver_mysql.Transport.close transport;
      error
  | Ok scheme ->
      Ok
        {
          transport;
          scheme;
          authority = host ^ ":" ^ Int.to_string port;
          input = Base_bigstring.create (receive_frame + 9);
          start = 0;
          stop = 0;
          decoder = Hpack.decoder ~limit:4096;
          next_stream = 1;
          send_window = 65_535;
          stream_window = 65_535;
          initial_window = 65_535;
          max_frame = 16_384;
          received = 0;
          going_away = None;
        }

let read_frame t =
  let rec fill count =
    if t.stop - t.start >= count then Ok ()
    else
      let () =
        if t.start > 0 then (
          Base_bigstring.blit ~src:t.input ~src_pos:t.start ~dst:t.input ~dst_pos:0 ~len:(t.stop - t.start);
          t.stop <- t.stop - t.start;
          t.start <- 0)
      in
      match
        Quiver_mysql.Transport.read t.transport t.input ~off:t.stop ~len:(Base_bigstring.length t.input - t.stop)
      with
      | 0 ->
          Error
            (Option.fold ~none:(`Http2 Closed)
               ~some:(fun error -> `Transport error)
               (Quiver_mysql.Transport.failure t.transport))
      | count_read ->
          t.stop <- t.stop + count_read;
          fill count
  in
  let%bind () = fill 9 in
  let byte i = Char.code (Base_bigstring.get t.input (t.start + i)) in
  let length = (byte 0 lsl 16) lor (byte 1 lsl 8) lor byte 2 and kind = byte 3 and flags = byte 4 in
  let stream = ((byte 5 land 0x7F) lsl 24) lor (byte 6 lsl 16) lor (byte 7 lsl 8) lor byte 8 in
  if length > receive_frame then Error (`Http2 (Frame_size { kind; length }))
  else
    let%bind () = fill (9 + length) in
    let payload = Base_bigstring.To_string.sub t.input ~pos:(t.start + 9) ~len:length in
    t.start <- t.start + 9 + length;
    Ok (kind, flags, stream, payload)

let largest_window = (1 lsl 31) - 1
let int_at text pos width = Base.String.foldi (String.sub text pos width) ~init:0 ~f:(fun _ acc c -> (acc lsl 8) lor Char.code c)

type stream = { connection : t; id : int; mutable answered : response option }

let other t ~stream ~kind ~flags ~frame_stream payload =
  let length = String.length payload in
  let connection_frame () =
    match kind with
    | 4 when frame_stream <> 0 -> Error (`Http2 (Unexpected { kind; stream = frame_stream }))
    | 4 when flags land 1 = 1 -> if length = 0 then Ok () else Error (`Http2 (Frame_size { kind; length }))
    | 4 ->
        if length mod 6 <> 0 then Error (`Http2 (Frame_size { kind; length }))
        else
          let%bind () =
            Base.List.fold_result
              (List.init (length / 6) (fun i -> (int_at payload (6 * i) 2, int_at payload ((6 * i) + 2) 4)))
              ~init:()
              ~f:(fun () (id, value) ->
                match id with
                | 2 when value > 1 -> Error (`Http2 (Setting { id; value }))
                | 4 when value > largest_window -> Error (`Http2 (Setting { id; value }))
                | 4 when t.stream_window + value - t.initial_window > largest_window -> Error (`Http2 Window)
                | 4 ->
                    t.stream_window <- t.stream_window + value - t.initial_window;
                    t.initial_window <- value;
                    Ok ()
                | 5 when value < 16_384 || value > (1 lsl 24) - 1 -> Error (`Http2 (Setting { id; value }))
                | 5 ->
                    t.max_frame <- value;
                    Ok ()
                | _ -> Ok ())
          in
          Quiver_mysql.Transport.write t.transport (frame ~kind:4 ~flags:1 ~stream:0 "")
    | 6 when frame_stream <> 0 -> Error (`Http2 (Unexpected { kind; stream = frame_stream }))
    | 6 when length <> 8 -> Error (`Http2 (Frame_size { kind; length }))
    | 6 -> if flags land 1 = 1 then Ok () else Quiver_mysql.Transport.write t.transport (frame ~kind:6 ~flags:1 ~stream:0 payload)
    | 7 when frame_stream <> 0 -> Error (`Http2 (Unexpected { kind; stream = frame_stream }))
    | 7 when length < 8 -> Error (`Http2 (Frame_size { kind; length }))
    | 7 ->
        t.going_away <- Some (int_at payload 0 4 land 0x7FFF_FFFF, int_at payload 4 4, String.sub payload 8 (length - 8));
        Ok ()
    | 8 when length <> 4 -> Error (`Http2 (Frame_size { kind; length }))
    | 8 ->
        let increment = int_at payload 0 4 land 0x7FFF_FFFF in
        if increment = 0 then Error (`Http2 Window)
        else if frame_stream = 0 then
          if t.send_window + increment > largest_window then Error (`Http2 Window)
          else (
            t.send_window <- t.send_window + increment;
            Ok ())
        else if frame_stream = stream then
          if t.stream_window + increment > largest_window then Error (`Http2 Window)
          else (
            t.stream_window <- t.stream_window + increment;
            Ok ())
        else Ok ()
    | 5 | 9 -> Error (`Http2 (Unexpected { kind; stream = frame_stream }))
    | _ -> Ok ()
  in
  match kind with
  | 0 | 1 | 9 -> Error (`Http2 (Unexpected { kind; stream = frame_stream }))
  | 3 when frame_stream = 0 -> Error (`Http2 (Unexpected { kind; stream = frame_stream }))
  | 3 -> Ok ()
  | _ -> (
      let%bind () = connection_frame () in
      match t.going_away with
      | Some (last, code, debug) when last < stream -> Error (`Http2 (Going_away { last; code; debug }))
      | Some _ | None -> Ok ())

let response t ~stream first =
  let unpadded ~flags ~extra payload =
    if flags land 0x8 = 0 then
      if String.length payload < extra then Error (`Http2 Padding)
      else if extra = 0 then Ok payload
      else Ok (String.sub payload extra (String.length payload - extra))
    else if String.length payload = 0 then Error (`Http2 Padding)
    else
      let padding = Char.code payload.[0] in
      let length = String.length payload - 1 - extra - padding in
      if length < 0 then Error (`Http2 Padding) else Ok (String.sub payload (1 + extra) length)
  in
  let body = ref [] in
  let whole () = match !body with [ only ] -> only | chunks -> String.concat "" (List.rev chunks) in
  let rec go (kind, flags, frame_stream, payload) ~status ~headers ~unacknowledged =
    let next ~status ~headers ~unacknowledged =
      let%bind frame = read_frame t in
      go frame ~status ~headers ~unacknowledged
    in
    match kind with
    | 0 when frame_stream = stream ->
        let length = String.length payload in
        t.received <- t.received + length;
        let unacknowledged = unacknowledged + length in
        if t.received > connection_receive_window || unacknowledged > stream_receive_window then Error (`Http2 Flow_control)
        else
          let%bind data = unpadded ~flags ~extra:0 payload in
          let () = body := data :: !body in
          let%bind () =
            if t.received >= connection_receive_window / 2 then (
              let increment = t.received in
              t.received <- 0;
              Quiver_mysql.Transport.write t.transport (frame ~kind:8 ~flags:0 ~stream:0 (u32 increment)))
            else Ok ()
          in
          if flags land 1 = 1 then
            Option.fold ~none:(Error (`Http2 No_status))
              ~some:(fun status -> Ok { status; headers; body = whole (); trailers = [] })
              status
          else if unacknowledged >= stream_receive_window / 2 then
            let%bind () = Quiver_mysql.Transport.write t.transport (frame ~kind:8 ~flags:0 ~stream (u32 unacknowledged)) in
            next ~status ~headers ~unacknowledged:0
          else next ~status ~headers ~unacknowledged
    | 1 when frame_stream = stream -> (
        let extra = if flags land 0x20 = 0 then 0 else 5 in
        let%bind first = unpadded ~flags ~extra payload in
        let rec continued fragments flags =
          if flags land 0x4 <> 0 then Ok (String.concat "" (List.rev fragments))
          else
            let%bind kind, next_flags, frame_stream, payload = read_frame t in
            if kind <> 9 || frame_stream <> stream then Error (`Http2 (Unexpected { kind; stream = frame_stream }))
            else continued (payload :: fragments) next_flags
        in
        let%bind block = continued [ first ] flags in
        let%bind fields = Hpack.decode t.decoder block in
        let ended = flags land 1 = 1 in
        let code = Option.bind (List.assoc_opt ":status" fields) int_of_string_opt in
        match (status, code) with
        | None, Some code when code >= 100 && code < 200 && not ended -> next ~status ~headers ~unacknowledged
        | None, None -> Error (`Http2 No_status)
        | None, Some code ->
            let headers = List.filter (fun (name, _) -> name <> ":status") fields in
            if ended then Ok { status = code; headers; body = ""; trailers = [] }
            else next ~status:(Some code) ~headers ~unacknowledged
        | Some code, _ ->
            if ended then Ok { status = code; headers; body = whole (); trailers = fields }
            else Error (`Http2 (Unexpected { kind; stream })))
    | 3 when frame_stream = stream ->
        if String.length payload <> 4 then Error (`Http2 (Frame_size { kind = 3; length = String.length payload }))
        else Error (`Http2 (Reset (int_at payload 0 4)))
    | _ ->
        let%bind () = other t ~stream ~kind ~flags ~frame_stream payload in
        next ~status ~headers ~unacknowledged
  in
  go first ~status:None ~headers:[] ~unacknowledged:0

let start t ~path ~headers =
  let id = t.next_stream in
  let%bind () =
    match t.going_away with
    | Some (last, code, debug) -> Error (`Http2 (Going_away { last; code; debug }))
    | None -> if id > largest_window then Error (`Http2 Exhausted) else Ok ()
  in
  t.next_stream <- id + 2;
  t.stream_window <- t.initial_window;
  let block =
    let buffer = Buffer.create 256 in
    Hpack.encode buffer
      ([ (":method", "POST"); (":scheme", t.scheme); (":path", path); (":authority", t.authority) ] @ headers);
    Buffer.contents buffer
  in
  let fragments =
    List.init
      (Int.max 1 ((String.length block + t.max_frame - 1) / t.max_frame))
      (fun i -> String.sub block (i * t.max_frame) (Int.min t.max_frame (String.length block - (i * t.max_frame))))
  in
  let count = List.length fragments in
  let frames =
    List.mapi
      (fun i piece ->
        let last = if i = count - 1 then 0x4 else 0 in
        if i = 0 then frame ~kind:1 ~flags:last ~stream:id piece else frame ~kind:9 ~flags:last ~stream:id piece)
      fragments
  in
  let%bind () = Quiver_mysql.Transport.write t.transport (String.concat "" frames) in
  Ok { connection = t; id; answered = None }

let send stream pieces =
  let t = stream.connection in
  let rec go pieces pos =
    match (stream.answered, pieces) with
    | Some _, _ | None, [] -> Ok ()
    | None, text :: rest when pos >= String.length text -> go rest 0
    | None, _ :: _ ->
        let room = Int.min t.max_frame (Int.min t.send_window t.stream_window) in
        if room <= 0 then
          let%bind ((kind, flags, frame_stream, payload) as incoming) = read_frame t in
          match kind with
          | (0 | 1 | 3) when frame_stream = stream.id ->
              let%bind answer = response t ~stream:stream.id incoming in
              stream.answered <- Some answer;
              Quiver_mysql.Transport.write t.transport (frame ~kind:3 ~flags:0 ~stream:stream.id (u32 0))
          | _ ->
              let%bind () = other t ~stream:stream.id ~kind ~flags ~frame_stream payload in
              go pieces pos
        else
          let rec slices pieces pos room acc =
            match pieces with
            | text :: rest when room > 0 ->
                let length = Int.min room (String.length text - pos) in
                if pos + length >= String.length text then slices rest 0 (room - length) ((text, pos, length) :: acc)
                else (List.rev ((text, pos, length) :: acc), pieces, pos + length)
            | _ -> (List.rev acc, pieces, pos)
          in
          let taken, rest, next = slices pieces pos room [] in
          let length = List.fold_left (fun total (_, _, length) -> total + length) 0 taken in
          t.send_window <- t.send_window - length;
          t.stream_window <- t.stream_window - length;
          let%bind () = Quiver_mysql.Transport.write t.transport (header ~kind:0 ~flags:0 ~stream:stream.id length) in
          let%bind () =
            Base.List.fold_result taken ~init:() ~f:(fun () (text, pos, len) -> Quiver_mysql.Transport.write t.transport ~pos ~len text)
          in
          go rest next
  in
  go pieces 0

let finish stream =
  let t = stream.connection in
  match stream.answered with
  | Some answer -> Ok answer
  | None ->
      let%bind () = Quiver_mysql.Transport.write t.transport (frame ~kind:0 ~flags:1 ~stream:stream.id "") in
      Result.bind (read_frame t) (response t ~stream:stream.id)

let request t ~path ~headers body =
  let%bind stream = start t ~path ~headers in
  let%bind () = send stream [ body ] in
  finish stream

let close t = Quiver_mysql.Transport.close t.transport
