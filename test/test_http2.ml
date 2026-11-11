open Sexplib0.Sexp_conv
open Quiver_tikv

let frame ~kind ~flags ~stream payload =
  let length = String.length payload in
  let byte n = String.make 1 (Char.chr (n land 0xFF)) in
  String.concat ""
    [ byte (length lsr 16); byte (length lsr 8); byte length; byte kind; byte flags; byte (stream lsr 24); byte (stream lsr 16); byte (stream lsr 8); byte stream; payload ]

let u32 n = String.init 4 (fun i -> Char.chr ((n lsr (8 * (3 - i))) land 0xFF))
let number text = Base.String.fold text ~init:0 ~f:(fun acc c -> (acc lsl 8) lor Char.code c)

let rec write fd text pos =
  if pos < String.length text then write fd text (pos + Unix.write_substring fd text pos (String.length text - pos))

let read_exact fd count =
  let bytes = Bytes.create count in
  let rec go pos =
    if pos = count then Some (Bytes.to_string bytes)
    else match Unix.read fd bytes pos (count - pos) with 0 -> None | n -> go (pos + n) | exception Unix.Unix_error _ -> None
  in
  go 0

let read_frame fd =
  Option.bind (read_exact fd 9) (fun header ->
      let length = number (String.sub header 0 3) in
      Option.map
        (fun payload -> (Char.code header.[3], Char.code header.[4], number (String.sub header 5 4) land 0x7FFF_FFFF, payload))
        (read_exact fd length))

type noise =
  | Ping of string
  | Unknown of string
  | Priority
  | Connection_window of int
[@@deriving sexp_of]

type piece = { noise : noise list; text : string; padding : int option } [@@deriving sexp_of]
type opening = Informational | Direct [@@deriving sexp_of]

(* What the first frame of the response's headers carries besides its fragment. *)
type marks = Bare | Padded of int | Prioritized | Padded_prioritized of int [@@deriving sexp_of]

type reply = {
  opening : opening;
  status : int;
  fields : Test_hpack.choice list;
  fragments : int;
  marks : marks;
  pieces : piece list;
  trailers : Test_hpack.choice list option;
}
[@@deriving sexp_of]

type exchange = { path : string; headers : (string * string) list; body : string; reply : reply } [@@deriving sexp_of]
type scenario = { initial_window : int; max_frame : int; exchanges : exchange list } [@@deriving sexp_of]

(* What the fake server saw the client do wrong, and the requests it got. *)
type heard = { faults : string list; requests : ((string * string) list * string) list; largest : int }

let serve listener scenario =
  match Quiver_core.Blocking.wait ~seconds:5. Readable listener with
  | Timed_out -> { faults = [ "no client" ]; requests = []; largest = 0 }
  | Ready ->
  let client, _ = Unix.accept listener in
  let faults = ref [] and requests = ref [] and largest = ref 0 in
  let fault text = faults := text :: !faults in
  let decoder = Hpack.decoder ~limit:4096 and table = { Test_hpack.max = 4096; entries = [||] } in
  let acked = ref false and pings = ref [] in
  let send_window = ref 65_535 and stream_windows = Hashtbl.create 4 in
  let client_window = ref 65_535 and client_stream = ref 65_535 and client_initial = ref 65_535 in
  let base () = if !acked then scenario.initial_window else 65_535 in
  let handle (kind, flags, stream, payload) =
    match kind with
    | 4 when flags land 1 = 1 ->
        acked := true;
        Hashtbl.filter_map_inplace (fun _ window -> Some (window + scenario.initial_window - 65_535)) stream_windows
    | 4 ->
        List.iter
          (fun i -> if number (String.sub payload (6 * i) 2) = 4 then client_initial := number (String.sub payload ((6 * i) + 2) 4))
          (List.init (String.length payload / 6) Fun.id);
        write client (frame ~kind:4 ~flags:1 ~stream:0 "") 0
    | 6 when flags land 1 = 1 ->
        if List.mem payload !pings then pings := List.filter (fun ping -> ping <> payload) !pings else fault "an unknown PING acknowledged"
    | 6 -> fault "the client pinged"
    | 8 when stream = 0 -> client_window := !client_window + number payload
    | 8 -> client_stream := !client_stream + number payload
    | 3 -> ()
    | _ -> fault (Printf.sprintf "frame %d on stream %d" kind stream)
  in
  let refill ~stream =
    let chunk = Int.max 4096 scenario.initial_window in
    let starved =
      !send_window <= 0 || Option.fold ~none:false ~some:(fun window -> window <= 0) (Hashtbl.find_opt stream_windows stream)
    in
    if starved then
      match Quiver_core.Blocking.wait ~seconds:0.005 Readable client with
      | Ready -> ()
      | Timed_out ->
    let () =
      if !send_window <= 0 then (
        write client (frame ~kind:8 ~flags:0 ~stream:0 (u32 (chunk - !send_window))) 0;
        send_window := chunk)
    in
    Option.iter
      (fun window ->
        if window <= 0 then (
          write client (frame ~kind:8 ~flags:0 ~stream (u32 (chunk - window))) 0;
          Hashtbl.replace stream_windows stream chunk))
      (Hashtbl.find_opt stream_windows stream)
  in
  let rec request stream headers body =
    refill ~stream;
    match read_frame client with
    | None ->
        fault "closed inside a request";
        None
    | Some (1, flags, frame_stream, payload) when frame_stream = stream ->
        let rec continued block flags =
          if flags land 4 <> 0 then Some block
          else
            match read_frame client with
            | Some (9, flags, s, more) when s = stream -> continued (block ^ more) flags
            | _ ->
                fault "no CONTINUATION";
                None
        in
        Option.bind (continued payload flags) (fun block ->
            match Hpack.decode decoder block with
            | Error _ ->
                fault "a broken header block";
                None
            | Ok fields -> if flags land 1 = 1 then Some (fields, body) else request stream fields body)
    | Some (0, flags, frame_stream, payload) when frame_stream = stream ->
        let length = String.length payload and window = Hashtbl.find stream_windows stream in
        if length > Int.max 0 !send_window || length > Int.max 0 window then fault "data past the window";
        if length > if !acked then scenario.max_frame else 16_384 then fault "a frame past the size";
        send_window := !send_window - length;
        largest := Int.max !largest length;
        if flags land 1 = 1 then (
          Hashtbl.remove stream_windows stream;
          Some (headers, body ^ payload))
        else (
          Hashtbl.replace stream_windows stream (window - length);
          request stream headers (body ^ payload))
    | Some other ->
        handle other;
        request stream headers body
  in
  let encode choices = Test_hpack.encode table { Test_hpack.updates = []; fields = choices } in
  let headers_frames ?(marks = Bare) ~stream ~ended ~fragments block =
    let size = Int.max 1 ((String.length block + fragments - 1) / fragments) in
    String.concat ""
      (List.init fragments (fun i ->
           let pos = Int.min (String.length block) (i * size) in
           let piece = String.sub block pos (Int.min size (String.length block - pos)) in
           let flags = (if i = fragments - 1 then 4 else 0) lor if i = 0 && ended then 1 else 0 in
           if i > 0 then frame ~kind:9 ~flags ~stream piece
           else
             let priority = "\x80\x00\x00\x03\x10" in
             match marks with
             | Bare -> frame ~kind:1 ~flags ~stream piece
             | Padded padding -> frame ~kind:1 ~flags:(flags lor 8) ~stream (String.make 1 (Char.chr padding) ^ piece ^ String.make padding '\x00')
             | Prioritized -> frame ~kind:1 ~flags:(flags lor 0x20) ~stream (priority ^ piece)
             | Padded_prioritized padding ->
                 frame ~kind:1 ~flags:(flags lor 0x28) ~stream (String.make 1 (Char.chr padding) ^ priority ^ piece ^ String.make padding '\x01')))
  in
  let rec give stream text =
    let room = Int.min !client_window !client_stream in
    if String.length text <= room then (
      client_window := !client_window - String.length text;
      client_stream := !client_stream - String.length text;
      true)
    else
      match read_frame client with
      | Some other ->
          handle other;
          give stream text
      | None -> false
  in
  let noise stream = function
    | Ping payload ->
        pings := payload :: !pings;
        write client (frame ~kind:6 ~flags:0 ~stream:0 payload) 0
    | Unknown payload -> write client (frame ~kind:0xfa ~flags:0 ~stream payload) 0
    | Priority -> write client (frame ~kind:2 ~flags:0 ~stream "\x00\x00\x00\x00\x10") 0
    | Connection_window increment ->
        send_window := !send_window + increment;
        write client (frame ~kind:8 ~flags:0 ~stream:0 (u32 increment)) 0
  in
  let respond stream reply =
    let () =
      match reply.opening with
      | Informational -> write client (headers_frames ~stream ~ended:false ~fragments:1 (encode [ { Test_hpack.header = (":status", "100"); kind = Without; name_from = By_index; name_coding = Raw; value_coding = Raw } ])) 0
      | Direct -> ()
    in
    let status : Test_hpack.choice =
      { header = (":status", Int.to_string reply.status); kind = Incremental; name_from = By_index; name_coding = Raw; value_coding = Huffman }
    in
    let only = reply.pieces = [] && reply.trailers = None in
    write client (headers_frames ~marks:reply.marks ~stream ~ended:only ~fragments:reply.fragments (encode (status :: reply.fields))) 0;
    let count = List.length reply.pieces in
    List.iteri
      (fun i piece ->
        List.iter (noise stream) piece.noise;
        let ended = i = count - 1 && reply.trailers = None in
        let payload, flags =
          match piece.padding with
          | None -> (piece.text, 0)
          | Some padding -> (String.make 1 (Char.chr padding) ^ piece.text ^ String.make padding '\x00', 8)
        in
        if give stream payload then write client (frame ~kind:0 ~flags:(flags lor if ended then 1 else 0) ~stream payload) 0)
      reply.pieces;
    Option.iter (fun trailers -> write client (headers_frames ~stream ~ended:true ~fragments:1 (encode trailers)) 0) reply.trailers
  in
  let () =
    match read_exact client 24 with
    | Some "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" ->
        write client
          (frame ~kind:4 ~flags:0 ~stream:0 ("\x00\x04" ^ u32 scenario.initial_window ^ "\x00\x05" ^ u32 scenario.max_frame))
          0;
        List.iteri
          (fun i exchange ->
            let stream = (2 * i) + 1 in
            Hashtbl.replace stream_windows stream (base ());
            match request stream [] "" with
            | Some received ->
                client_stream := !client_initial;
                requests := received :: !requests;
                respond stream exchange.reply
            | None -> ())
          scenario.exchanges
    | Some _ | None -> fault "no preface"
  in
  let rec drain () = match read_frame client with Some other -> handle other; drain () | None -> () in
  drain ();
  if not !acked then fault "settings never acknowledged";
  if !pings <> [] then fault "a PING unanswered";
  Unix.close client;
  { faults = List.rev !faults; requests = List.rev !requests; largest = !largest }

let listening () =
  let listener = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
  Unix.setsockopt listener SO_REUSEADDR true;
  Unix.bind listener Support.loopback;
  Unix.listen listener 1;
  match Unix.getsockname listener with ADDR_INET (_, port) -> (listener, port) | ADDR_UNIX _ -> Alcotest.fail "not TCP"

let session scenario client =
  let listener, port = listening () in
  let heard = Support.start (fun () -> serve listener scenario) in
  let result =
    Result.bind (Http2.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } ~host:"127.0.0.1" ~port) (fun connection ->
        let result = client connection in
        Http2.close connection;
        result)
  in
  let heard = Support.join heard in
  Unix.close listener;
  (result, heard, port)

let show = function
  | `Http2 error -> Base.Sexp.to_string (Http2.sexp_of_error error)
  | `Hpack error -> Base.Sexp.to_string (Hpack.sexp_of_error error)
  | `Transport error -> Base.Sexp.to_string (Quiver_mysql.Transport.sexp_of_error error)

let fields choices = List.map (fun (choice : Test_hpack.choice) -> choice.header) choices

let gen =
  QCheck2.Gen.(
    let choice header =
      let* kind = oneof_list Test_hpack.[ Indexed; Incremental; Without; Never ]
      and* name_from = oneof_list Test_hpack.[ By_index; Literal_name ]
      and* name_coding = oneof_list Test_hpack.[ Huffman; Raw ]
      and* value_coding = oneof_list Test_hpack.[ Huffman; Raw ] in
      return { Test_hpack.header; kind; name_from; name_coding; value_coding }
    in
    let name = oneof_list [ "content-type"; "grpc-status"; "grpc-message"; "x-a"; "server" ] in
    let field = bind (pair name (string_size ~gen:printable (int_range 0 30))) choice in
    let noise =
      oneof
        [
          map (fun text -> Ping text) (string_size ~gen:char (return 8));
          map (fun text -> Unknown text) (string_size ~gen:char (int_range 0 20));
          return Priority;
          map (fun n -> Connection_window n) (int_range 1 1000);
        ]
    in
    let piece =
      let* noise = list_size (int_range 0 2) noise
      and* text = no_shrink (string_size ~gen:char (int_range 0 3000))
      and* padding = option (int_range 0 255) in
      return { noise; text; padding }
    in
    let* initial_window = oneof_list [ 1; 100; 16_384; 65_535; 1 lsl 20 ]
    and* max_frame = oneof_list [ 16_384; 20_000; 1 lsl 20 ] in
    let largest = Int.min 200_000 ((initial_window * 30) + 2000) in
    let exchange =
      let* path = oneof_list [ "/tikvpb.Tikv/KvScan"; "/a"; "/x/y" ]
      and* headers = list_size (int_range 0 4) (pair name (string_size ~gen:printable (int_range 0 40)))
      and* body = oneof [ return ""; no_shrink (string_size ~gen:char (int_range 1 largest)) ]
      and* opening = oneof_list [ Direct; Direct; Informational ]
      and* status = oneof_list [ 200; 404 ]
      and* reply_fields = list_size (int_range 0 5) field
      and* fragments = int_range 1 4
      and* marks = oneof [ return Bare; map (fun n -> Padded n) (int_range 0 255); return Prioritized; map (fun n -> Padded_prioritized n) (int_range 0 255) ]
      and* pieces = list_size (int_range 0 4) piece
      and* trailers = option (list_size (int_range 0 3) field) in
      return { path; headers; body; reply = { opening; status; fields = reply_fields; fragments; marks; pieces; trailers } }
    in
    let* exchanges = list_size (int_range 1 3) exchange in
    return { initial_window; max_frame; exchanges })

let exchanged =
  QCheck2.Test.make ~count:300
    ~name:
      "requests reach the server whole within its windows and frame size, and responses come back \
       whole through continuations, padding, informational headers, trailers, pings and frames to \
       ignore, several on one connection"
    ~print:(fun scenario -> Base.Sexp.to_string_hum (sexp_of_scenario scenario))
    gen
    (fun scenario ->
      let result, heard, port =
        session scenario (fun connection ->
            Base.List.fold_result scenario.exchanges ~init:[] ~f:(fun acc exchange ->
                let pieces =
                  let rec cut pos sizes acc =
                    if pos >= String.length exchange.body then List.rev acc
                    else
                      match sizes with
                      | size :: rest ->
                          let length = Int.min size (String.length exchange.body - pos) in
                          cut (pos + length) (rest @ [ size ]) (String.sub exchange.body pos length :: "" :: acc)
                      | [] -> List.rev acc
                  in
                  cut 0 [ 1; 7; 16_385; 3; 40_000 ] []
                in
                let sent =
                  Result.bind (Http2.start connection ~path:exchange.path ~headers:exchange.headers) (fun stream ->
                      Result.bind
                        (Base.List.fold_result (Base.List.chunks_of pieces ~length:3) ~init:() ~f:(fun () pieces -> Http2.send stream pieces))
                        (fun () -> Http2.finish stream))
                in
                Result.map (fun response -> response :: acc) sent))
      in
      let expected_requests =
        List.map
          (fun exchange ->
            ( [ (":method", "POST"); (":scheme", "http"); (":path", exchange.path); (":authority", "127.0.0.1:" ^ Int.to_string port) ] @ exchange.headers,
              exchange.body ))
          scenario.exchanges
      in
      match result with
      | Error error -> QCheck2.Test.fail_reportf "%s; faults: %s" (show error) (String.concat ", " heard.faults)
      | Ok responses ->
          let expected =
            List.map
              (fun exchange ->
                let reply = exchange.reply in
                ( reply.status,
                  fields reply.fields,
                  String.concat "" (List.map (fun piece -> piece.text) reply.pieces),
                  Option.fold ~none:[] ~some:fields reply.trailers ))
              scenario.exchanges
          in
          let got = List.rev_map (fun (response : Http2.response) -> (response.status, response.headers, response.body, response.trailers)) responses in
          if heard.faults <> [] then QCheck2.Test.fail_reportf "faults: %s" (String.concat ", " heard.faults)
          else if heard.requests <> expected_requests then QCheck2.Test.fail_report "the server got other requests"
          else got = expected)

let raw ~settings frames =
  let listener, port = listening () in
  let heard =
    Support.start (fun () ->
        match Quiver_core.Blocking.wait ~seconds:5. Readable listener with
        | Timed_out -> []
        | Ready ->
            let client, _ = Unix.accept listener in
            ignore (read_exact client 24);
            write client (frame ~kind:4 ~flags:0 ~stream:0 settings) 0;
            let rec request () =
              match read_frame client with Some (1, _, _, _) | None -> () | Some _ -> request ()
            in
            request ();
            List.iter (fun text -> try write client text 0 with Unix.Unix_error _ -> ()) frames;
            let rec after acc = match read_frame client with Some (kind, _, stream, _) -> after ((kind, stream) :: acc) | None -> List.rev acc in
            let heard = if frames = [] then [] else after [] in
            Unix.close client;
            heard)
  in
  (port, fun () -> Support.join heard)

let block fields =
  let buffer = Buffer.create 64 in
  Hpack.encode buffer fields;
  Buffer.contents buffer

let refusals () =
  let outcome ?(settings = "") ?(body = "") ?(next = `Alone) frames =
    let port, heard = raw ~settings frames in
    let result =
      Result.bind (Http2.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext } ~host:"127.0.0.1" ~port) (fun connection ->
          let first = Http2.request connection ~path:"/p" ~headers:[] body in
          let second =
            match next with
            | `Second -> Result.bind first (fun _ -> Http2.request connection ~path:"/p" ~headers:[] "")
            | `Alone -> first
          in
          Http2.close connection;
          Ok (first, second))
    in
    (result, heard ())
  in
  let refused name expected ?settings ?body frames =
    match outcome ?settings ?body frames with
    | Ok (Error (`Http2 error), _), _ when error = expected -> ()
    | Ok (Error error, _), _ -> Alcotest.failf "%s: %s" name (show error)
    | Ok (Ok _, _), _ -> Alcotest.failf "%s: answered" name
    | Error error, _ -> Alcotest.failf "%s: %s" name (show error)
  in
  let ok = frame ~kind:1 ~flags:5 ~stream:1 (block [ (":status", "200") ]) in
  refused "reset" (Reset 7) [ frame ~kind:3 ~flags:0 ~stream:1 (u32 7) ];
  refused "going away before the stream" (Going_away { last = 0; code = 2; debug = "bye" })
    [ frame ~kind:7 ~flags:0 ~stream:0 (u32 0 ^ u32 2 ^ "bye") ];
  let () =
    match outcome [] with
    | Ok (Error (`Http2 Closed | `Transport _), _), _ -> ()
    | _ -> Alcotest.fail "closed: answered or another failure"
  in
  refused "a window past 2^31 - 1" (Setting { id = 4; value = 1 lsl 31 }) ~settings:("\x00\x04" ^ u32 (1 lsl 31)) [ ok ];
  refused "a frame size below 16384" (Setting { id = 5; value = 16_383 }) ~settings:("\x00\x05" ^ u32 16_383) [ ok ];
  refused "a frame size past 2^24 - 1" (Setting { id = 5; value = 1 lsl 24 }) ~settings:("\x00\x05" ^ u32 (1 lsl 24)) [ ok ];
  refused "push enabled by the server" (Setting { id = 2; value = 2 }) ~settings:("\x00\x02" ^ u32 2) [ ok ];
  refused "settings of a broken length" (Frame_size { kind = 4; length = 5 }) ~settings:"\x00\x04\x00\x00\x01" [ ok ];
  refused "a ping of seven bytes" (Frame_size { kind = 6; length = 7 }) [ frame ~kind:6 ~flags:0 ~stream:0 "1234567"; ok ];
  refused "a window update of 0" Window [ frame ~kind:8 ~flags:0 ~stream:0 (u32 0); ok ];
  refused "a connection window past 2^31 - 1" Window [ frame ~kind:8 ~flags:0 ~stream:0 (u32 ((1 lsl 31) - 1)); ok ];
  refused "a push promise" (Unexpected { kind = 5; stream = 1 }) [ frame ~kind:5 ~flags:4 ~stream:1 (u32 2 ^ block [ (":method", "GET") ]) ];
  refused "data on stream 0" (Unexpected { kind = 0; stream = 0 }) [ frame ~kind:0 ~flags:0 ~stream:0 "x"; ok ];
  refused "a continuation alone" (Unexpected { kind = 9; stream = 1 }) [ frame ~kind:9 ~flags:4 ~stream:1 ""; ok ];
  refused "headers interrupted" (Unexpected { kind = 0; stream = 1 })
    [ frame ~kind:1 ~flags:0 ~stream:1 (block [ (":status", "200") ]); frame ~kind:0 ~flags:1 ~stream:1 "" ];
  refused "padding past the frame" Padding
    [ frame ~kind:1 ~flags:4 ~stream:1 (block [ (":status", "200") ]); frame ~kind:0 ~flags:9 ~stream:1 "\x05abc" ];
  refused "no status" No_status [ frame ~kind:1 ~flags:5 ~stream:1 (block [ ("x", "y") ]) ];
  refused "a frame past a mebibyte" (Frame_size { kind = 0; length = (1 lsl 20) + 1 })
    [ frame ~kind:1 ~flags:4 ~stream:1 (block [ (":status", "200") ]); frame ~kind:0 ~flags:1 ~stream:1 (String.make ((1 lsl 20) + 1) 'x') ];
  let () =
    match outcome ~next:`Second [ frame ~kind:7 ~flags:0 ~stream:0 (u32 1 ^ u32 0); ok ] with
    | Ok (Ok response, Error (`Http2 (Going_away { last = 1; code = 0; debug = "" }))), _ -> Alcotest.(check int) "status" 200 response.status
    | _ -> Alcotest.fail "a stream the server still serves answers, the next one is refused"
  in
  match outcome ~settings:("\x00\x04" ^ u32 10) ~body:(String.make 100_000 'b') [ frame ~kind:1 ~flags:5 ~stream:1 (block [ (":status", "200"); ("grpc-status", "8") ]) ] with
  | Ok (Ok response, _), heard ->
      Alcotest.(check (list (pair string string))) "an answer before the body went whole" [ ("grpc-status", "8") ] response.headers;
      Alcotest.(check bool) "the stream reset by the client" true (List.mem (3, 1) heard)
  | _ -> Alcotest.fail "no early answer"

let large () =
  let pieces = List.init 2560 (fun i -> { noise = []; text = String.make 16_384 (Char.chr (i mod 256)); padding = None }) in
  let scenario =
    {
      initial_window = 65_535;
      max_frame = 16_384;
      exchanges =
        [ { path = "/p"; headers = []; body = ""; reply = { opening = Direct; status = 200; fields = []; fragments = 1; marks = Bare; pieces; trailers = None } } ];
    }
  in
  let result, heard, _ = session scenario (fun connection -> Http2.request connection ~path:"/p" ~headers:[] "") in
  Alcotest.(check (list string)) "faults" [] heard.faults;
  Alcotest.(check bool) "forty mebibytes through both windows" true
    (match result with Ok response -> response.body = String.concat "" (List.map (fun piece -> piece.text) pieces) | Error _ -> false)

let wide () =
  let exchange : exchange =
    { path = "/p"; headers = []; body = String.make 100_000 'w'; reply = { opening = Direct; status = 200; fields = []; fragments = 1; marks = Bare; pieces = []; trailers = None } }
  in
  let scenario =
    { initial_window = 1 lsl 20; max_frame = 1 lsl 20; exchanges = [ { exchange with headers = [ ("x-long", String.make 40_000 'h') ] }; exchange ] }
  in
  let result, heard, _ =
    session scenario (fun connection ->
        Base.List.fold_result scenario.exchanges ~init:() ~f:(fun () exchange ->
            Result.map ignore (Http2.request connection ~path:exchange.path ~headers:exchange.headers exchange.body)))
  in
  Alcotest.(check (list string)) "faults" [] heard.faults;
  Alcotest.(check (list (pair string string))) "a header block in continuations" [ ("x-long", String.make 40_000 'h') ]
    (List.filter (fun (name, _) -> name = "x-long") (fst (List.hd heard.requests)));
  Alcotest.(check bool) "answered" true (Result.is_ok result);
  Alcotest.(check int) "a body in one frame once the server's settings are known" 100_000 heard.largest

let secured () =
  let exchange : exchange =
    {
      path = "/s";
      headers = [];
      body = "ping";
      reply =
        {
          opening = Direct;
          status = 200;
          fields = [];
          fragments = 1;
          marks = Bare;
          pieces = [ { noise = []; text = "pong"; padding = None } ];
          trailers = None;
        };
    }
  in
  let scenario = { initial_window = 65_535; max_frame = 16_384; exchanges = [ exchange ] } in
  let context identity =
    match Quiver_openssl.Tls.cluster (Ca_file "tls/ca.crt") identity with
    | Ok context -> context
    | Error (_ : Quiver_openssl.Tls.unusable) -> Alcotest.fail "no context"
  in
  let through ?(alpn = "h2") ?(clients = "-") ?(identity = Quiver_openssl.Tls.Anonymous) certificate =
    let listener, backend = listening () in
    let heard = Support.start (fun () -> serve listener scenario) in
    let front =
      Unix.open_process_args_in "python3"
        [| "python3"; "-I"; "tls_h2.py"; Int.to_string backend; "tls/" ^ certificate ^ ".crt"; "tls/" ^ certificate ^ ".key"; alpn; clients |]
    in
    let port = int_of_string (input_line front) in
    let result =
      Result.bind
        (Http2.connect { timeout = Core.Time_ns.Span.of_int_sec 5; security = Secured (context identity) } ~host:"127.0.0.1" ~port)
        (fun connection ->
          let response = Http2.request connection ~path:"/s" ~headers:[] "ping" in
          Http2.close connection;
          response)
    in
    let (_ : Unix.process_status) = Unix.close_process_in front in
    let heard = Support.join heard in
    Unix.close listener;
    (result, heard)
  in
  let answered (result, (heard : heard)) =
    match result with
    | Ok (response : Http2.response) -> Some (response.body, List.map (fun (headers, _) -> List.assoc_opt ":scheme" headers) heard.requests)
    | Error _ -> None
  in
  let refused result = match fst result with Ok _ -> "answered" | Error error -> show error in
  let pong = Some ("pong", [ Some "https" ]) in
  Alcotest.(check (option (pair string (list (option string))))) "over TLS by the CA" pong (answered (through "node"));
  Alcotest.(check string) "a chain to another CA" "(Untrusted\"unable to get local issuer certificate\")" (refused (through "stranger"));
  Alcotest.(check bool) "a certificate for another name" true
    (Base.String.is_prefix (refused (through "elsewhere")) ~prefix:"(Untrusted");
  Alcotest.(check string) "a server choosing no ALPN protocol" "(Not_h2())" (refused (through ~alpn:"-" "node"));
  Alcotest.(check bool) "a server asking for a certificate this side does not present" true
    (Option.is_none (answered (through ~clients:"tls/ca.crt" "node")));
  Alcotest.(check (option (pair string (list (option string))))) "and one it does" pong
    (answered (through ~clients:"tls/ca.crt" ~identity:(Presenting { certificate = "tls/node.crt"; key = "tls/node.key" }) "node"));
  let unusable trust identity =
    match Quiver_openssl.Tls.cluster trust identity with
    | Ok _ -> "usable"
    | Error (Context _) -> "context"
    | Error (Certificate _) -> "certificate"
    | Error (Key _) -> "key"
    | Error Unmatching_keys -> "unmatching"
    | Error (Peers _) -> "peers"
  in
  Alcotest.(check (list string)) "unusable files"
    [ "peers"; "certificate"; "key"; "key" ]
    [
      unusable (Ca_file "tls/missing.crt") Anonymous;
      unusable (Ca_file "tls/ca.crt") (Presenting { certificate = "tls/missing.crt"; key = "tls/node.key" });
      unusable (Ca_file "tls/ca.crt") (Presenting { certificate = "tls/node.crt"; key = "tls/missing.key" });
      unusable (Ca_file "tls/ca.crt") (Presenting { certificate = "tls/node.crt"; key = "tls/stranger.key" });
    ]

let tests =
  [
    Alcotest.test_case "over TLS: the server's chain, name and ALPN, this side's certificate" `Quick secured;
    Alcotest.test_case "broken frames and refusals" `Quick refusals;
    Alcotest.test_case "a response larger than the windows" `Quick large;
    Alcotest.test_case "frames as large as the server lets" `Quick wide;
  ]
  @ List.map QCheck_alcotest.to_alcotest [ exchanged ]
