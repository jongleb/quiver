open Base.Result.Let_syntax

type error =
  | Closed
  | Io of string
  | Malformed of string
  | Tls of Protection.error
[@@deriving sexp_of ~portable]

type peer = { host : string; address : Unix.sockaddr }

type incoming =
  | Message of string
  | Acknowledged of int
  | Batch of {
      codec : Frame.codec;
      chunk : int;
      seq : int;
      raw : int;
      header : string;
      payload : string;
      length : int;
    }

(** Bytes received from the peer. *)
type received = {
  buffer : Bytes.t;
  mutable start : int;  (** First byte received and not yet taken. *)
  mutable stop : int;  (** End of the bytes received. *)
  mutable batch : Bytes.t;  (** The last batch frame's payload. *)
}

type t = {
  flow : Protection.flow;
  received : received Await.Capsule.Await.With_mutex.t;
      (** Held by the thread reading a frame. *)
}

(** Why reading a frame failed. *)
type failure : value mod contended portable =
  [ `Channel of error | `Frame of Frame.error ]

(** Whether a listening socket still accepts connections. *)
type state = Listening | Stopped

type use = {
  mutable state : state;
  mutable users : int;  (** Threads waiting on the socket now. *)
}
(** Who may still use a listening socket: it is closed once it is stopped and
    nobody waits on it, so its descriptor is never reused under a waiter. *)

type listener = { socket : Unix.file_descr; use : use Await.Capsule.Await.With_mutex.t }

let io message = Error (`Channel (Io message))

let unix f =
  match f () with
  | value -> Ok value
  | exception Unix.Unix_error (error, _, _) -> io (Unix.error_message error)

let of_flow flow =
  Result.map_error
    (fun (`Protection error) -> `Channel (Tls error))
    (Result.map
       (fun flow ->
         {
           flow;
           received =
             Await.Capsule.Await.With_mutex.create (fun () ->
                 {
                   buffer = Bytes.create (1 lsl 16);
                   start = 0;
                   stop = 0;
                   batch = Bytes.create 0;
                 });
         })
       flow)

let connect protection { host; address } =
  Base.Portable_lazy.force Quiver_core.Blocking.sockets;
  let%bind fd =
    unix (fun () ->
        let fd =
          Unix.socket ~cloexec:true (Unix.domain_of_sockaddr address) SOCK_STREAM 0
        in
        match
          Quiver_core.Tcp.tune fd;
          Unix.connect fd address
        with
        | () -> fd
        | exception failure ->
            Unix.close fd;
            raise failure)
  in
  of_flow (Protection.connected protection ~host fd)

let listen address =
  Base.Portable_lazy.force Quiver_core.Blocking.sockets;
  unix (fun () ->
      let socket =
        Unix.socket ~cloexec:true (Unix.domain_of_sockaddr address) SOCK_STREAM 0
      in
      Unix.setsockopt socket SO_REUSEADDR true;
      Unix.bind socket address;
      Unix.listen socket 64;
      Unix.set_nonblock socket;
      {
        socket;
        use =
          Await.Capsule.Await.With_mutex.create (fun () ->
              { state = Listening; users = 0 });
      })

let listening_address listener = Unix.getsockname listener.socket

let release socket =
  match Unix.close socket with () -> () | exception Unix.Unix_error _ -> ()

let accept listener protection =
  let socket = listener.socket in
  let rec go () =
    match
      Quiver_core.Guarded.hold listener.use (fun use ->
          match use.state with
          | Stopped -> Stopped
          | Listening ->
              use.users <- use.users + 1;
              Listening)
    with
    | Stopped -> Error (`Channel Closed)
    | Listening -> (
        let check_stopped_every = 0.1 in
        let accepted =
          match
            ignore
              (Quiver_core.Blocking.wait ~seconds:check_stopped_every Readable
                 socket);
            Unix.accept ~cloexec:true socket
          with
          | fd, _ -> Ok fd
          | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
              Error `Again
          | exception Unix.Unix_error (error, _, _) -> Error (`Failed error)
        in
        let state =
          Quiver_core.Guarded.hold listener.use (fun use ->
              use.users <- use.users - 1;
              match (use.state, use.users) with
              | Stopped, 0 ->
                  release socket;
                  Stopped
              | state, _ -> state)
        in
        match (accepted, state) with
        | Ok fd, Stopped ->
            release fd;
            Error (`Channel Closed)
        | Ok fd, Listening -> (
            match
              Unix.clear_nonblock fd;
              Quiver_core.Tcp.tune fd
            with
            | () -> of_flow (Protection.accepted protection fd)
            | exception Unix.Unix_error (error, _, _) ->
                release fd;
                io (Unix.error_message error))
        | Error `Again, Listening -> go ()
        | Error (`Again | `Failed _), Stopped -> Error (`Channel Closed)
        | Error (`Failed error), Listening -> io (Unix.error_message error))
  in
  go ()

let stop listener =
  let socket = listener.socket in
  match
    Quiver_core.Guarded.hold listener.use (fun use ->
        match (use.state, use.users) with
        | Stopped, _ -> `Stopped
        | Listening, 0 ->
            use.state <- Stopped;
            release socket;
            `Stopped
        | Listening, (_ : int) ->
            use.state <- Stopped;
            `Waited_on (Unix.getsockname socket))
  with
  | `Stopped -> ()
  | `Waited_on address -> (
      match Unix.socket ~cloexec:true (Unix.domain_of_sockaddr address) SOCK_STREAM 0 with
      | exception Unix.Unix_error _ -> ()
      | waking ->
          (match
             Unix.set_nonblock waking;
             Unix.connect waking address
           with
          | () -> ()
          | exception Unix.Unix_error (EINPROGRESS, _, _) ->
              let connecting_seconds = 0.1 in
              ignore
                (Quiver_core.Blocking.wait ~seconds:connecting_seconds Writable
                   waking)
          | exception Unix.Unix_error _ -> ());
          release waking)

let all (type (a : value mod contended portable)) (type (e : value mod contended portable)) count (opening : unit -> (a, e) result) ~close =
  let opened = Quiver_core.Concurrent.each count (fun (_ : int) -> opening ()) in
  match List.find_map (function Error error -> Some error | Ok _ -> None) opened with
  | None -> Ok (List.filter_map Result.to_option opened)
  | Some error ->
      List.iter (function Ok channel -> close channel | Error _ -> ()) opened;
      Error error

let write t frame ~length =
  Result.map_error
    (fun message -> `Channel (Io message))
    (Protection.write t.flow frame 0 length)

let send t sexp =
  let text = Sexplib0.Sexp.to_string sexp in
  let buffer = Quiver_core.Byte_buffer.create () in
  Frame.start buffer;
  Quiver_core.Byte_buffer.append_string buffer text;
  Frame.seal buffer Message Plain ~raw:(String.length text) ~chunk:0;
  (* The buffer is not changed while it is written. *)
  write t (Bytes.unsafe_to_string buffer.bs) ~length:buffer.len

let acknowledge t ~received =
  let buffer = Quiver_core.Byte_buffer.create () in
  Frame.start buffer;
  Frame.seal buffer Ack Plain ~raw:0 ~chunk:received;
  (* The buffer is not changed while it is written. *)
  write t (Bytes.unsafe_to_string buffer.bs) ~length:buffer.len

let read t =
  let flow = t.flow in
  match
    (Quiver_core.Guarded.hold t.received (fun received : (incoming, failure) result ->
         let rec fill count =
           if received.stop - received.start >= count then Ok ()
           else (
             if received.start + count > Bytes.length received.buffer then (
               let kept = received.stop - received.start in
               Bytes.blit received.buffer received.start received.buffer 0 kept;
               received.start <- 0;
               received.stop <- kept);
             match
               Protection.read flow received.buffer received.stop
                 (Bytes.length received.buffer - received.stop)
             with
             | Ok 0 -> Error (`Channel Closed)
             | Ok got ->
                 received.stop <- received.stop + got;
                 fill count
             | Error message -> Error (`Channel (Io message)))
         in
         let take target count =
           match count <= Bytes.length received.buffer with
           | true ->
               let%map () = fill count in
               Bytes.blit received.buffer received.start target 0 count;
               received.start <- received.start + count
           | false ->
               let buffered = received.stop - received.start in
               Bytes.blit received.buffer received.start target 0 buffered;
               received.start <- received.stop;
               let rec rest filled =
                 if filled = count then Ok ()
                 else
                   match Protection.read flow target filled (count - filled) with
                   | Ok 0 -> Error (`Channel Closed)
                   | Ok got -> rest (filled + got)
                   | Error message -> Error (`Channel (Io message))
               in
               rest buffered
         in
         let%bind () = fill Frame.header_bytes in
         let text =
           Bytes.sub_string received.buffer received.start Frame.header_bytes
         in
         received.start <- received.start + Frame.header_bytes;
         let%bind header = Frame.header text in
         match header.kind with
         | Batch ->
             if Bytes.length received.batch < header.length then
               received.batch <- Bytes.create header.length;
             let%bind () = take received.batch header.length in
             (* The bytes change only at this channel's next read. *)
             let payload = Bytes.unsafe_to_string received.batch in
             let%map () = Frame.payload header payload in
             Batch
               {
                 codec = header.codec;
                 chunk = header.chunk;
                 seq = header.seq;
                 raw = header.raw;
                 header = text;
                 payload;
                 length = header.length;
               }
         | Ack when header.length > 0 ->
             Error (`Channel (Malformed "an ack with a payload"))
         | Ack -> Ok (Acknowledged header.chunk)
         | Message ->
             let payload = Bytes.create header.length in
             let%bind () = take payload header.length in
             let payload = Bytes.unsafe_to_string payload in
             let%map () = Frame.payload header payload in
             Message payload)
      : (incoming, failure) result)
  with
  | Ok incoming -> Ok incoming
  | Error (`Channel error) -> Error (`Channel error)
  | Error (`Frame error) -> Error (`Frame error)

let receive t decode =
  let%bind incoming = read t in
  match incoming with
  | Batch _ -> Error (`Channel (Malformed "a batch where a message was due"))
  | Acknowledged _ ->
      Error (`Channel (Malformed "an ack where a message was due"))
  | Message text -> (
      match Parsexp.Single.parse_string text with
      | Error error ->
          Error (`Channel (Malformed (Parsexp.Parse_error.message error)))
      | Ok sexp -> (
          match decode sexp with
          | value -> Ok value
          | exception Sexplib0.Sexp_conv.Of_sexp_error (failure, _) ->
              Error
                (`Channel
                   (Malformed
                      (match failure with
                      | Failure reason -> reason
                      | other -> Printexc.to_string other)))))

let forward t ~header ~payload ~length =
  let%bind () = write t header ~length:(String.length header) in
  write t payload ~length

(** What one side of a splice got next. *)
type piece = Pending  (** Bytes in the buffer to pass on. *) | Ended

let splice a b =
  let pass (from, into) =
    let flow = from.flow in
    Quiver_core.Guarded.hold from.received (fun received : (unit, error) result ->
        let rec go () =
          match
            if received.stop > received.start then Ok Pending
            else
              match
                Protection.read flow received.buffer 0
                  (Bytes.length received.buffer)
              with
              | Ok 0 -> Ok Ended
              | Ok count ->
                  received.start <- 0;
                  received.stop <- count;
                  Ok Pending
              | Error message -> Error (Io message)
          with
          | Ok Ended -> Ok (Protection.shutdown into.flow)
          | Ok Pending -> (
              let start = received.start and stop = received.stop in
              received.start <- stop;
              (* Nothing changes the buffer until the write returns. *)
              match
                Protection.write into.flow
                  (Bytes.unsafe_to_string received.buffer)
                  start (stop - start)
              with
              | Ok () -> go ()
              | Error message -> Error (Io message))
          | Error error -> Error error
        in
        go ())
  in
  match
    List.find_map
      (function Ok () -> None | Error error -> Some error)
      (Quiver_core.Concurrent.each 2 (fun index ->
           let passed =
             match index with 0 -> pass (a, b) | _ -> pass (b, a)
           in
           Result.iter_error
             (fun (_ : error) ->
               Protection.abort a.flow;
               Protection.abort b.flow)
             passed;
           passed))
  with
  | None -> Ok ()
  | Some error -> Error (`Channel error)

type handle = Unix.file_descr

let handle t = Protection.descriptor t.flow
let path t = Quiver_core.Tcp.path (Protection.descriptor t.flow)
let strain t = Quiver_core.Tcp.strain (Protection.descriptor t.flow)
let mark t traffic = Quiver_core.Tcp.mark (Protection.descriptor t.flow) traffic

let sever fd =
  match Unix.shutdown fd SHUTDOWN_ALL with
  | () -> ()
  | exception Unix.Unix_error _ -> ()

let finish t = Protection.shutdown t.flow
let abort t = Protection.abort t.flow
let close t = Protection.close t.flow
