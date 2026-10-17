open Base.Result.Let_syntax

type error =
  | Closed
  | Io of string
  | Malformed of string
  | Tls of Protection.error
[@@deriving sexp_of ~portable]

type peer = { host : string; address : Unix.sockaddr }

type incoming =
  | Message of string  (** JSON text. *)
  | Batch of {
      codec : Frame.codec;
      chunk : int;
      seq : int;
      raw : int;
      header : string;
      payload : string;
    }

(** Bytes received from the peer. *)
type received = {
  mutable buffer : Bytes.t;
  mutable start : int;  (** First byte received and not yet taken. *)
  mutable stop : int;  (** End of the bytes received. *)
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

type listener = { socket : Unix.file_descr; state : state Atomic.t }

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
                 { buffer = Bytes.create (1 lsl 16); start = 0; stop = 0 });
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
      { socket; state = Atomic.make Listening })

let listening_address listener = Unix.getsockname listener.socket

let accept listener protection =
  let rec go () =
    match Atomic.get listener.state with
    | Stopped -> Error (`Channel Closed)
    | Listening -> (
        let check_stopped_every = 0.1 in
        match
          ignore
            (Quiver_core.Blocking.wait ~seconds:check_stopped_every Readable
               listener.socket);
          Unix.accept ~cloexec:true listener.socket
        with
        | fd, _ -> (
            match
              Unix.clear_nonblock fd;
              Quiver_core.Tcp.tune fd
            with
            | () -> of_flow (Protection.accepted protection fd)
            | exception Unix.Unix_error (error, _, _) ->
                Unix.close fd;
                io (Unix.error_message error))
        | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK | EINTR), _, _) ->
            go ()
        | exception Unix.Unix_error (error, _, _) -> (
            match Atomic.get listener.state with
            | Stopped -> Error (`Channel Closed)
            | Listening -> io (Unix.error_message error)))
  in
  go ()

let stop listener =
  Atomic.set listener.state Stopped;
  match Unix.close listener.socket with
  | () -> ()
  | exception Unix.Unix_error _ -> ()

let all (type (a : value mod contended portable)) (type (e : value mod contended portable)) count (opening : unit -> (a, e) result) ~close =
  let opened = Quiver_core.Concurrent.each count (fun (_ : int) -> opening ()) in
  match List.find_map (function Error error -> Some error | Ok _ -> None) opened with
  | None -> Ok (List.filter_map Result.to_option opened)
  | Some error ->
      List.iter (function Ok channel -> close channel | Error _ -> ()) opened;
      Error error

let write t frame =
  Result.map_error
    (fun message -> `Channel (Io message))
    (Protection.write t.flow frame 0 (String.length frame))

(* The frame is not changed while it is written. *)
let write_bytes t frame = write t (Bytes.unsafe_to_string frame)

let send t sexp =
  let text = Sexplib0.Sexp.to_string sexp in
  let buffer = Quiver_core.Byte_buffer.create () in
  Frame.start buffer;
  Quiver_core.Byte_buffer.append_string buffer text;
  Frame.seal buffer Message Plain ~raw:(String.length text) ~chunk:0;
  write t (Quiver_core.Byte_buffer.contents buffer)

let read t =
  let flow = t.flow in
  match
    (Quiver_core.Guarded.hold t.received (fun received : (incoming, failure) result ->
         let take count =
           let rec fill () =
             if received.stop - received.start >= count then Ok ()
             else (
               if received.start + count > Bytes.length received.buffer then (
                 let kept = received.stop - received.start in
                 let target =
                   if count > Bytes.length received.buffer then
                     Bytes.create (Int.max count (2 * Bytes.length received.buffer))
                   else received.buffer
                 in
                 Bytes.blit received.buffer received.start target 0 kept;
                 received.buffer <- target;
                 received.start <- 0;
                 received.stop <- kept);
               match
                 Protection.read flow received.buffer received.stop
                   (Bytes.length received.buffer - received.stop)
               with
               | Ok 0 -> Error (`Channel Closed)
               | Ok count ->
                   received.stop <- received.stop + count;
                   fill ()
               | Error message -> Error (`Channel (Io message)))
           in
           let%map () = fill () in
           let text = Bytes.sub_string received.buffer received.start count in
           received.start <- received.start + count;
           text
         in
         let%bind text = take Frame.header_bytes in
         let%bind header = Frame.header text in
         let%bind payload = take header.length in
         let%bind () = Frame.payload header payload in
         match header.kind with
         | Batch ->
             Ok
               (Batch
                  {
                    codec = header.codec;
                    chunk = header.chunk;
                    seq = header.seq;
                    raw = header.raw;
                    header = text;
                    payload;
                  })
         | Message -> Ok (Message payload))
      : (incoming, failure) result)
  with
  | Ok incoming -> Ok incoming
  | Error (`Channel error) -> Error (`Channel error)
  | Error (`Frame error) -> Error (`Frame error)

let receive t decode =
  let%bind incoming = read t in
  match incoming with
  | Batch _ -> Error (`Channel (Malformed "a batch where a message was due"))
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

let forward t ~header ~payload =
  let%bind () = write t header in
  write t payload

(** What one side of a splice got next. *)
type piece = Bytes of string | Ended

let splice a b =
  let pass (from, into) =
    let flow = from.flow in
    Quiver_core.Guarded.hold from.received (fun received : (unit, error) result ->
        let rec go () =
          match
            if received.stop > received.start then (
              let pending = received.stop - received.start in
              let text =
                Bytes.sub_string received.buffer received.start pending
              in
              received.start <- received.stop;
              Ok (Bytes text))
            else
              match
                Protection.read flow received.buffer 0
                  (Bytes.length received.buffer)
              with
              | Ok 0 -> Ok Ended
              | Ok count -> Ok (Bytes (Bytes.sub_string received.buffer 0 count))
              | Error message -> Error (Io message)
          with
          | Ok Ended -> Ok (Protection.shutdown into.flow)
          | Ok (Bytes text) -> (
              match write into text with
              | Ok () -> go ()
              | Error (`Channel error) -> Error error)
          | Error error -> Error error
        in
        go ())
  in
  match
    List.find_map
      (function Ok () -> None | Error error -> Some error)
      (Quiver_core.Concurrent.each 2 (function
        | 0 -> pass (a, b)
        | _ -> pass (b, a)))
  with
  | None -> Ok ()
  | Some error -> Error (`Channel error)

type handle = Unix.file_descr

let handle t = Protection.descriptor t.flow

let sever fd =
  match Unix.shutdown fd SHUTDOWN_ALL with
  | () -> ()
  | exception Unix.Unix_error _ -> ()

let finish t = Protection.shutdown t.flow
let abort t = Protection.abort t.flow
let close t = Protection.close t.flow
