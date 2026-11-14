type error = Unusable of string | Untrusted of string | Handshake of string
[@@deriving sexp_of ~portable]

type t : value mod contended portable =
  | Plaintext
  | Mutual of Quiver_openssl.Tls.context

let plaintext = Plaintext

let mutual ~certificate ~key ~peers =
  let peers = File_path.to_string peers in
  match
    Quiver_openssl.Tls.mutual
      ~certificate:(File_path.to_string certificate)
      ~key:(File_path.to_string key) ~peers
  with
  | Ok context -> Ok (Mutual context)
  | Error (Context reason | Certificate reason | Key reason) ->
      Error (`Protection (Unusable reason))
  | Error Unmatching_keys ->
      Error (`Protection (Unusable "the key does not match the certificate"))
  | Error (Peers _) ->
      Error (`Protection (Unusable ("cannot read trusted certificates " ^ peers)))

(** Whether bytes go to the socket as they are or through OpenSSL. *)
type channel : value mod contended portable =
  | Plain  (** The kernel serves a reader and a writer. *)
  | Secure of Quiver_openssl.Tls.session
      (** OpenSSL takes one call at a time, and nobody holds the session while
          waiting for the socket. *)

(** Whether this side told the peer it sends nothing more. *)
type notice = Unsent | Sent

(** How long reads and writes may wait for the socket. *)
type patience =
  | Unlimited
  | Within of Core.Time_ns.Span.t

(** Until when this flow's reads and writes may wait. *)
type limit =
  | Unbounded
  | Until of { since : Quiver_core.Stopwatch.t; within : Core.Time_ns.Span.t }

type flow = {
  fd : Unix.file_descr;
  channel : channel;
  notice : notice Atomic.t;
  limit : limit Atomic.t;
}

let flowing fd channel limit =
  Unix.set_nonblock fd;
  { fd; channel; notice = Atomic.make Unsent; limit = Atomic.make limit }

let closed = "the connection closed without TLS close_notify"

let secure fd session ~start limit =
  let bounded seconds =
    match
      Unix.setsockopt_float fd SO_RCVTIMEO seconds;
      Unix.setsockopt_float fd SO_SNDTIMEO seconds
    with
    | () -> ()
    | exception Unix.Unix_error _ -> ()
  in
  let () =
    match limit with
    | Unbounded -> ()
    | Until { within; _ } -> bounded (Core.Time_ns.Span.to_sec within)
  in
  match start session with
  | Quiver_openssl.Tls.Done () ->
      bounded 0.;
      Ok (flowing fd (Secure session) limit)
  | (Want_read | Want_write | Closed | Lost | Failed _) as failed -> (
      Unix.close fd;
      match Quiver_openssl.Tls.verified session with
      | Error reason -> Error (`Protection (Untrusted reason))
      | Ok () ->
          Error
            (`Protection
               (Handshake
                  (match failed with
                  | Failed reason -> reason
                  | Done () | Want_read | Want_write | Closed | Lost -> closed))))

let accepted t fd patience =
  let limit =
    match patience with
    | Unlimited -> Unbounded
    | Within within -> Until { since = Quiver_core.Stopwatch.start (); within }
  in
  match t with
  | Plaintext -> Ok (flowing fd Plain limit)
  | Mutual context -> (
      match Quiver_openssl.Tls.session context fd with
      | Error reason -> Error (`Protection (Handshake reason))
      | Ok session -> secure fd session ~start:Quiver_openssl.Tls.accept limit)

let connected t ~host fd =
  match t with
  | Plaintext -> Ok (flowing fd Plain Unbounded)
  | Mutual context -> (
      let expectation : Quiver_openssl.Tls.expectation =
        match Quiver_openssl.Tls.naming host with Ip_address -> Ip | Host_name -> Host
      in
      match
        Result.bind (Quiver_openssl.Tls.session context fd) (fun session ->
            Result.map (fun () -> session) (Quiver_openssl.Tls.expect session expectation host))
      with
      | Error reason -> Error (`Protection (Handshake reason))
      | Ok session -> secure fd session ~start:Quiver_openssl.Tls.connect Unbounded)

(** How a call into OpenSSL ended once the socket stopped making it wait. *)
type 'a settled = Settled of 'a | Ended | Broken of string

let silent = "the peer sent nothing in the time it had"

let waited flow direction =
  match Atomic.get flow.limit with
  | Unbounded ->
      ignore (Quiver_core.Blocking.wait direction flow.fd);
      `Waited
  | Until { since; within } -> (
      match Core.Time_ns.Span.(within - Quiver_core.Stopwatch.elapsed since) with
      | left when Core.Time_ns.Span.(left <= zero) -> `Out_of_time
      | left ->
          ignore
            (Quiver_core.Blocking.wait ~seconds:(Core.Time_ns.Span.to_sec left)
               direction flow.fd);
          `Waited)

let rec settled flow attempt =
  let after direction =
    match waited flow direction with
    | `Waited -> settled flow attempt
    | `Out_of_time -> Broken silent
  in
  match attempt () with
  | Quiver_openssl.Tls.Done value -> Settled value
  | Closed | Lost -> Ended
  | Failed reason -> Broken reason
  | Want_read -> after Readable
  | Want_write -> after Writable

let rec plain flow direction attempt =
  match attempt () with
  | count -> Ok count
  | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) -> (
      match waited flow direction with
      | `Waited -> plain flow direction attempt
      | `Out_of_time -> Error silent)
  | exception Unix.Unix_error (EINTR, _, _) -> plain flow direction attempt
  | exception Unix.Unix_error (error, _, _) -> Error (Unix.error_message error)

let settle flow = Atomic.set flow.limit Unbounded

let read flow bytes off len =
  match flow.channel with
  | Plain ->
      plain flow Readable (fun () ->
          Core_unix.read_assume_fd_is_nonblocking flow.fd ~pos:off ~len bytes)
  | Secure session -> (
      match
        settled flow (fun () ->
            Quiver_openssl.Tls.read_nonblocking session bytes ~pos:off ~len)
      with
      | Settled count -> Ok count
      | Ended -> Ok 0
      | Broken reason -> Error ("TLS: " ^ reason))

let write flow text off len =
  let rec go off len =
    if len = 0 then Ok ()
    else
      match flow.channel with
      | Plain -> (
          match
            plain flow Writable (fun () ->
                Core_unix.write_assume_fd_is_nonblocking flow.fd ~pos:off ~len
                  (Bytes.unsafe_of_string text))
          with
          | Ok count -> go (off + count) (len - count)
          | Error _ as error -> error)
      | Secure session -> (
          match
            settled flow (fun () ->
                Quiver_openssl.Tls.write_nonblocking session text ~pos:off ~len)
          with
          | Settled count -> go (off + count) (len - count)
          | Ended -> Error ("TLS: " ^ closed)
          | Broken reason -> Error ("TLS: " ^ reason))
  in
  go off len

let shutdown flow =
  match Atomic.compare_and_set flow.notice Unsent Sent with
  | false -> ()
  | true -> (
      match flow.channel with
      | Plain -> (
          match Unix.shutdown flow.fd SHUTDOWN_SEND with
          | () -> ()
          | exception Unix.Unix_error _ -> ())
      | Secure session ->
          ignore
            (settled flow (fun () -> Quiver_openssl.Tls.shutdown session)))

let descriptor flow = flow.fd

let rekey_after flow bytes =
  match flow.channel with
  | Plain -> ()
  | Secure session -> Quiver_openssl.Tls.rekey_after session bytes

let key_updates flow =
  match flow.channel with
  | Plain -> 0
  | Secure session -> Quiver_openssl.Tls.key_updates session

let abort flow =
  match Unix.shutdown flow.fd SHUTDOWN_ALL with
  | () -> ()
  | exception Unix.Unix_error _ -> ()

let close flow =
  shutdown flow;
  match Unix.close flow.fd with () -> () | exception Unix.Unix_error _ -> ()
