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
  | Plain  (** A blocking socket: the kernel serves a reader and a writer. *)
  | Secure of Quiver_openssl.Tls.session
      (** A non-blocking socket: OpenSSL takes one call at a time, and nobody
          holds the session while waiting for the socket. *)

(** Whether this side told the peer it sends nothing more. *)
type notice = Unsent | Sent

type flow = { fd : Unix.file_descr; channel : channel; notice : notice Atomic.t }

let closed = "the connection closed without TLS close_notify"

let secure fd session ~start =
  match start session with
  | Quiver_openssl.Tls.Done () ->
      Unix.set_nonblock fd;
      Ok { fd; channel = Secure session; notice = Atomic.make Unsent }
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

let accepted t fd =
  match t with
  | Plaintext -> Ok { fd; channel = Plain; notice = Atomic.make Unsent }
  | Mutual context -> (
      match Quiver_openssl.Tls.session context fd with
      | Error reason -> Error (`Protection (Handshake reason))
      | Ok session -> secure fd session ~start:Quiver_openssl.Tls.accept)

let connected t ~host fd =
  match t with
  | Plaintext -> Ok { fd; channel = Plain; notice = Atomic.make Unsent }
  | Mutual context -> (
      match
        Result.bind (Quiver_openssl.Tls.session context fd) (fun session ->
            Result.map
              (fun () -> session)
              (Quiver_openssl.Tls.expect session
                 (Quiver_openssl.Tls.naming host)
                 host))
      with
      | Error reason -> Error (`Protection (Handshake reason))
      | Ok session -> secure fd session ~start:Quiver_openssl.Tls.connect)

(** How a call into OpenSSL ended once the socket stopped making it wait. *)
type 'a settled = Settled of 'a | Ended | Broken of string

let rec settled fd attempt =
  match attempt () with
  | Quiver_openssl.Tls.Done value -> Settled value
  | Closed | Lost -> Ended
  | Failed reason -> Broken reason
  | Want_read ->
      ignore (Quiver_core.Blocking.wait Readable fd);
      settled fd attempt
  | Want_write ->
      ignore (Quiver_core.Blocking.wait Writable fd);
      settled fd attempt

let read flow bytes off len =
  match flow.channel with
  | Plain -> (
      let rec go () =
        match Unix.read flow.fd bytes off len with
        | count -> Ok count
        | exception Unix.Unix_error (EINTR, _, _) -> go ()
        | exception Unix.Unix_error (error, _, _) ->
            Error (Unix.error_message error)
      in
      go ())
  | Secure session -> (
      match
        settled flow.fd (fun () ->
            Quiver_openssl.Tls.read session bytes ~pos:off ~len)
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
          match Unix.write_substring flow.fd text off len with
          | count -> go (off + count) (len - count)
          | exception Unix.Unix_error (EINTR, _, _) -> go off len
          | exception Unix.Unix_error (error, _, _) ->
              Error (Unix.error_message error))
      | Secure session -> (
          match
            settled flow.fd (fun () ->
                Quiver_openssl.Tls.write session text ~pos:off ~len)
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
            (settled flow.fd (fun () -> Quiver_openssl.Tls.shutdown session)))

let descriptor flow = flow.fd

let abort flow =
  match Unix.shutdown flow.fd SHUTDOWN_ALL with
  | () -> ()
  | exception Unix.Unix_error _ -> ()

let close flow =
  shutdown flow;
  match Unix.close flow.fd with () -> () | exception Unix.Unix_error _ -> ()
