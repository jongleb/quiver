open Base.Result.Let_syntax

type endpoint = Tcp of { host : string; port : int } | Socket of File_path.t
[@@deriving sexp_of ~portable]

type error =
  | Unresolved of string
  | Unreachable of { endpoint : endpoint; message : string }
  | Tls of string
  | Expired_certificate
  | Io of string
  | Timed_out of Core.Time_ns.Span.t
[@@deriving sexp_of ~portable]

type authority =
  | System_roots
  | Ca_file of File_path.t
  | Pinned of Fingerprint.t

type trust = Trusted | Loopback | Unproven of string [@@deriving sexp_of ~portable]

(** Whether bytes go to the socket as they are or through OpenSSL. *)
type channel = Plain | Secure of Quiver_openssl.Tls.session

type t = {
  fd : Unix.file_descr;
  mutable timeout : Core.Time_ns.Span.t;
  mutable channel : channel;
  mutable failure : error option;
}

let lost = "the connection closed without TLS close_notify"

let connect ~timeout endpoint =
  Base.Portable_lazy.force Quiver_core.Blocking.sockets;
  let seconds = Core.Time_ns.Span.to_sec timeout in
  let unreachable error =
    Error (`Transport (Unreachable { endpoint; message = Unix.error_message error }))
  in
  let attempt family address ~tune =
    match Unix.socket ~cloexec:true family SOCK_STREAM 0 with
    | exception Unix.Unix_error (error, _, _) -> unreachable error
    | fd -> (
    match
      Unix.set_nonblock fd;
      tune fd;
      (match Unix.connect fd address with
      | () -> ()
      | exception Unix.Unix_error ((EINPROGRESS | EAGAIN), _, _) -> (
          match Quiver_core.Blocking.wait ~seconds Writable fd with
          | Timed_out -> raise (Unix.Unix_error (ETIMEDOUT, "connect", ""))
          | Ready -> (
              match Unix.getsockopt_error fd with
              | None -> ()
              | Some error -> raise (Unix.Unix_error (error, "connect", "")))));
      Unix.clear_nonblock fd;
      Unix.setsockopt_float fd SO_RCVTIMEO seconds;
      Unix.setsockopt_float fd SO_SNDTIMEO seconds
    with
    | () -> Ok { fd; timeout; channel = Plain; failure = None }
    | exception Unix.Unix_error (error, _, _) ->
        (try Unix.close fd with Unix.Unix_error _ -> ());
        unreachable error)
  in
  match endpoint with
  | Socket path ->
      attempt PF_UNIX (ADDR_UNIX (File_path.to_string path)) ~tune:ignore
  | Tcp { host; port } -> (
      let attempt (address : Unix.addr_info) =
        attempt address.ai_family address.ai_addr ~tune:Quiver_core.Tcp.tune
      in
      match
        Unix.getaddrinfo host (Int.to_string port) [ AI_SOCKTYPE SOCK_STREAM ]
      with
      | [] | (exception Unix.Unix_error _) -> Error (`Transport (Unresolved host))
      | first :: rest ->
          List.fold_left
            (fun result address ->
              match result with Ok _ -> result | Error _ -> attempt address)
            (attempt first) rest)

let read t buffer ~off ~len =
  let fail target error =
    target.failure <- Some error;
    0
  in
  match t.channel with
  | Plain ->
      let rec go () =
        match Unix.read_bigarray t.fd buffer off len with
        | count -> count
        | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
            fail t (Timed_out t.timeout)
        | exception Unix.Unix_error (EINTR, _, _) -> go ()
        | exception Unix.Unix_error (error, _, _) ->
            fail t (Io (Unix.error_message error))
      in
      go ()
  | Secure session -> (
      match Quiver_openssl.Tls.read_bigstring session buffer ~pos:off ~len with
      | Done count -> count
      | Want_read | Want_write -> fail t (Timed_out t.timeout)
      | Closed -> 0
      | Lost -> fail t (Tls lost)
      | Failed reason -> fail t (Tls reason))

let write t text =
  let rec go off =
    if off = String.length text then Ok ()
    else
      match t.channel with
      | Plain -> (
          match
            Unix.write_substring t.fd text off (String.length text - off)
          with
          | count -> go (off + count)
          | exception Unix.Unix_error ((EAGAIN | EWOULDBLOCK), _, _) ->
              Error (`Transport (Timed_out t.timeout))
          | exception Unix.Unix_error (EINTR, _, _) -> go off
          | exception Unix.Unix_error (error, _, _) ->
              Error (`Transport (Io (Unix.error_message error))))
      | Secure session -> (
          match
            Quiver_openssl.Tls.write session text ~pos:off
              ~len:(String.length text - off)
          with
          | Done count -> go (off + count)
          | Want_write | Want_read -> Error (`Transport (Timed_out t.timeout))
          | Closed | Lost -> Error (`Transport (Tls lost))
          | Failed reason -> Error (`Transport (Tls reason)))
  in
  go 0

let start_tls t ~host authority =
  let check =
    match (authority, host) with
    | System_roots, ("127.0.0.1" | "::1") -> `Validity
    | System_roots, _ | Ca_file _, _ -> `Chain
    | Pinned fingerprint, _ -> `Pinned fingerprint
  and trust : Quiver_openssl.Tls.trust =
    match authority with
    | System_roots | Pinned _ -> System_roots
    | Ca_file file -> Ca_file (File_path.to_string file)
  in
  let expectations : (Quiver_openssl.Tls.expectation * string) list =
    match (check, Quiver_openssl.Tls.naming host) with
    | (`Validity | `Pinned _), _ -> []
    | `Chain, Ip -> [ (Ip, host) ]
    | `Chain, (Host | Server_name) -> [ (Server_name, host); (Host, host) ]
  in
  let tls reason = `Transport (Tls reason) in
  let%bind session =
    Result.map_error tls
      (Result.bind (Quiver_openssl.Tls.client trust) (fun context ->
           Quiver_openssl.Tls.session context t.fd))
  in
  let%bind () =
    Result.map_error tls
      (Base.List.fold_result expectations ~init:() ~f:(fun () (expectation, name) ->
           Quiver_openssl.Tls.expect session expectation name))
  in
  match Quiver_openssl.Tls.connect session with
  | Want_read | Want_write -> Error (`Transport (Timed_out t.timeout))
  | Closed | Lost -> Error (tls lost)
  | Failed reason -> Error (tls reason)
  | Done () -> (
      t.channel <- Secure session;
      match check with
      | `Pinned fingerprint -> (
          match
            Fingerprint.check fingerprint
              ~digest:
                (Option.value ~default:""
                   (Quiver_openssl.Tls.fingerprint session))
          with
          | `Pinned -> Ok Trusted
          | `Different ->
              Ok
                (Unproven
                   (Format.asprintf "certificate is not the pinned %a"
                      Fingerprint.pp fingerprint)))
      | `Validity -> (
          match Quiver_openssl.Tls.validity session with
          | Within_dates -> Ok Loopback
          | Outside_dates | No_certificate ->
              Error (`Transport Expired_certificate))
      | `Chain -> (
          match Quiver_openssl.Tls.verified session with
          | Ok () -> Ok Trusted
          | Error reason -> Ok (Unproven reason)))

let set_timeout t timeout =
  let seconds = Core.Time_ns.Span.to_sec timeout in
  (try
     Unix.setsockopt_float t.fd SO_RCVTIMEO seconds;
     Unix.setsockopt_float t.fd SO_SNDTIMEO seconds
   with Unix.Unix_error ((_ : Unix.error), _, _) -> ());
  t.timeout <- timeout

let failure t = t.failure

let close t =
  let () =
    match t.channel with
    | Plain -> ()
    | Secure session -> ignore (Quiver_openssl.Tls.shutdown session)
  in
  try Unix.close t.fd with Unix.Unix_error _ -> ()
