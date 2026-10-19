(** [Plaintext] is for local test servers only. *)
type tls = Verified_tls of Transport.authority | Plaintext

(** Where the server listens, and how the bytes are protected on the way. *)
type route =
  | Tcp of { host : string; port : int; tls : tls }
  | Socket of File_path.t
      (** A Unix socket of a server on this machine: no TLS, the kernel carries
          the bytes, and the server counts it as a secure transport, so a
          password may go in the clear. *)

(** Whether the server may ask for local data ([LOAD DATA LOCAL INFILE]); a
    request is answered only by {!load_local}, with the data it is given and
    never with a file the server names. *)
type local_data = Without_local_data | With_local_data

type timeouts = {
  connecting : Core.Time_ns.Span.t;
      (** Longest wait while connecting, TLS and authentication included. *)
  silence : Core.Time_ns.Span.t;
      (** Longest silence of an authenticated connection. *)
}

(** A capability the client cannot work without. *)
type capability =
  | Protocol_41
  | Secure_connection
  | Plugin_auth
  | Deprecate_eof
  | Ssl
  | Lenenc_auth_data
[@@deriving sexp_of ~portable]

type error =
  | Unsupported_protocol of int  (** Greeting is not protocol version 10. *)
  | Missing_capability of capability
  | Unsupported_server of string  (** Version string of an untested line. *)
  | Unsupported_auth of string  (** Authentication plugin quiver lacks. *)
  | Untrusted_certificate of string  (** A remote server failed verification. *)
  | Cleartext_refused  (** Clear-text password asked without trusted TLS. *)
  | Unusable_public_key  (** The server's RSA key does not encrypt the password. *)
  | Short_scramble  (** The server's challenge is under 20 bytes. *)
  | Refused of { code : int; sqlstate : string; message : string }
      (** An ERR packet from the server. *)
  | Unexpected_packet of int  (** First byte of a packet out of place. *)
[@@deriving sexp_of ~portable]

type t
(** An authenticated connection speaking the client/server protocol. *)

val connect :
  local_data:local_data ->
  route ->
  timeouts:timeouts ->
  user:string ->
  password:string option ->
  ( t,
    [> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
  )
  result @@ portable

val server : t -> Server.t @@ portable

val query :
  t ->
  string ->
  ( string option array list,
    [> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
  )
  result @@ portable

val execute :
  t ->
  string ->
  ( unit,
    [> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
  )
  result @@ portable

val run_query :
  t ->
  Query.t ->
  ( string option array list,
    [> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
  )
  result @@ portable

val run_statement :
  t ->
  Statement.t ->
  ( unit,
    [> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
  )
  result @@ portable

val close : t -> unit @@ portable
val command : t -> string -> (unit, [> `Transport of Transport.error ]) result @@ portable

val receive_on :
  t ->
  (Wire.Cursor.reader ->
  ( 'a,
    ([> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
     as
     'e) )
  result)
  @ local ->
  ('a, 'e) result @@ portable

val refused : Wire.Cursor.reader -> [> `Link of error ] @@ portable

val upload :
  t ->
  string ->
  ( unit,
    [> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
  )
  result @@ portable

val send_data :
  t -> string -> (unit, [> `Transport of Transport.error ]) result
  @@ portable

val complete :
  t ->
  ( unit,
    [> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
  )
  result @@ portable
(** Ends the data of the [LOAD DATA LOCAL] statement [upload] started; the
    server then runs the statement to completion. *)

val load_local :
  t ->
  string ->
  (Bytes.t ->
  pos:int ->
  len:int ->
  ( int,
    ([> `Link of error | `Wire of Wire.error | `Transport of Transport.error ]
     as
     'e) )
  result) ->
  (unit, 'e) result
  @@ portable
