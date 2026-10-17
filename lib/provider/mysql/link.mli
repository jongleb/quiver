(** [Plaintext] is for local test servers only. *)
type tls = Verified_tls of Transport.authority | Plaintext

(** Whether the server may ask for local data ([LOAD DATA LOCAL INFILE]); a
    request is answered only by {!load_local}, with the data it is given and
    never with a file the server names. *)
type local_data = Without_local_data | With_local_data

(** A capability the client cannot work without. *)
type capability =
  | Protocol_41
  | Secure_connection
  | Plugin_auth
  | Deprecate_eof
  | Ssl
[@@deriving sexp_of ~portable]

type error =
  | Unsupported_protocol of int  (** Greeting is not protocol version 10. *)
  | Missing_capability of capability
  | Unsupported_server of string  (** Version string of an untested line. *)
  | Unsupported_auth of string  (** Authentication plugin quiver lacks. *)
  | Untrusted_certificate of string  (** A remote server failed verification. *)
  | Cleartext_refused  (** Clear-text password asked without trusted TLS. *)
  | Refused of { code : int; sqlstate : string; message : string }
      (** An ERR packet from the server. *)
  | Unexpected_packet of int  (** First byte of a packet out of place. *)
[@@deriving sexp_of ~portable]

type t
(** An authenticated connection speaking the client/server protocol. *)

val connect :
  local_data:local_data ->
  tls:tls ->
  timeout:Core.Time_ns.Span.t ->
  host:string ->
  port:int ->
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
