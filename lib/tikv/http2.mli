@@ portable

(** A client of HTTP/2 (RFC 9113) over TCP, by prior knowledge or under TLS, one request at a
    time on a connection. *)

type error =
  | Closed  (** The server closed the connection. *)
  | Frame_size of { kind : int; length : int }
      (** A frame longer than this side allows, or of a length its kind cannot have. *)
  | Unexpected of { kind : int; stream : int }  (** A frame where the protocol has none. *)
  | Setting of { id : int; value : int }  (** A setting out of its range. *)
  | Window  (** A window update of 0 or past 2{^31} − 1. *)
  | Flow_control  (** The server sent more data than the windows let it. *)
  | Padding  (** Padding longer than its frame. *)
  | Going_away of { last : int; code : int; debug : string }
      (** The server ends the connection before this request. *)
  | Reset of int  (** The server ended the stream with this error code. *)
  | No_status  (** A response without [:status]. *)
  | Exhausted  (** The connection used all its stream identifiers. *)
  | Untrusted of string  (** OpenSSL's reason the server's chain or name failed. *)
  | Not_h2 of string option  (** Under TLS, the server chose another ALPN protocol, or none. *)
[@@deriving sexp_of ~portable]

(** How a connection's bytes go. *)
type security =
  | Cleartext  (** h2c, by prior knowledge. *)
  | Secured of Quiver_openssl.Tls.context
      (** TLS offering h2, the server's chain and host name checked against the context's CAs. *)

type dial = {
  timeout : Core.Time_ns.Span.t;  (** Bounds every wait. *)
  security : security;
}
(** How connections are opened. *)

type t
(** One connection; one thread uses it at a time. *)

val connect :
  dial ->
  host:string ->
  port:int ->
  (t, [> `Http2 of error | `Transport of Quiver_mysql.Transport.error ]) result
(** Opens it and sends this side's preface. *)

type response = {
  status : int;
  headers : (string * string) list;  (** Without [:status]. *)
  body : string;
  trailers : (string * string) list;
}

type stream
(** A request whose body is still being sent; the connection carries nothing else until it is
    finished. *)

val start :
  t ->
  path:string ->
  headers:(string * string) list ->
  (stream, [> `Http2 of error | `Transport of Quiver_mysql.Transport.error ]) result
(** A POST; [headers] after the pseudo-headers. *)

val send :
  stream ->
  string list ->
  (unit, [> `Http2 of error | `Hpack of Hpack.error | `Transport of Quiver_mysql.Transport.error ]) result
(** More of the body, these pieces one after another, as the windows let it go; nothing once
    the server has answered. *)

val finish :
  stream ->
  (response, [> `Http2 of error | `Hpack of Hpack.error | `Transport of Quiver_mysql.Transport.error ]) result
(** Ends the body and reads the response. *)

val request :
  t ->
  path:string ->
  headers:(string * string) list ->
  string ->
  (response, [> `Http2 of error | `Hpack of Hpack.error | `Transport of Quiver_mysql.Transport.error ]) result
(** A POST with this body; [headers] after the pseudo-headers. *)

val close : t -> unit
