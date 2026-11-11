@@ portable

(** One request over HTTP/1.1 (RFC 9112) on a connection of its own, under TLS when the dial
    says so, and the body of its answer. *)

type error =
  | Refused of { code : int; body : string }  (** A status other than 2xx. *)
  | Malformed of string  (** A response not as HTTP/1.1 gives one. *)
  | Untrusted of string  (** OpenSSL's reason the server's chain or name failed. *)
[@@deriving sexp_of ~portable]

(** What is asked. *)
type request =
  | Post of { path : string; json : string }  (** A JSON body to the path. *)
  | Delete of string  (** The resource at the path. *)

val text : host:string -> port:int -> request -> string
(** The request as it goes on the wire, the connection closed after the answer. *)

val body : string -> (string, [> `Http1 of error ]) result
(** The body of a whole response: by its length, in chunks, or up to the end; [Refused] unless
    the status is 2xx. *)

val request :
  Http2.dial ->
  host:string ->
  port:int ->
  request ->
  (string, [> `Http1 of error | `Transport of Quiver_mysql.Transport.error ]) result
(** The answer's body; under TLS the connection offers ALPN http/1.1. *)
