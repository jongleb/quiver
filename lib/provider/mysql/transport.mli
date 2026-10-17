type error =
  | Unresolved of string  (** The host name has no address. *)
  | Unreachable of { host : string; port : int; message : string }
  | Tls of string  (** OpenSSL's description of a failed TLS handshake. *)
  | Expired_certificate  (** Outside its validity period. *)
  | Io of string  (** The connection failed after it was set up. *)
  | Timed_out of Core.Time_ns.Span.t
      (** Nothing could be read or written for so long. *)
[@@deriving sexp_of ~portable]

(** Who vouches for the server's certificate. *)
type authority =
  | System_roots
      (** The system's CAs, by Connector/C 3.4 rules: on loopback only the
          validity period is checked. *)
  | Ca_file of File_path.t
      (** Chain to these CAs and host name, on every host. *)
  | Pinned of Fingerprint.t
      (** This certificate exactly; chain and name are not checked. *)

(** How far the server's certificate is trusted. *)
type trust =
  | Trusted  (** Chain and host name verified. *)
  | Loopback  (** 127.0.0.1 or ::1: validity period checked. *)
  | Unproven of string  (** OpenSSL's reason the remote chain or name failed. *)
[@@deriving sexp_of ~portable]

type t
(** A TCP connection to a server, plain or upgraded to TLS. *)

val connect :
  timeout:Core.Time_ns.Span.t ->
  host:string ->
  port:int ->
  (t, [> `Transport of error ]) result @@ portable

val read : t -> Base_bigstring.t -> off:int -> len:int -> int @@ portable
val write : t -> string -> (unit, [> `Transport of error ]) result @@ portable

val start_tls :
  t -> host:string -> authority -> (trust, [> `Transport of error ]) result @@ portable

val failure : t -> error option @@ portable
val close : t -> unit @@ portable
