@@ portable

(** TLS through OpenSSL, declared portable at the C boundary: every call is
    safe from any thread, as [openssl_stubs.c] explains. *)

type context : value mod contended portable
(** Configured completely when made, then only read. *)

(** Whom a client believes about a server's certificate. *)
type trust =
  | System_roots
  | Ca_file of string

val client : trust -> (context, string) result
(** TLS 1.2 or 1.3; the error is OpenSSL's reason. *)

(** What of a node's own TLS material OpenSSL refused. *)
type unusable =
  | Context of string
  | Certificate of string
  | Key of string
  | Unmatching_keys
  | Peers of string

val mutual :
  certificate:string -> key:string -> peers:string -> (context, unusable) result
(** TLS 1.3 only: this side presents the PEM [certificate] chain and [key], and
    a peer must present a certificate chaining to [peers]. *)

type session : value mod contended portable
(** One connection; each call takes the session's lock, so threads may share it.
*)

val session : context -> Unix.file_descr -> (session, string) result

(** What the handshake must find in the peer certificate, or tell the peer. *)
type expectation =
  | Host  (** The certificate names this host. *)
  | Ip  (** The certificate names this IP address. *)
  | Server_name  (** The host is sent in SNI. *)

val expect : session -> expectation -> string -> (unit, string) result
(** Before the handshake. *)

val naming : string -> expectation
(** [Ip] for an IPv4 or IPv6 address, else [Host]. *)

(** How one call into OpenSSL ended. *)
type 'a outcome =
  | Done of 'a
  | Want_read
  | Want_write
  | Closed  (** The peer ended TLS with close_notify. *)
  | Lost  (** The socket failed or closed without close_notify. *)
  | Failed of string  (** OpenSSL's reason. *)

val connect : session -> unit outcome
val accept : session -> unit outcome

val read : session -> Bytes.t -> pos:int -> len:int -> int outcome
(** Into the bytes; [Done n] with [n > 0]. *)

val read_bigstring :
  session -> Base_bigstring.t -> pos:int -> len:int -> int outcome

val write : session -> string -> pos:int -> len:int -> int outcome
(** A prefix of the substring; after [Want_read] or [Want_write] the same
    substring must be written again. *)

val shutdown : session -> unit outcome
(** Sends close_notify. *)

val verified : session -> (unit, string) result
(** Whether the peer chain and the expected name checked out, with OpenSSL's
    reason when not. *)

val fingerprint : session -> string option
(** SHA-256 of the peer certificate. *)

(** Whether the peer certificate is within its dates now. *)
type validity = Within_dates | Outside_dates | No_certificate

val validity : session -> validity
