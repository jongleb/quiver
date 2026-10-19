(** How connections of quiver's protocol are protected: plaintext, for private
    networks only, or TLS 1.3 where each side proves itself with a certificate
    the other trusts. *)

type error =
  | Unusable of string
      (** This node's certificate, key or trusted certificates: OpenSSL's
          reason. *)
  | Untrusted of string
      (** The peer's certificate failed verification: OpenSSL's reason. *)
  | Handshake of string  (** The handshake failed otherwise. *)
[@@deriving sexp_of ~portable]

type t : value mod contended portable

val plaintext : t

val mutual :
  certificate:File_path.t ->
  key:File_path.t ->
  peers:File_path.t ->
  (t, [> `Protection of error ]) result
(** This node's certificate chain and key, both PEM, and the certificates a
    peer's must chain to: a CA, or the peer's own self-signed certificate. *)

type flow : value mod contended portable
(** A connected socket, plain or under TLS, blocking its reader and writer; one
    thread may read while another writes. *)

val accepted :
  t -> Unix.file_descr -> (flow, [> `Protection of error ]) result @@ portable
(** The listening side: the peer must present a trusted certificate. *)

val connected :
  t -> host:string -> Unix.file_descr -> (flow, [> `Protection of error ]) result
  @@ portable
(** The connecting side: the peer's certificate must be trusted and name [host].
*)

val read : flow -> Bytes.t -> int -> int -> (int, string) result
(** Bytes into the buffer, [0] once the peer finished. *) @@ portable

val write : flow -> string -> int -> int -> (unit, string) result
(** All of the substring. *) @@ portable

val shutdown : flow -> unit
(** Tells the peer nothing more will be sent. *) @@ portable

val descriptor : flow -> Unix.file_descr @@ portable

val rekey_after : flow -> int -> unit @@ portable
(** Under TLS, the first write after so many more bytes went out changes this
    side's sending keys; 64 GiB unless set. *)

val key_updates : flow -> int @@ portable
(** Key updates this side sent; 0 in plaintext. *)

val abort : flow -> unit
(** Ends both directions at once, waking a reader blocked on the socket. *) @@ portable

val close : flow -> unit @@ portable
