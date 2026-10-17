@@ portable

(** The SHA-256 digest of a server certificate the client pins. *)

type error [@@deriving sexp_of ~portable]
(** Text that is not 64 hex digits, colons allowed. *)

type t : immutable_data

val of_string : string -> (t, [> `Fingerprint of error ]) result
(** Hex as [openssl x509 -fingerprint -sha256] prints it, or without colons. *)

val check : t -> digest:string -> [ `Pinned | `Different ]
(** [digest] is the raw 32-byte SHA-256 of the presented certificate. *)

val pp : Format.formatter -> t -> unit
