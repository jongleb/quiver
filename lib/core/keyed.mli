@@ portable

(** Values masked under a secret key: under one key a value always masks the same way, and
    without the key the mask tells nothing of the value. HMAC-SHA256 throughout. *)

type t : value mod contended portable

val of_secret : string -> t

val shape : t -> string -> string
(** Each letter and digit replaced by one of its kind (Latin, Cyrillic, digit), any other letter
    by a Latin one, the rest kept: as many characters, never more bytes. *)

val email : t -> string -> string
(** [shape] before the last [@], the domain kept. *)

val hash_text : t -> string -> string
(** As many hex digits as the text has characters. *)

val hash_bytes : t -> string -> string
(** As many bytes as the value. *)

val hash_int64 : t -> int64 -> int64

val within : t -> string -> low:int64 -> high:int64 -> int64
(** A number from [low] to [high], both included; any number when [low] is above [high]. *)

val choice : t -> string -> int -> int
(** One of [n] places, from [0]. *)

val digits : t -> string -> int -> string
(** [n] decimal digits. *)

val hex : t -> string -> int -> string
(** [n] hex digits. *)
