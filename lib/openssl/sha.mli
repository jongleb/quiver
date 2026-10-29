@@ portable

(** SHA digests through OpenSSL's libcrypto. *)

val sha1 : string -> string
val sha256 : string -> string

val hmac_sha256 : key:string -> prefix:int64 -> string -> string
(** The 32-byte HMAC-SHA256 under [key], at most 64 bytes, of [prefix] in 8
    big-endian bytes followed by the text. *)
