@@ portable

(** RSA-OAEP encryption under a PEM public key through OpenSSL's libcrypto. *)

val encrypt : public_key:string -> string -> string option
