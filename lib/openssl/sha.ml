(** The digest [quiver_sha] computes. *)
type algorithm = Sha1 | Sha256 [@@warning "-37"]

external digest : algorithm -> string -> string @@ portable = "quiver_sha"

let sha1 text = digest Sha1 text
let sha256 text = digest Sha256 text

external hmac : string -> int64 -> string -> string @@ portable = "quiver_hmac_sha256"

let hmac_sha256 ~key ~prefix text = hmac key prefix text
