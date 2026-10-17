(** The digest [quiver_sha] computes. *)
type algorithm = Sha1 | Sha256 [@@warning "-37"]

external digest : algorithm -> string -> string @@ portable = "quiver_sha"

let sha1 text = digest Sha1 text
let sha256 text = digest Sha256 text
