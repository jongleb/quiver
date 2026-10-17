(** A digest of a chunk's output as stored on disk. *)
type t = Xxh3_64 of int64

val equal : t -> t -> bool @@ portable
val sexp_of_t : t -> Sexplib0.Sexp.t @@ portable
val t_of_sexp : Sexplib0.Sexp.t -> t @@ portable

val yojson_of_t : t -> Yojson.Safe.t
(** The constructor and the digest as 16 hex digits. *)

val t_of_yojson : Yojson.Safe.t -> t
