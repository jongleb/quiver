(** A running XXH3-64 digest of bytes as they are written. *)

type t

val create : unit -> t @@ portable
val add : t -> Bytes.t -> pos:int -> len:int -> unit @@ portable
val value : t -> int64 @@ portable

val digest : Bytes.t -> pos:int -> len:int -> int64 @@ portable
(** Of these bytes alone. *)
