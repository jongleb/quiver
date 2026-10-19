@@ portable

(** A growable byte buffer whose bytes and length hot paths write directly. *)

type t = { mutable bs : Bytes.t @@ aliased; mutable len : int }

val create : ?cap:int -> unit -> t @ unique
val[@zero_alloc] length : t @ local -> int
val[@zero_alloc] clear : t @ local -> unit

val[@zero_alloc] ensure_cap : t @ local -> int -> unit
(** At least so many bytes in all. *)

val[@zero_alloc] ensure_free : t @ local -> int -> unit
(** At least so many bytes after [len]. *)

val[@zero_alloc] add_char : t @ local -> char -> unit
val[@zero_alloc] append_subbytes : t @ local -> Bytes.t -> int -> int -> unit
val[@zero_alloc] add_int32_le : t @ local -> int -> unit
val[@zero_alloc] append_string : t @ local -> string -> unit
val[@zero_alloc] append_buffer : t @ local -> t @ local -> unit
val contents : t @ local -> string
