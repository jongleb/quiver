type t : immutable_data
(** Replacement text for the bytes of a cell that must be escaped. *)

val make : (char -> string option) -> t @@ portable

val[@zero_alloc] add :
  t -> string Quiver_core.Column.t @ local -> int -> Quiver_core.Byte_buffer.t @ local -> unit @@ portable

val[@zero_alloc] add_string : t -> string -> Quiver_core.Byte_buffer.t @ local -> unit @@ portable
