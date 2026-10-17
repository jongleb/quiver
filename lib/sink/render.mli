(** Exact text of a double via zmij: the shortest decimal that parses back to
    the same double, fixed notation for small decimal exponents and [1.5e+20]
    style otherwise; [-0] keeps its sign. Single-precision values are printed as
    doubles: MySQL parses [FLOAT] text through a double, and the shortest
    single-precision digits could round twice to a neighbour. *)

val[@zero_alloc] float : Quiver_core.Byte_buffer.t @ local -> float# -> unit @@ portable

val[@zero_alloc] text :
  Quiver_core.Byte_buffer.t @ local -> string Quiver_core.Column.t @ local -> int -> unit @@ portable

val[@zero_alloc] hex :
  Quiver_core.Byte_buffer.t @ local -> string Quiver_core.Column.t @ local -> int -> unit @@ portable
