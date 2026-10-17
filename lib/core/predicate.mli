@@ portable

(** A row filter over key fields, built from a chunk and rendered to SQL by the
    provider. *)

type comparison = Less | Equal | Greater | At_least [@@deriving sexp_of]

type t =
  | Always
  | Compare of {
      field : Schema.Field.t;
      comparison : comparison;
      value : Key.value;
    }
  | All of t * t list
  | Any of t * t list
[@@deriving sexp_of]

val of_chunk : Chunk.t -> t
