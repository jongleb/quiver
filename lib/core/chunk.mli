type error = Unsplittable_key of Schema.Name.t  (** The table has no key. *)
[@@deriving sexp_of ~portable, equal ~portable]

(** Part of a key domain between boundaries, ordered as the server orders the
    key (lexicographically over the key fields, each by its type and collation).
*)
module Range : sig
  type t = private
    | Below of Key.t  (** [key < bound]. *)
    | Between of Key.t * Key.t  (** [lo <= key < hi], [lo < hi]. *)
    | From of Key.t  (** [bound <= key]. *)
  [@@deriving sexp_of ~portable, equal ~portable, yojson_of]
end

(** A serialisable piece of one table. *)
type t = private
  | Whole of Schema.Table.t  (** Every row. *)
  | Range of {
      table : Schema.Table.t;
      key : Schema.Field.t * Schema.Field.t list;
      range : Range.t;
    }  (** Rows whose [key] falls in [range]. *)
[@@deriving sexp_of ~portable]

val pp : Format.formatter -> t -> unit @@ portable
(** The table and the key range, for people. *)

val table : t -> Schema.Table.t @@ portable

val bounds : t -> (int64 * int64) option @@ portable
(** [lo] and [hi] of a chunk over one integer key column bounded on both sides:
    its rows have [lo <= key < hi] in the key's order, signed or unsigned. *)

val integer_key : t -> Schema.Field.t option @@ portable
(** The key column of a chunk over one integer column, which {!closed} bounds. *)

val closed : t -> Key.value -> Key.value -> t option @@ portable
(** A chunk over its {!integer_key} whose rows' keys all lie from [lo] to [hi],
    both included, as the same rows bounded on both sides; [None] when [hi] is
    the largest value of the key, so nothing above it exists to bound by. *)

val divide : t -> int64 -> (t * t) option @@ portable
(** A chunk with {!bounds} [lo] and [hi] cut at [at], strictly between them in
    the key's order: its rows below [at] and its rows from [at]. *)
val whole : Schema.Table.t -> t @@ portable

val of_bounds :
  Schema.Table.t ->
  Key.value list list ->
  (t list, [> `Chunk of error | `Key of Key.error ]) result @@ portable
