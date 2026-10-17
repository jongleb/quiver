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
val whole : Schema.Table.t -> t @@ portable

val of_bounds :
  Schema.Table.t ->
  Key.value list list ->
  (t list, [> `Chunk of error | `Key of Key.error ]) result @@ portable
