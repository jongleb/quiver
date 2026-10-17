type error =
  | Arity of { expected : int; got : int }
  | Mismatch of { field : Ident.t; ltype : Ltype.t }
      (** The value's kind does not fit the field, or the field's type cannot
          bound a chunk. *)
[@@deriving sexp_of ~portable, equal ~portable]

(** One key column's value at a chunk boundary. *)
type value =
  | Signed of int64  (** Signed integer types and narrow unsigned ones. *)
  | Unsigned of int64  (** [Uint64], as a bit pattern. *)
  | Text of string  (** [String], UTF-8. *)
  | Binary of string  (** [Bytes]; hex in JSON, which must stay UTF-8. *)
  | Exact of string  (** [Decimal], canonical text. *)
  | Day of Core.Date.t  (** [Date]. *)
  | Instant of Moment.t  (** [Datetime], [Timestamp], UTC. *)
  | Duration of int64  (** [Time], µs. *)

val equal_value : value -> value -> bool @@ portable
val sexp_of_value : value -> Sexplib0.Sexp.t @@ portable
val yojson_of_value : value -> Yojson.Safe.t

type t = private value * value list [@@deriving sexp_of ~portable, equal ~portable, yojson_of]
(** A boundary: one value per key field, in key order. *)

val pp : Format.formatter -> t -> unit @@ portable

val make :
  Schema.Field.t * Schema.Field.t list ->
  value list ->
  (t, [> `Key of error ]) result @@ portable
