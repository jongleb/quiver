(** A database's sequence, created with the database before its tables: a table's default can
    take its next value. *)
type t = private {
  name : Ident.t;
  text : string;  (** The [CREATE SEQUENCE] statement exactly as the server shows it. *)
  last : int64 option;
      (** The last value the source may have handed out, set on the target; none when it falls
          outside 64 bits, as the source then handed out none. *)
}
[@@deriving sexp ~portable, yojson]

(** Where a server says its sequence stands. *)
type counter =
  | Unused of { next : int64; increment : int64 }
      (** The first value neither handed out nor cached (MariaDB's [next_not_cached_value]);
          an increment of 0, [auto_increment_increment], counts as 1. *)
  | Above of int64
      (** One more than the last value handed out or cached, whichever the direction (TiDB's
          [NEXT_GLOBAL_ROW_ID]). *)

val make : name:Ident.t -> text:string -> counter -> t @@ portable

val statements : t -> string list @@ portable
(** Without terminators, in the current database: the [CREATE SEQUENCE], then a [DO SETVAL]
    of [last]. *)
