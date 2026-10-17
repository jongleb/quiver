(** MySQL-dialect SQL files in mydumper's layout, restorable with
    [cat *.sql | mysql] (or [zstdcat *.sql.zst | mysql]). *)

type t : value mod contended portable

type part
(** A data file being written. *)

type renderer : value mod contended portable
(** What rendering a part's batches needs. *)

type rendered : value mod portable
(** One batch rendered as complete [INSERT] statements, encoded. *)

type error =
  | Output of Output.error
  | Unrepresentable_date of {
      table : Quiver_core.Schema.Name.t @@ aliased;
      column : Quiver_core.Ident.t @@ aliased;
    }  (** A date or time value outside the proleptic Gregorian years 0–9999. *)
  | Unsplittable_definition of Quiver_core.Schema.Name.t
      (** A routine, trigger or view with [";\n"] inside a string literal, which
          myloader would split. *)
[@@deriving sexp_of ~portable]

val create : Layout.t -> statement_bytes:int -> t @@ portable

include
  Quiver_core.Sink_intf.S
    with type t := t
     and type part := part
     and type renderer := renderer
     and type rendered := rendered
     and type error := error
