(** CSV data files next to SQL schema files, in mydumper's layout. *)

(* In both dialects of {!Quiver_core.Output_format.dialect}, NULL is an
   unquoted [\N], bytes are upper-case hex and decimals, numbers, dates and times
   are bare. *)

type t : value mod contended portable

type part
(** A data file being written. *)

type renderer : value mod contended portable
(** What rendering a part's batches needs. *)

type rendered : value mod portable
(** One batch rendered as CSV lines, encoded. *)

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

val create : Layout.t -> Quiver_core.Output_format.dialect -> t @@ portable

type lines : immutable_data
(** How a table's rows are written as CSV lines in one dialect. *)

val lines :
  Quiver_core.Output_format.dialect -> Quiver_core.Schema.Table.t -> lines @@ portable

val add_lines :
  lines ->
  Quiver_core.Batch.sealed @ local ->
  Quiver_core.Byte_buffer.t @ local ->
  ( unit,
    [> `Unrepresentable_date of Quiver_core.Schema.Name.t * Quiver_core.Ident.t ]
  )
  result @@ portable

include
  Quiver_core.Sink_intf.S
    with type t := t
     and type part := part
     and type renderer := renderer
     and type rendered := rendered
     and type error := error
