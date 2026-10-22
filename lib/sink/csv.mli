(** CSV data files next to SQL schema files, in mydumper's layout.

    In both dialects of {!Quiver_core.Output_format.dialect}, NULL is an unquoted
    [\N], bytes are upper-case hex and decimals, numbers, dates and times are
    bare. *)

(** How a table's rows become CSV lines in one dialect. *)
module Dialect : Files.Format with type settings = Quiver_core.Output_format.dialect

(** Who reads a JSON value's text on the target. *)
type json_reader =
  | Rapidjson  (** MySQL's [JSON] type, whose parser misreads some doubles. *)
  | Verbatim  (** A text column, MariaDB's [JSON]: the text is kept as written. *)

val write_apart :
  json_reader ->
  Dialect.lines ->
  Quiver_core.Batch.sealed @ local ->
  Quiver_core.Byte_buffer.t @ local ->
  ( int list,
    [> `Unrepresentable_date of Quiver_core.Schema.Name.t * Quiver_core.Ident.t ] )
  result
  @@ portable
(** {!Dialect.write}, but a row with a JSON number a [Rapidjson] reader would
    misread ({!Json_numbers}) is left out instead of refused: the rows left
    out, in order, for {!Sql.values}. *)

type t : value mod contended portable

val create : Layout.t -> Quiver_core.Output_format.dialect -> t @@ portable

include Quiver_core.Sink_intf.S with type t := t
