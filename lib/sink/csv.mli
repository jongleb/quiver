(** CSV data files next to SQL schema files, in mydumper's layout.

    In both dialects of {!Quiver_core.Output_format.dialect}, NULL is an unquoted
    [\N], bytes are upper-case hex and decimals, numbers, dates and times are
    bare. *)

(** How a table's rows become CSV lines in one dialect. *)
module Dialect : Files.Format with type settings = Quiver_core.Output_format.dialect

type t : value mod contended portable

val create : Layout.t -> Quiver_core.Output_format.dialect -> t @@ portable

include Quiver_core.Sink_intf.S with type t := t
