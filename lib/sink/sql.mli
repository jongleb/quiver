(** MySQL-dialect SQL files in mydumper's layout, restorable with myloader, or
    with [mysql <db>] (and [zstdcat] for [.sql.zst]). *)

(** How a table's rows become [INSERT] statements. *)
module Insert : Files.Format with type settings = int

val values :
  Insert.lines ->
  Quiver_core.Batch.sealed @ local ->
  Quiver_core.Byte_buffer.t @ local ->
  rows:int list ->
  ( unit,
    [> `Unrepresentable_date of Quiver_core.Schema.Name.t * Quiver_core.Ident.t ] )
  result
  @@ portable
(** [INSERT] statements of the batch's [rows] alone, ascending; settings of
    {!Insert.lines} give the most bytes of one statement. *)

type t : value mod contended portable

val create : Layout.t -> statement_bytes:int -> t @@ portable

include Quiver_core.Sink_intf.S with type t := t
