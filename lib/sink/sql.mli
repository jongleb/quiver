(** MySQL-dialect SQL files in mydumper's layout, restorable with myloader, or
    with [mysql <db>] (and [zstdcat] for [.sql.zst]). *)

type t : value mod contended portable

val create : Layout.t -> statement_bytes:int -> t @@ portable

include Quiver_core.Sink_intf.S with type t := t
