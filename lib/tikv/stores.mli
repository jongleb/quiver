@@ portable

(** Connections to TiKV stores, one for each address asked, opened when first needed. *)

type t

val create : timeout:Core.Time_ns.Span.t -> t

val connection :
  t ->
  string ->
  ( Http2.t,
    [> `Region of Region.error | `Transport of Quiver_mysql.Transport.error ] )
  result
(** The connection to a store at [host:port] ([\[host\]:port] for IPv6). *)

val close : t -> unit
(** Closes every connection opened. *)
