@@ portable

(** A rule of PD's region labels (its HTTP API) keeping PD from scheduling the regions of some
    ranges of keys: no leaders or replicas moved, no merges, as long as it lasts. *)

type t : immutable_data

val deny : name:string -> string * string -> (string * string) list -> t
(** Over these ranges [\[start, stop)] of keys, under a name of letters, digits and [_]. *)

val json : t -> Core.Time_ns.Span.t -> string
(** The rule as PD takes it, ending after this long unless asked again. *)

val keep :
  Http2.dial ->
  host:string ->
  port:int ->
  t ->
  Pd.hold ->
  (unit, [> `Http1 of Http1.error | `Transport of Quiver_mysql.Transport.error ]) result
(** Asks PD at [host:port] to set the rule for this long more, or to drop it. *)
