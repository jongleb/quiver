@@ portable

(** A range of keys read from TiKV region by region, at one version. *)

type error =
  | Unheld of string  (** The region found for this key does not hold it. *)
  | Region_kept_failing of Kv.region_error  (** The last of {!attempts} region errors in a row. *)
  | Still_locked of Kv.lock  (** Locked through {!attempts} tries in a row. *)
  | Key of Kv.key_error  (** A key error a read cannot wait out. *)
  | Disordered of string  (** A key out of order or outside the range asked for. *)
[@@deriving sexp_of ~portable]

(** Whether the region of a key may come from what is known or must be learned again. *)
type freshness = Known | Afresh

type 'e store = {
  locate : freshness -> string -> (Region.t, 'e) result;
  scan : Region.t -> start:string -> stop:Kv.bound -> (Kv.scanned, 'e) result;
      (** At most [limit] keys from [start] up to [stop], all in the region. *)
  pause : Core.Time_ns.Span.t -> unit;
}
(** How the read reaches TiKV. *)

val attempts : int

val range :
  'e store ->
  start:string ->
  stop:Kv.bound ->
  limit:int ->
  f:(Kv.pair list -> (unit, ([> `Read of error ] as 'e)) result) ->
  (unit, 'e) result
(** Every key from [start] up to [stop] once, in order, handed to [f] a scan at a time; a region
    error learns the region again and retries, a busy server or a lock waits and retries. *)

val table :
  Quiver_mysql.Link.t ->
  Quiver_core.Schema.Name.t ->
  version:int64 ->
  timeout:Core.Time_ns.Span.t ->
  limit:int ->
  f:
    (Kv.pair list ->
    ( unit,
      ([> `Read of error
       | `Region of Region.error
       | `Link of Quiver_mysql.Link.error
       | `Wire of Quiver_mysql.Wire.error
       | `Transport of Quiver_mysql.Transport.error
       | `Proto of Proto.error
       | `Grpc of Grpc.error
       | `Http2 of Http2.error
       | `Hpack of Hpack.error ]
       as
       'e) )
    result) ->
  (unit, 'e) result
(** The rows of a TiDB table as TiKV keeps them, at [version]: its regions learned through the
    link to TiDB, one connection to each store asked. *)
