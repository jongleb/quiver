@@ portable

(** A range of keys read from TiKV region by region, at one version. *)

type error =
  | Unheld of string  (** The region found for this key does not hold it. *)
  | Region_kept_failing of Kv.region_error  (** The last of {!attempts} region errors in a row. *)
  | Still_locked of Kv.lock  (** Waited on for {!patience} without a transaction settled. *)
  | Key of Kv.key_error  (** A key error a read cannot wait out. *)
  | Disordered of string  (** A key out of order or outside the range asked for. *)
  | Disagreeing of int64  (** An async-commit transaction, by its start, whose locks contradict each other. *)
[@@deriving sexp_of ~portable]

(** Whether the region of a key may come from what is known or must be learned again. *)
type freshness = Known | Afresh

type 'e store = {
  locate : freshness -> string -> (Region.t, 'e) result;  (** Any key's, inside the range or not. *)
  scan : Region.t -> Kv.settled -> start:string -> stop:Kv.bound -> (Kv.scan Kv.reply, 'e) result;
      (** At most [limit] keys from [start] up to [stop], all in the region. *)
  check : Region.t -> Kv.check -> (Kv.status Kv.reply, 'e) result;
  secondaries : Region.t -> version:int64 -> string list -> (Kv.secondaries Kv.reply, 'e) result;
  now : unit -> (int64, 'e) result;  (** A timestamp of the cluster's oracle. *)
  pause : Core.Time_ns.Span.t -> unit;
}
(** How the read reaches TiKV. *)

val attempts : int
val patience : Core.Time_ns.Span.t

val range :
  'e store ->
  version:int64 ->
  start:string ->
  stop:Kv.bound ->
  limit:int ->
  f:(Kv.pair list -> (unit, ([> `Read of error ] as 'e)) result) ->
  (unit, 'e) result
(** Every key from [start] up to [stop] once, in order, as of [version], handed to [f] a scan at a
    time; a region error learns the region again and retries, a busy server waits and retries. A
    lock's transaction is checked at its primary key, rolled back there once expired, and, if it
    cannot have committed at or before [version], passed; if it has, read from the lock; if
    alive, waited on. *)
