@@ portable

(** Sorted keys written into TiKV region by region through {!Import}, again after a region changed
    under them. *)

(** Why a region would not take its keys. *)
type failure =
  | Write_failed of string  (** A peer's store refused the write. *)
  | Leaderless  (** No peer written to leads the region. *)
  | Ingest_failed of Kv.region_error  (** The leader refused the files. *)
[@@deriving sexp_of ~portable]

type error =
  | Unheld of string  (** The region found for this key does not hold it. *)
  | Kept_failing of failure  (** The last of {!attempts} failures in a row. *)
  | Refused of Kv.region_error  (** A refusal no retry helps, as a full disk. *)
[@@deriving sexp_of ~portable]

type ('sst, 'e) store = {
  locate : Read.freshness -> string -> (Region.t, 'e) result;
  write : Region.t -> Import.job -> ((Region.replica * 'sst Import.written) list, 'e) result;
      (** The job to every replica of the region, what each said. *)
  ingest : Region.t -> 'sst list -> (Import.ingested, 'e) result;  (** Through the region's leader. *)
  uuid : unit -> string;  (** Sixteen bytes no job had. *)
  pause : Core.Time_ns.Span.t -> unit;
}
(** How the keys reach TiKV. *)

val attempts : int
(** Failures of a region in a row before its keys are given up. *)

val patience : Core.Time_ns.Span.t
(** How long a busy leader is waited for before its keys are given up. *)

val deliver :
  ('sst, ([> `Ingest of error ] as 'e)) store ->
  commit_ts:int64 ->
  batch:int ->
  Pairs.span ->
  (unit, 'e) result
(** Every pair, in key order, at [commit_ts]: a job for each region they fall in, its messages of
    about [batch] bytes, the leader's files ingested; a write or ingest that fails learns the
    region again and writes the job anew, a busy leader is waited out for up to {!patience}. *)
