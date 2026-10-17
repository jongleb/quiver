@@ portable

type error =
  | Non_positive_threads of int
  | Non_positive_render_threads of int
  | Non_positive_batch_bytes of int
[@@deriving sexp_of]

type t = private {
  plan : Plan_opts.t;
  threads : int;
  render_threads : int;
  batch_bytes : int;
}
(** How to run a dump: [threads] sessions read in parallel, [render_threads]
    domains turn full batches into output, batches are shipped at [batch_bytes];
    all positive. *)

val make :
  plan:Plan_opts.t ->
  threads:int ->
  render_threads:int ->
  batch_bytes:int ->
  (t, [> `Dump_opts of error ]) result
