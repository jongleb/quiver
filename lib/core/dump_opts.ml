type error =
  | Non_positive_threads of int
  | Non_positive_render_threads of int
  | Non_positive_cores of int
  | Non_positive_batch_bytes of int
[@@deriving sexp_of]

type readers = Readers of int | Fitting
type renderers = Renderers of int | Spare
type order = Largest_chunks | Largest_tables [@@deriving sexp_of, enumerate]
type declaring = Before_its_rows | All_at_once [@@deriving sexp_of, enumerate]

type t = {
  plan : Plan_opts.t;
  readers : readers;
  renderers : renderers;
  cores : int;
  batch_bytes : int;
  order : order;
  declaring : declaring;
}

let make ~plan ~readers ~renderers ~cores ~batch_bytes ~order ~declaring =
  match (readers, renderers) with
  | Readers threads, _ when threads < 1 ->
      Error (`Dump_opts (Non_positive_threads threads))
  | _, Renderers threads when threads < 1 ->
      Error (`Dump_opts (Non_positive_render_threads threads))
  | _ when cores < 1 -> Error (`Dump_opts (Non_positive_cores cores))
  | _ when batch_bytes < 1 ->
      Error (`Dump_opts (Non_positive_batch_bytes batch_bytes))
  | _ -> Ok { plan; readers; renderers; cores; batch_bytes; order; declaring }

let most_readers { readers; cores; _ } =
  match readers with Readers threads -> threads | Fitting -> 2 * Int.max 4 (cores / 2)
