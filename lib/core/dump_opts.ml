type error =
  | Non_positive_threads of int
  | Non_positive_render_threads of int
  | Non_positive_batch_bytes of int
[@@deriving sexp_of]

type t = {
  plan : Plan_opts.t;
  threads : int;
  render_threads : int;
  batch_bytes : int;
}

let make ~plan ~threads ~render_threads ~batch_bytes =
  if threads < 1 then Error (`Dump_opts (Non_positive_threads threads))
  else if render_threads < 1 then
    Error (`Dump_opts (Non_positive_render_threads render_threads))
  else if batch_bytes < 1 then
    Error (`Dump_opts (Non_positive_batch_bytes batch_bytes))
  else Ok { plan; threads; render_threads; batch_bytes }
