type error = Non_positive_chunk_bytes of int [@@deriving sexp_of]
type t = { chunk_bytes : int }

let make ~chunk_bytes =
  if chunk_bytes < 1 then
    Error (`Plan_opts (Non_positive_chunk_bytes chunk_bytes))
  else Ok { chunk_bytes }

let shared { chunk_bytes } ~bytes ~readers =
  let chunks_per_reader = 4 and smallest = 4 lsl 20 in
  {
    chunk_bytes =
      Int.min chunk_bytes
        (Int.max smallest (bytes / Int.max 1 (chunks_per_reader * readers)));
  }
