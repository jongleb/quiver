@@ portable

type error = Non_positive_chunk_bytes of int [@@deriving sexp_of]

type t = private { chunk_bytes : int }
(** Planning knobs. [chunk_bytes] is the target size of one chunk, always
    positive. *)

val make : chunk_bytes:int -> (t, [> `Plan_opts of error ]) result

val shared : t -> bytes:int -> readers:int -> t
(** For a dump of about [bytes] read by [readers]: chunks no larger than a
    quarter of each reader's share, so every reader gets four or more and the
    last one to finish waits at most a quarter of the time, yet no smaller than
    4 MiB unless asked smaller. *)
