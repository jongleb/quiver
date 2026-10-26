@@ portable

type error =
  | Non_positive_threads of int
  | Non_positive_render_threads of int
  | Non_positive_cores of int
  | Non_positive_batch_bytes of int
[@@deriving sexp_of]

(** How many sessions read. *)
type readers =
  | Readers of int  (** Exactly this many. *)
  | Fitting
      (** Half the cores, at least four, fewer when the server suggests fewer;
          as many again wait in the snapshot and join while readers hold the
          dump back and more of them read more. *)

(** How many threads turn full batches into output. *)
type renderers =
  | Renderers of int  (** Exactly this many. *)
  | Spare  (** Half the readers, within the cores. *)

(** Which chunks readers take first. *)
type order =
  | Largest_chunks  (** The largest chunks first, wherever their tables are. *)
  | Largest_tables  (** The largest tables first, their chunks together, so their tails start early. *)
[@@deriving sexp_of, enumerate]

(** When the sink hears of a table. *)
type declaring =
  | Before_its_rows  (** Just before the first chunk of the table. *)
  | All_at_once  (** Every table before any rows, so the sink prepares them while rows are read. *)
[@@deriving sexp_of, enumerate]

type t = private {
  plan : Plan_opts.t;
  readers : readers;
  renderers : renderers;
  cores : int;  (** The machine's cores the dump may keep busy. *)
  batch_bytes : int;  (** Full batches are shipped at this size. *)
  order : order;
  declaring : declaring;
}
(** How to run a dump; every count positive. *)

val make :
  plan:Plan_opts.t ->
  readers:readers ->
  renderers:renderers ->
  cores:int ->
  batch_bytes:int ->
  order:order ->
  declaring:declaring ->
  (t, [> `Dump_opts of error ]) result

val most_readers : t -> int
(** The readers a dump with these options reads on at most. *)
