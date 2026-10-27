@@ portable

(** Where a run's time went, summed over the parts it wrote. *)

type t = {
  reading : Core.Time_ns.Span.t;  (** Readers fetching and decoding rows. *)
  waiting : Core.Time_ns.Span.t;
      (** Readers held back by the stages after them. *)
  rendering : Core.Time_ns.Span.t;
  writing : Core.Time_ns.Span.t;
      (** Handing rendered batches to the sink: files, a server, the network. *)
  committing : Core.Time_ns.Span.t;
}

(** The stage that held the run back. *)
type stage =
  | Source  (** Readers waited for the stages after them under a quarter of their time. *)
  | Rendering
  | Writing

val of_states : Manifest.state list -> t
val holding : t -> stage

(** How one table's parts were read. *)
type table = {
  parts : int;
  spent : t;
  began : Core.Time_ns.Span.t;  (** Its first part's query, since the dump began. *)
  ended : Core.Time_ns.Span.t;  (** Its last part's last row, since the dump began. *)
}

val of_tables : Manifest.entry list -> (Schema.Name.t * table) list
(** The tables with a part read, the last to finish reading first. *)
