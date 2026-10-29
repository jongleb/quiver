type part : immutable_data = {
  file : File_path.Part.t;  (** Inside the dump's directory. *)
  rows : int;
  bytes : int;  (** Uncompressed length. *)
  stored_bytes : int;  (** Length on disk. *)
  checksum : Checksum.t;  (** Of the bytes on disk. *)
}
[@@deriving sexp ~portable]
(** What a sink produced for one chunk. *)

type reading : immutable_data = {
  reader : int;  (** Which of the dump's readers. *)
  started : Core.Time_ns.Span.t;  (** Since the dump began. *)
  duration : Core.Time_ns.Span.t;  (** From the query to the last row. *)
  pool_wait : Core.Time_ns.Span.t;  (** Blocked on an empty batch pool. *)
  batches : int;
  allocated_words : int;  (** By the reading thread, for this chunk. *)
}
[@@deriving sexp_of ~portable]
(** How one chunk was read. *)

type writing : immutable_data = {
  rendering : Core.Time_ns.Span.t;  (** Over all batches, on any thread. *)
  appending : Core.Time_ns.Span.t;  (** Over all batches, under its lock. *)
  reordering : Core.Time_ns.Span.t;
      (** Over all batches, rendered and waiting for an earlier one. *)
  committing : Core.Time_ns.Span.t;  (** From closing it to it being durable. *)
}
[@@deriving sexp_of ~portable]
(** How one part was written. *)

(** How a frame's batch was encoded on the wire. *)
type encoding = Plain | Zstd of int  (** At this level. *)
[@@deriving sexp_of ~portable, equal ~portable]

type carrying : immutable_data = {
  frames : int;
  wire_bytes : int;  (** As sent, encoded, with frame headers. *)
  raw_bytes : int;  (** The batches before encoding. *)
  encodings : (encoding * int) list;  (** Frames per encoding. *)
  streams : int;  (** Data connections open when it was committed. *)
  round_trip : Core.Time_ns.Span.t option;
      (** Of the control connection then, where the kernel tells. *)
  sending : Core.Time_ns.Span.t;  (** From the part's start to its commit. *)
}
[@@deriving sexp_of ~portable]
(** How one part travelled over quiver's wire protocol. *)

type state =
  | Pending
  | Done of { part : part; reading : reading; writing : writing }
[@@deriving sexp_of ~portable]
(** Stored with spans in nanoseconds, under names ending in [_ns]. *)

type entry = {
  table : Schema.Name.t;
  key : Ident.t list;  (** Empty for a whole-table chunk. *)
  range : Chunk.Range.t option;
  state : state;
}
[@@deriving yojson_of]
(** One planned chunk and how far it got. *)

type threads = {
  sessions : int;  (** Opened in the snapshot. *)
  readers : int;  (** Reading at the start. *)
  peak : int;  (** Reading at once at most. *)
  renderers : int;
}
[@@deriving yojson_of]
(** How many threads read and rendered. *)

type phases = {
  connected : Core.Time_ns.Span.t;  (** Every session open, since the dump began. *)
  planned : Core.Time_ns.Span.t;  (** Schema read and chunks planned. *)
  snapshot : Core.Time_ns.Span.t;  (** Every session in the snapshot. *)
  first_part : Core.Time_ns.Span.t option;  (** The first chunk's query; none without rows. *)
  read : Core.Time_ns.Span.t;  (** The last reader stopped. *)
  committed : Core.Time_ns.Span.t;  (** The sink took every part. *)
}
(** When the dump reached each phase, since it began. *)

type masked = { table : Schema.Name.t; column : Ident.t; action : string }
(** A column whose values the dump masked, and the name of the action. *)

type t = private {
  version : int;
  level : Consistency.any;
  format : Output_format.t;
  position : Yojson.Safe.t;
  threads : threads;
  phases : phases;
  schema : Schema.t;
  masked : masked list;
  chunks : entry list;
}
(** Everything needed to inspect or resume a dump. *)

val yojson_of_t : t -> Yojson.Safe.t

val make :
  level:Consistency.any ->
  format:Output_format.t ->
  position:Yojson.Safe.t ->
  threads:threads ->
  phases:phases ->
  schema:Schema.t ->
  masked:masked list ->
  (Chunk.t * state) list ->
  t
(** A chunk's range is left out when a column of its key is masked: it would show the values
    the mask hides. *)

(** Why a manifest cannot be read back. *)
type error =
  | Malformed of string  (** Not a quiver manifest: the decoder's reason. *)
  | Unfinished of Schema.Name.t  (** A chunk of this table was never written. *)
[@@deriving sexp_of ~portable]

type contents = {
  format : Output_format.t;
  tables : Schema.Name.t list;
      (** Every table of the dump; one without parts was dumped without rows. *)
  parts : (Schema.Name.t * part) list;  (** Every chunk's part, in plan order. *)
}
(** What loading needs from a manifest. *)

val read : Yojson.Safe.t -> (contents, [> `Manifest of error ]) result

val localize :
  Yojson.Safe.t ->
  format:Output_format.t ->
  (Schema.Name.t -> int -> part option) ->
  (Yojson.Safe.t, [> `Manifest of error ]) result
(** The same manifest as written by another sink: its format, and as the part of
    each table's [n]-th chunk that sink's part number [n]. *)

val annotate :
  Yojson.Safe.t ->
  (Schema.Name.t -> int -> carrying option) ->
  (Yojson.Safe.t, [> `Manifest of error ]) result
(** The same manifest with how each table's [n]-th chunk travelled, stored with
    its state as [carrying]. *)
