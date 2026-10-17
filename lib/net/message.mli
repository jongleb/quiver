(** Messages of quiver's wire protocol, sent as S-expressions in [Message]
    frames on the control connection; data connections carry only batch frames,
    after an [attach]. *)

(** The byte order batches are encoded in, the sender's own. *)
type byte_order = Little_endian | Big_endian [@@deriving sexp ~portable]

val byte_order : byte_order
(** This machine's. *)

(** What an agent sends on its control connection. [Hello], [Database], [Table],
    [Objects] and [Finish] are answered in order with [Ready], [Done], [Refused]
    or [Skipped], and may be sent before earlier ones are answered; a chunk's
    [End] is answered with [Committed] or [Rejected] whenever it is durable. *)
type control =
  | Hello of {
      session : string;
      byte_order : byte_order;
      streams : int;  (** Data connections that follow. *)
      chunks : int;  (** At most so many chunks begun and not answered. *)
    }
  | Database of { name : Quiver_core.Ident.t; ddl : string }
  | Table of { table : Quiver_core.Schema.Table.t; ddl : string; parts : int }
  | Objects of {
      database : Quiver_core.Ident.t;
      definitions : Quiver_core.Definition.t list;
    }
  | Begin of { chunk : int; table : Quiver_core.Schema.Name.t; index : int }
      (** Its batches, numbered from zero, travel on any data connection. *)
  | End of { chunk : int; frames : int }
  | Abort of { chunk : int }  (** Drop the chunk; unanswered. *)
  | Finish of {
      source : string;
      tables : (Quiver_core.Schema.Name.t * int) list;
      manifest : string;  (** The manifest's JSON text. *)
    }
[@@deriving sexp ~portable]

type attach = { session : string } [@@deriving sexp ~portable]
(** The first message on a data connection. *)

(** What a collector answers. *)
type reply =
  | Ready of Quiver_core.Output_format.t
      (** To [Hello]: the format the collector writes. *)
  | Done
  | Refused of string  (** The collector's own description of its failure. *)
  | Skipped
      (** Not applied: an earlier request was refused, and the session is lost.
      *)
  | Committed of { chunk : int; part : Quiver_core.Manifest.part }
  | Rejected of { chunk : int; reason : string }
[@@deriving sexp ~portable]
