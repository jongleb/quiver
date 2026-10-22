(** Messages of quiver's wire protocol, sent as S-expressions in [Message]
    frames on the control connection; data connections carry only batch frames,
    after an [attach], and back the last node's acks of them. *)

(** The byte order batches are encoded in, the sender's own. *)
type byte_order = Little_endian | Big_endian
[@@deriving sexp ~portable, enumerate ~portable]

val byte_order : byte_order
(** This machine's. *)

(** What an agent sends on its control connection. [Hello], [Database], [Table],
    [Grown], [Objects] and [Finish] are answered in order with [Ready], [Done], [Refused]
    or [Skipped], and may be sent before earlier ones are answered; a chunk's
    [End] is answered with [Committed] or [Rejected] whenever it is durable.
    Every message after [Hello] but [Heartbeat] and [Leave] is counted, and so
    is every reply after [Ready] but [Heard] and [Failed]: a control connection
    that replaces a broken one carries on from those counts. *)
type control =
  | Hello of {
      session : string;
      byte_order : byte_order;
      streams : int;  (** Data connections that follow. *)
      chunks : int;  (** At most so many chunks begun and not answered. *)
    }
  | Database of { name : Quiver_core.Ident.t; ddl : string }
  | Table of { table : Quiver_core.Schema.Table.t; ddl : string; parts : int }
  | Grown of { table : Quiver_core.Schema.Name.t }
      (** The table has one part more than announced. *)
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
  | Heartbeat of { replies : int }
      (** Every second: so many replies arrived. *)
  | Leave
      (** The last message: the session ends, finished or given up. *)
[@@deriving sexp ~portable]

(** The first message on a connection after the control one. *)
type attach =
  | Data of { session : string }
  | Control of { session : string; replies : int }
      (** Replaces the session's control connection, which broke after so many
          replies arrived. *)
[@@deriving sexp ~portable]

(** Which columns of a table the collector's sink takes. *)
type columns =
  | Written  (** Those [LOAD DATA] or [INSERT] writes back. *)
  | Stored_too  (** And the [STORED] generated ones after them, as the source holds them. *)
[@@deriving sexp ~portable]

(** What a collector answers. *)
type reply =
  | Ready of { format : Quiver_core.Output_format.t; columns : columns }
      (** To [Hello]: the format the collector writes, and the columns of a table it,
          or a node after it, takes. *)
  | Done
  | Refused of string  (** The collector's own description of its failure. *)
  | Skipped
      (** Not applied: an earlier request was refused, and the session is lost.
      *)
  | Committed of { chunk : int; part : Quiver_core.Manifest.part }
  | Rejected of { chunk : int; reason : string }
  | Heard of { messages : int }
      (** So many messages arrived: every second, and first on a control
          connection that replaces a broken one. *)
  | Failed of string
      (** The last reply: the session is lost, for the collector's reason. *)
[@@deriving sexp ~portable]
