(** The sink of an agent: a dump sent over quiver's wire protocol to a
    collector, which writes it with a sink of its own. Batches travel as they
    lie in memory, zstd-compressed on the render threads, numbered within their
    chunk; whichever data connection is free sends the next one, so one chunk
    spreads over every connection. A chunk counts once the collector committed
    it, while the readers go on: at most [chunks] are begun and not answered. *)

type t : value mod contended portable
type part
type renderer : value mod contended portable
(** What rendering a part's batches needs. *)

type rendered : value mod portable

type error =
  | Lost of Channel.error  (** A connection to the collector failed. *)
  | Damaged of Frame.error
      (** The collector sent a frame that fails its checks. *)
  | Refused of { request : string; reason : string }
      (** The collector's own description of why it failed a request. *)
  | Rejected of string  (** Why the collector could not commit a chunk. *)
  | Unexpected_reply of string  (** A reply out of place: which one. *)
  | Codec of string @@ aliased  (** The compressor failed. *)
  | Manifest of Quiver_core.Manifest.error
      (** The dump's manifest could not take how its parts travelled. *)
  | Unstarted of Quiver_core.Concurrent.error
      (** No thread for a data connection's sender. *)
  | Stalled
      (** A data connection's frames went unacknowledged while the others'
          were acknowledged. *)
[@@deriving sexp_of ~portable]

(** How batches are encoded on the wire. *)
type compression =
  | Fixed of Quiver_sink.Compression.t
  | Adaptive
      (** Without compression, zstd at levels -5, 1 or 3, moved a step while
          the network or rendering holds the dump back, kept only when it goes
          faster ({!Encoding}); zstd at level 1 first. *)
[@@deriving sexp_of ~portable]

type settings = {
  streams : int;  (** Data connections to start with. *)
  most_streams : int;
      (** At most so many while the network holds the dump back; as many as
          [streams] keeps their number. *)
  chunks : int;  (** Chunks begun and not answered at most. *)
  compression : compression;
  pace : int option;
      (** Bytes a second that all data connections together carry at most. *)
  traffic : Quiver_core.Tcp.Class.t option;  (** Of every connection's packets. *)
  reconnect : Core.Time_ns.Span.t;
      (** How long replacing a broken control connection may take before the
          session fails. *)
  stall : Core.Time_ns.Span.t;
      (** A data connection whose oldest frame waits this long for its ack,
          while another connection waits for none or eight times less, is
          taken for a dead path: its frames go to the others and a new
          connection replaces it (none when it was being closed anyway).
          Frames waiting on every connection are a slow collector, and nothing
          is replaced until they wait five times this long. *)
}

val create :
  Protection.t -> Channel.peer -> settings -> (t, [> `Sink of error ]) result
(** Connects to the collector: a control connection, then [streams] data
    connections. A data connection that breaks gives its unacknowledged frames
    to the others and is replaced, after a few pauses if need be; the session
    fails once none is left. A control connection that breaks, or brings
    nothing for 15 seconds, is replaced within [reconnect]: the collector
    answers again what did not arrive, and is sent again what it did not
    hear. While the network holds the dump back, the
    session opens more data connections, up to [most_streams], and keeps them
    only when they carry more ({!Streams}). *)

val adjust : t -> Quiver_core.Width.decision -> unit
(** Opens or closes so many data connections now, as the session does on its
    own while [most_streams] allows; the last one is never closed. *)

val streams : t -> int

val advice : t -> Quiver_core.Advice.t list
(** What the data connections that ended show the operator could change. *)

val waited_for_acks : t -> Core.Time_ns.Span.t
(** How long data connections waited, all together, with as many frames
    unacknowledged as they may hold. *)
(** Data connections open now. *)

val path : t -> Quiver_core.Tcp.path option
(** Of the first control connection, measured once the collector answered. *)

val shutdown : t -> unit
(** Ends every connection, each with a TLS [close_notify] once its frames are
    sent, and waits until they are. *)

include
  Quiver_core.Sink_intf.S
    with type t := t
     and type part := part
     and type renderer := renderer
     and type rendered := rendered
     and type error := error
