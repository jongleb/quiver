(** The sink of an agent: a dump sent over quiver's wire protocol to a
    collector, which writes it with a sink of its own. Batches travel as they
    lie in memory, zstd-compressed on the render domains, numbered within their
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
  | Unstarted of Quiver_core.Concurrent.error
      (** No thread for a data connection's sender. *)
[@@deriving sexp_of ~portable]

val create :
  Protection.t ->
  Channel.peer ->
  streams:int ->
  chunks:int ->
  Quiver_sink.Compression.Level.t ->
  (t, [> `Sink of error ]) result
(** Connects to the collector: a control connection, then [streams] data
    connections. *)

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
