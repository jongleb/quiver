(** Receives a dump an agent sends over quiver's wire protocol and writes it
    with a local sink: each data connection is served on a thread of its own,
    which decodes and renders its chunk's batches and answers a chunk's end once
    the sink closed the part. A collector can also pass everything on to the
    next hop as it arrives, so one read of the source reaches a chain of
    targets; a chunk then counts once every target committed it. A relay passes
    everything on and writes nothing. *)

type error : value mod contended portable =
  | Lost of Channel.error
      (** A connection to the agent or the next hop failed. *)
  | Damaged of Frame.error  (** A frame failed its checks. *)
  | Foreign_byte_order of Message.byte_order
      (** The agent's batches are in a byte order this machine does not use. *)
  | Out_of_place of string  (** A message the protocol does not allow there. *)
  | Downstream of string  (** The next hop's own description of its failure. *)
  | Manifest of Quiver_core.Manifest.error  (** The agent's manifest. *)
  | Abandoned  (** The agent left before it finished. *)
[@@deriving sexp_of ~portable]

type session
(** An agent that said hello on its control connection. *)

val accept :
  Channel.listener ->
  Protection.t ->
  reconnect:Core.Time_ns.Span.t ->
  introduction:Core.Time_ns.Span.t ->
  (session, [> `Collector of error ]) result
(** Its data connections and the next hop are protected alike. A control
    connection that breaks may be replaced by the agent within [reconnect]: the
    session goes on, its replies that did not arrive sent again. Every
    connection accepted, the first too, has [introduction] to finish its
    handshake and say what it is, or it is closed. *)

val chunks : session -> int
(** At most so many chunks are open at once: a sink with sessions per part needs
    as many. *)

type downstream
(** The next hop of a session, greeted as the agent greeted this one. *)

val forward :
  Channel.peer ->
  session ->
  (downstream, [> `Collector of error ]) result
(** Greets the next hop; when that fails, the agent is refused and the session
    ends. Each data connection that arrives later gets one of its own to it. *)

val relay :
  Channel.listener ->
  session ->
  downstream ->
  (unit, [> `Collector of error ]) result
(** Answers the hello, then pairs every data connection that arrives, the
    first ones and those replacing broken ones alike, with a new one to the
    next hop and copies every byte both ways, without reading frames, until
    both are done; a data connection that breaks is the agent's to send again,
    and to replace. So is a control connection; the relay fails when none
    replaced a broken one within the session's [reconnect]. Connections are
    taken until the session ends, when the listener stops. *)

module Make (S : Quiver_core.Sink_intf.S) : sig
  val run :
    ?forward:downstream ->
    Channel.listener ->
    session ->
    S.t ->
    columns:Message.columns ->
    (unit, [> `Collector of error | `Sink of S.error | `Mask of Quiver_core.Mask.error ]) result
  (** Answers the hello, takes every data connection that arrives and writes
      everything sent until the agent finishes, passing it on to [forward] too; the agent reads
      [columns] of each table, or [Stored_too] when [forward] takes them; the first
      failure of the sink, the protocol or the next hop, or [Ok] once the sink
      finished and the agent left. Data connections, and control connections
      that replace broken ones, are taken until then, when the listener stops;
      no connection holds the session back; the next hop's control connection is
      not replaced. *)
end
