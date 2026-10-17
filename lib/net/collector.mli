(** Receives a dump an agent sends over quiver's wire protocol and writes it
    with a local sink: each data connection is served on a domain of its own,
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
[@@deriving sexp_of ~portable]

type session
(** An agent that said hello on its control connection. *)

val accept :
  Channel.listener ->
  Protection.t ->
  (session, [> `Collector of error ]) result
(** Its data connections and the next hop are protected alike. *)

val chunks : session -> int
(** At most so many chunks are open at once: a sink with sessions per part needs
    as many. *)

type downstream
(** The next hop of a session, greeted as the agent greeted this one. *)

val forward :
  Channel.peer ->
  session ->
  (downstream, [> `Collector of error ]) result

val relay :
  Channel.listener ->
  session ->
  downstream ->
  (unit, [> `Collector of error ]) result
(** Accepts the data connections and copies every byte both ways between the
    agent and the next hop, without reading frames, until both are done. *)

module Make (S : Quiver_core.Sink_intf.S) : sig
  val run :
    ?forward:downstream ->
    Channel.listener ->
    session ->
    S.t ->
    (unit, [> `Collector of error | `Sink of S.error ]) result
  (** Answers the hello, accepts the data connections and writes everything sent
      until the agent finishes, passing it on to [forward] too; the first
      failure of the sink, the protocol or the next hop, or [Ok] once the sink
      finished. *)
end
