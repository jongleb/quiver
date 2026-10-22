@@ portable

val tune : Unix.file_descr -> unit
(** Sets what every TCP connection of quiver needs: no Nagle delay, a bound on
    bytes waiting unsent, keepalive that finds a silent peer in about a minute
    and a half, and an end to data unacknowledged for two minutes. *)

type path = {
  rtt : Core.Time_ns.Span.t;  (** Smoothed round trip time. *)
  send_buffer : int;
      (** The most bytes the kernel lets one connection's send buffer hold:
          what it may have in flight unacknowledged. *)
}
(** What the kernel knows of a connection's path. *)

val path : Unix.file_descr -> path option
(** [None] where the kernel does not tell. *)

(** What held a connection back, as its kernel counted. *)
type held =
  | Timed of {
      busy : Core.Time_ns.Span.t;  (** It had data to send. *)
      receive_window : Core.Time_ns.Span.t;
          (** Of that, the peer's receive window was full. *)
      send_buffer : Core.Time_ns.Span.t;
          (** Of that, its own send buffer was full. *)
    }  (** Linux. *)
  | Peer_window of int  (** macOS: bytes the peer lets it have in flight now. *)

type strain = {
  sent : int;  (** Bytes, retransmissions included. *)
  resent : int;  (** Of them, bytes sent again. *)
  held : held;
}
(** A connection's sending so far. *)

val strain : Unix.file_descr -> strain option
(** [None] where the kernel does not tell (Linux before 4.19). *)

val ceiling : path -> float option
(** Bytes per second one connection carries at most on the path: a full send
    buffer each round trip; [None] for a round trip too short to measure. *)

(** A DSCP, the class routers give a connection's packets: 8 (CS1) asks to go
    after other traffic. *)
module Class : sig
  type t = private int

  val of_int : int -> t option
  (** [Some] from 0 to 63. *)
end

val mark : Unix.file_descr -> Class.t -> unit
(** Every packet of the connection carries the class. *)
