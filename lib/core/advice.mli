@@ portable

(** How a run showed that a limit held it back. *)
type evidence =
  | Share of float
      (** Of the time connections had data to send, the share it held them
          back, as their kernel counted. *)
  | Near_ceiling of float
      (** Connections carried this share of what it lets one carry in a round
          trip. *)
[@@deriving sexp_of]

(** What the operator may change to make the next run faster, from what this
    one measured. *)
type t =
  | Receive_buffer of evidence
      (** The receiving host's kernel kept connections' windows below what the
          path needs. *)
  | Send_buffer of evidence
      (** The sending host's kernel capped what a connection had in flight. *)
  | Lossy_path of float
      (** This share of bytes was sent again: loss-based congestion control
          backs off on every loss, BBR does not. *)
  | Durable_commits
      (** The target syncs its redo log at every commit
          ([innodb_flush_log_at_trx_commit = 1]). *)
  | Synced_binlog  (** The target syncs its binlog at every commit. *)
  | Logged_load
      (** The target writes what is loaded to its binlog, which [--binlog skip]
          leaves out where replicas need not see it. *)
[@@deriving sexp_of]

type counts = {
  sent : int;  (** Bytes, retransmissions included. *)
  resent : int;
  held : held;
  connections : int;  (** That told, all together. *)
}
(** What the kernel counted for a run's connections, summed. *)

(** What held them back, summed over connections. *)
and held =
  | Timed of { busy_us : int; window_full_us : int; buffer_full_us : int }
      (** Linux. *)
  | Peer_windows of int  (** macOS: bytes, summed. *)

val of_counts :
  counts -> Tcp.path option -> seconds:float -> streams:int -> t list
(** Bytes sent again at least 0.1 %; on Linux, a limit holding connections at
    least 10 % of their busy time; on macOS, connections at 80 % or more of
    what a limit allows each round trip of the [path] measured at the start,
    over [seconds] on [streams] connections. *)
