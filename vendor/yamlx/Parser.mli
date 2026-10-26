(** YAML 1.2 parser. Transforms the token stream from the Scanner into an event
    stream.

    The parser implements a recursive-descent grammar matching the YAML 1.2.2
    specification. It is driven as a pull-style state machine: [get_event]
    advances it one step at a time. *)

type t
(** Opaque parser state. *)

val create : Scanner.state -> t
(** Create a parser from a Scanner. *)

val peek_event : t -> Types.event
(** Return (without consuming) the next event. Produces a new event if the
    buffer is empty. *)

val get_event : t -> Types.event
(** Consume and return the next event. *)

val check_event : t -> Types.event_kind list -> bool
(** True if the next event's kind matches one of [kinds]. Fields inside the
    constructor are ignored; only the constructor tag is checked. *)

val to_event_list : t -> Types.event list
(** Collect all events into a list. Reads until [Stream_end]. *)

val get_scanner : t -> Scanner.state
(** Expose the underlying scanner. Used to drain accumulated comments after
    parsing is complete via {!Scanner.drain_comments}. *)
