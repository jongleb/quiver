@@ portable

(** Keys and their values packed one after another in one buffer, in the order added: many pairs
    held with no object of their own for the collector to trace. *)

type t

val create : unit -> t

val add : t -> string -> string -> unit
(** A key and its value after the others. *)

val clear : t -> unit
(** Empty again, its room kept; spans of it are no longer valid. *)

type span
(** Pairs of a [t] one after another, while it is not cleared. *)

val all : t -> span
val of_list : Kv.pair list -> span
val to_list : span -> Kv.pair list
val length : span -> int

val key : span -> int -> string
(** The key of the pair at this place in the span. *)

val split : span -> Kv.bound -> span * span
(** The pairs whose key is below the bound and the rest, when the keys are in order. *)

val cut : span -> bytes:int -> span list
(** The span in runs of pairs one after another, a run ending before a pair its keys and values
    would take past [bytes]; one pair a run when it alone is larger. *)

val key_length : span -> int -> int
val value_length : span -> int -> int

val blit_key : span -> int -> Bytes.t -> int -> unit
(** The key of the pair at this place into the bytes at this position. *)

val blit_value : span -> int -> Bytes.t -> int -> unit
