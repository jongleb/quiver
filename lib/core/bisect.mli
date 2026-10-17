@@ portable

(** Boundaries that cut a key domain into ranges of at most a target of
    estimated rows, halving the heaviest ranges first: dense regions get more
    boundaries and sparse ones fewer, however the keys are spread. Only balance
    depends on the estimates; the ranges cover the domain whatever they are. *)

val boundaries :
  ('k : value mod contended portable) ('e : value mod contended portable).
  workers:int ->
  weigh:('k option -> 'k option -> (int, 'e) result) @ portable ->
  middle:('k option -> 'k option -> ('k option, 'e) result) ->
  target:int ->
  limit:int ->
  ('k list, 'e) result
(** [weigh lo hi] estimates the rows from [lo] (or the domain's start) up to
    [hi] (or its end), [hi] excluded, on up to [workers] threads at once;
    [middle lo hi] is a key strictly inside that range, or [None] when it cannot
    be cut. Each round cuts every range over the target, heaviest first, at
    most [limit] boundaries in all, in key order. The pieces' estimates are then scaled to add up to the whole
    domain's (servers overestimate wide ranges), neighbours are joined while
    together at most 1.1 targets, and a group under 0.2 targets goes to its
    lighter neighbour, so no run of tiny ranges is left. *)

val integer_middle :
  min:int64 -> max:int64 -> int64 option -> int64 option -> int64 option
(** A [middle] for an integer key whose smallest and largest values are [min]
    and [max] in key order, signed or unsigned: the ceiling of the halfway point
    between [lo] (or [min]) and the last key below [hi] (or [max]). *)
