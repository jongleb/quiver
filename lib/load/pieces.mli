(** A CSV data file of a dump cut at row ends into pieces of about [bytes] each, for a target
    that holds a transaction in its memory (TiDB): each piece is loaded and committed on its
    own. *)

type piece = {
  data : string;  (** Whole rows, after the file's header line when it has one. *)
  rows : int;  (** Without the header. *)
}

val statements :
  bytes:int ->
  skip:int ->
  (((string -> (unit, 'e) result) -> (unit, 'e) result)) ->
  run:(string -> (unit, 'e) result) ->
  commit:(int -> (unit, 'e) result) ->
  (unit, 'e) result
  @@ portable
(** Runs, of the statements the iterator hands on, all but the first [skip], and commits after
    the one that brings a piece to [bytes] and after the last: [commit n] with [n] statements
    done, the skipped ones counted. *)

val cut :
  Quiver_core.Output_format.dialect ->
  bytes:int ->
  skip:int ->
  (Bytes.t -> pos:int -> len:int -> (int, 'e) result) ->
  (piece -> (unit, 'e) result) ->
  (unit, 'e) result
  @@ portable
(** Reads the file through the reader, which returns 0 only at its end, leaves out its first
    [skip] rows, and hands on each piece in order: at least [bytes] of rows unless it is the
    last. An [Rfc4180] row ends at a line break outside quotes; a [Mysql_load_data] one at any
    line break, as it escapes those inside values. *)
