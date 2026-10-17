(** Statements of a dump file, read as a stream. Every statement quiver writes
    ends with [;] and a newline outside any literal, which the data renderer and
    {!Quiver_sink.Myloader} guarantee, as myloader itself relies on. *)

type error : immutable_data [@@deriving sexp_of ~portable]
(** Text after the last statement that never ends, cut short. *)

val iter :
  (Bytes.t -> pos:int -> len:int -> (int, ([> `Statements of error ] as 'e)) result) ->
  (string -> (unit, 'e) result) ->
  (unit, 'e) result
  @@ portable
(** Reads the text through [read], which fills at most [len] bytes at [pos] and
    returns how many, 0 only at the end. *)
