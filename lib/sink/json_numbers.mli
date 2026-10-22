(** Numbers in a JSON text that MySQL reads as another double. The server
    prints a double as its shortest exact text, but parses JSON with RapidJSON
    without full precision, whose quick path reads about one such text in nine
    as a neighbour; any copy through the text changes them unless the value is
    set apart, as an SQL double literal, which the server reads exactly. *)

type misread = {
  path : string;  (** A JSON path to the number: [$], [$."key"], [$[3]], … *)
  literal : string;  (** The number as an SQL double literal. *)
}

val misread : Bytes.t -> pos:int -> length:int -> misread list @@ portable
(** In document order, for the server's own text of a JSON value. *)
