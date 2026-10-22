(** A [JSON] column's text turned into the binary form the server stores
    (MySQL 8.4 [json_binary::serialize]), read the way the server reads it —
    RapidJSON's grammar and classes of numbers, keys ordered by length and then
    bytes, the last of a repeated key kept — except that a double is rounded
    correctly: the server prints a double as its shortest exact text, and
    RapidJSON's quick path without full precision reads some of them as a
    neighbour, so [LOAD DATA] would change them. *)

type error =
  | Syntax of int  (** Not JSON from this offset on. *)
  | Number_too_big of int  (** A number past the largest double. *)
  | Too_deep  (** Nested deeper than the server's 100 levels. *)
  | Key_too_long  (** A key of 64 KiB or more. *)
[@@deriving sexp_of ~portable]

val binary : Bytes.t -> pos:int -> length:int -> (string, [> `Json of error ]) result @@ portable
