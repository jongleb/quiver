(** The names of an SQL text that the server reads as code whichever way a backslash reads
    inside quotes: a definition's [sql_mode] is not known everywhere it is rewritten, and a name
    left out only makes the server refuse, while one taken from inside a literal would change
    data. *)

(** What a name looks like. *)
type kind =
  | Word  (** Letters, digits, [_] and [$]. *)
  | Backquoted of string  (** The name, a doubled backquote read as one. *)

type token = { start : int; length : int; kind : kind }
(** Where it lies, backquotes included. *)

val tokens : string -> token list @@ portable
(** In order, outside quotes and plain comments; inside [/*! ... */] too. *)

val created : string -> string option @@ portable
(** The name a statement such as [CREATE DEFINER=`u`@`h` PROCEDURE `p`] or [CREATE TABLE IF NOT
    EXISTS `t`] names after its first kind of object: [TABLE], [VIEW], [SEQUENCE], [PROCEDURE],
    [FUNCTION], [EVENT] or [TRIGGER], in any case. *)
