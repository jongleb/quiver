@@ portable

(** One name of a pattern: a run of characters where [*] stands for any run and
    [?] for any one character, or a name in backquotes taken as written. *)
type t : value mod contended portable [@@deriving sexp_of]

type error =
  | Empty_name of string  (** A pattern with a name left empty. *)
  | Names of { pattern : string; expected : int }
      (** Not this many names joined by dots outside backquotes. *)
  | Unclosed_quote of string
[@@deriving sexp_of]

type table = { database : t; table : t }
type column = { database : t; table : t; column : t }

val table : string -> (table, [> `Pattern of error ]) result
(** [DATABASE.TABLE]. *)

val column : string -> (column, [> `Pattern of error ]) result
(** [DATABASE.TABLE.COLUMN]. *)

(** Whether a name matches. *)
type fit = Fits | Misses

val fits_table : table -> Schema.Name.t -> fit
(** Databases and tables match as the server names them, letter case included. *)

val fits_column : column -> Schema.Name.t -> Ident.t -> fit
(** Columns match in any letter case, as the server compares them. *)

val assignment : string -> (string * string) option
(** The text before the first [=] outside backquotes, and the text after it. *)

val to_string : t -> string
