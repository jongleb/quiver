type context = {
  charset : Ident.t;  (** [character_set_client] at creation. *)
  collation : Ident.t;  (** [collation_connection] at creation. *)
  sql_mode : string option;  (** Server syntax, comma separated. *)
  time_zone : string option;  (** Events only. *)
}
[@@deriving sexp ~portable, yojson]
(** Session settings a server stored with an object's text; restoring under
    other settings could change what the object does. *)

(** What the text creates. *)
type kind =
  | View of { columns : Ident.t * Ident.t list }
  | Trigger of { table : Ident.t }
  | Procedure
  | Function
  | Event
[@@deriving sexp ~portable, yojson]

type t = {
  database : Ident.t;
  name : Ident.t;
  kind : kind;
  context : context;
  text : string;  (** The [CREATE] statement exactly as the server shows it. *)
}
[@@deriving sexp ~portable, yojson]
