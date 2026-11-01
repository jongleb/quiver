(** A database loaded under another name: the names its definitions qualify with it
    ([`shop`.`orders`], [nextval(`shop`.`ids`)]) take the new one. *)
type t : immutable_data

type error =
  | Ambiguous of Quiver_core.Ident.t
      (** A table, view, sequence or routine of the database bears its name: [`shop`.`shop`]
          could name a column of that table. *)
[@@deriving sexp_of ~portable]

val make :
  from:Quiver_core.Ident.t ->
  into:Quiver_core.Ident.t ->
  names:Quiver_core.Ident.t list ->
  (t, [> `Qualifier of error ]) result
  @@ portable
(** [names] are the database's tables, views, sequences and routines. *)

val rewrite : t -> string -> string @@ portable
(** Every [from] before a dot and one of the names, blanks between them or not, outside quotes
    and plain comments whichever way a backslash reads, becomes [`into`]. *)
