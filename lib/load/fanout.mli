(** The open [LOAD DATA] statements of a part copied into a target: one on the loader the
    part holds from start to end and, when statements end every so many bytes, one on each
    idle loader the part borrows until its statement ends. Texts go to them in turn. *)

(** How much of a part one transaction on the target takes. *)
type transactions =
  | Per_part
  | Every of int  (** Bytes of text, then a commit: TiDB holds a transaction in its memory. *)

type t

(** Whether the part may take another idle loader. *)
type wish = Borrow | Enough

(** What follows a text on its loader. *)
type step =
  | Open  (** The statement goes on. *)
  | Ended of { rows : int; after : after }  (** End it, checking its rows, and commit. *)

and after =
  | Again  (** Begin another statement: the part's own loader. *)
  | Back  (** Give the borrowed loader back. *)

(** A statement open when the part ends. *)
type statement = { loader : int; rows : int }

val create : transactions -> int -> t @@ portable
(** The part's own loader, its statement begun. *)

val wish : t -> wish @@ portable

val add : t -> int -> unit @@ portable
(** A borrowed loader, its statement begun: it takes the next text. *)

val send : t -> bytes:int -> rows:int -> int * step @@ portable
(** The loader the next text goes to, and what follows it there. *)

val ends : t -> statement * statement list @@ portable
(** The part's own statement and the borrowed ones still open. *)
