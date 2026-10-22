(** Which data file a free loading session takes next, so sessions spread over
    tables instead of crowding one: every page split of an index waits for the
    index's latch, and a table's [AUTO_INCREMENT] or an empty MariaDB table's
    bulk insert lets one statement in at a time. Each table's files go in
    their order (the key order of the dump's chunks). *)

(** How many sessions may load one table's files at once. *)
type cap : immutable_data =
  | Alone  (** One, always. *)
  | At_most of int
      (** This many while another table has a file to give; any number once
          none has, so the last table's tail is shared. *)
  | First_alone of int
      (** The first file alone until it is loaded, then [At_most]. *)
[@@deriving sexp_of]

type 'a t

type slot : immediate
(** A file given out, until it is released. *)

val create : (cap * ('a * int) list) list -> 'a t
(** Tables, each with its files in order and their sizes in bytes. *)

val take : 'a t -> (slot * 'a) option
(** The next file for a free session: from a table nobody loads (one already
    begun first, then the most bytes left), otherwise from the table whose
    share of the bytes left most exceeds its share of the busy sessions;
    [None] when every table with files left is at a cap that cannot be lifted
    or has no files left. *)

val release : 'a t -> slot -> unit
(** The file is loaded and its session free. *)
