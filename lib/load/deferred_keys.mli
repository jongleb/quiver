(** A [CREATE TABLE], as [SHOW CREATE TABLE] prints it (one definition per
    line), split so secondary keys and foreign keys are added after the data:
    InnoDB then builds each index by sorting instead of row by row. A key the
    [AUTO_INCREMENT] column needs stays; text that is not a recognised
    [CREATE TABLE] is left whole. *)

(** Which definitions wait until after the data. *)
type deferral =
  | All_keys  (** Secondary and foreign keys. *)
  | Foreign_keys  (** Foreign keys only; the other keys stay in the table. *)

(** Which keys the server sorts together: a key added later goes after those of its group. *)
type group = Unique | Plain

(** A deferred [KEY] or [UNIQUE KEY]. *)
type key = { name : string; definition : string  (** As the table's definition has it. *); group : group }

type outline : immutable_data
(** Where each deferred key stood in the table's definition. *)

type t = {
  create : string;  (** The table without the deferred definitions. *)
  later : string list;
      (** [ALTER TABLE] statements adding the deferred secondary keys back, in
          order: plain, unique and spatial keys at once, then each full-text key
          alone (InnoDB builds one at a time). *)
  foreign : string list;
      (** One [ALTER TABLE] adding the foreign keys back, if the table has any;
          it goes after [later]. *)
  keys : key list;
      (** The deferred [KEY] and [UNIQUE KEY] definitions, in order; a plain key after a spatial
          one is not among them, so adding the spatial key later keeps the order. *)
  special : string list;  (** [later] without [keys]: spatial keys and the plain ones after, full-text keys. *)
  outline : outline;
}

val split : deferral -> Quiver_core.Ident.t -> string -> t @@ portable

val with_keys : t -> key list -> string @@ portable
(** [create] with these of its [keys] back in their places. *)

val adding : Quiver_core.Ident.t -> key list -> string list @@ portable
(** One [ALTER TABLE] adding the keys, if there are any. *)
