(** A [CREATE TABLE], as [SHOW CREATE TABLE] prints it (one definition per
    line), split so secondary keys and foreign keys are added after the data:
    InnoDB then builds each index by sorting instead of row by row. A key the
    [AUTO_INCREMENT] column needs stays; text that is not a recognised
    [CREATE TABLE] is left whole. *)

(** Which definitions wait until after the data. *)
type deferral =
  | All_keys  (** Secondary and foreign keys. *)
  | Foreign_keys  (** Foreign keys only; the other keys stay in the table. *)

type t = {
  create : string;  (** The table without the deferred definitions. *)
  later : string list;
      (** [ALTER TABLE] statements adding the deferred secondary keys back, in
          order: plain, unique and spatial keys at once, then each full-text key
          alone (InnoDB builds one at a time). *)
  foreign : string list;
      (** One [ALTER TABLE] adding the foreign keys back, if the table has any;
          it goes after [later]. *)
}

val split : deferral -> Quiver_core.Ident.t -> string -> t @@ portable
