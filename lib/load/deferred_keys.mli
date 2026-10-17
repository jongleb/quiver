(** A [CREATE TABLE], as [SHOW CREATE TABLE] prints it (one definition per
    line), split so secondary keys and foreign keys are added after the data:
    InnoDB then builds each index by sorting instead of row by row. A key the
    [AUTO_INCREMENT] column needs stays; text that is not a recognised
    [CREATE TABLE] is left whole. *)

type t = {
  create : string;  (** The table with its columns, primary key and checks. *)
  later : string list;
      (** [ALTER TABLE] statements adding the rest back, in order: plain, unique
          and spatial keys at once, each full-text key alone (InnoDB builds one
          at a time), foreign keys last. *)
}

val split : Quiver_core.Ident.t -> string -> t @@ portable
