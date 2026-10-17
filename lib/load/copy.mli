(** A sink that writes a dump straight into a MySQL or MariaDB server, with no
    files: databases and tables are created as the dump reaches them, each chunk
    streams through [LOAD DATA LOCAL] in one transaction on a loading session of
    its own, and views, triggers and routines follow the data. *)

type t : value mod contended portable
type part
type renderer : value mod contended portable
(** What rendering a part's batches needs. *)

type rendered : value mod portable

type error =
  | Unreachable of Load.cause  (** A loading session could not be set up. *)
  | Rejected of { statement : string; cause : Load.cause }
      (** The target failed a statement; [statement] is cut short. *)
  | Several_databases of Quiver_core.Ident.t list
      (** [Into] asks for one target, the dump reached these. *)
  | Unrepresentable_date of {
      table : Quiver_core.Schema.Name.t @@ aliased;
      column : Quiver_core.Ident.t @@ aliased;
    }  (** A date or time value outside the proleptic Gregorian years 0–9999. *)
  | Unsplittable_definition of Quiver_core.Schema.Name.t
      (** A routine, trigger or view with [";\n"] inside a string literal. *)
  | Unstarted of Quiver_core.Concurrent.error
      (** The thread adding a table's keys did not start. *)
[@@deriving sexp_of ~portable]

val create :
  Quiver_mysql.Provider.conn_config ->
  Load.target ->
  keys:Load.keys ->
  binlog:Load.binlog ->
  sessions:int ->
  (t, [> `Sink of error ]) result
(** Opens [sessions] loading sessions, each taken by one part at a time from
    [part] to [close]: as many as parts open at once. *)

val shutdown : t -> unit
(** Ends the loading sessions, after the dump succeeded or failed. *)

include
  Quiver_core.Sink_intf.S
    with type t := t
     and type part := part
     and type renderer := renderer
     and type rendered := rendered
     and type error := error
