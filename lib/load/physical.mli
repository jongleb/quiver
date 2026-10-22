(** A table loaded by building its tablespace and importing it, instead of
    [LOAD DATA]: MySQL 8.4 with its data directory on this machine. The server
    creates the table with its primary key alone under a name of quiver's,
    exports it empty for a template, takes the built file with [IMPORT
    TABLESPACE] and only then gets the table's name; a table the writer cannot
    build goes the usual way. *)

type cause : value mod contended portable =
  [ `Link of Quiver_mysql.Link.error
  | `Wire of Quiver_mysql.Wire.error
  | `Transport of Quiver_mysql.Transport.error
  | `File of Quiver_innodb.File.error
  | `Tablespace of Quiver_innodb.Tablespace.error
  | `Check of Quiver_innodb.Check.error ]
[@@deriving sexp_of ~portable]

(** Why a table goes through [LOAD DATA] after all. *)
type unsuitable : immutable_data =
  | Name  (** A name with characters the server encodes in file names. *)
  | Definition  (** The table's [CREATE TABLE] is not one this knows. *)
  | Template of Quiver_innodb.File.error  (** No single [.ibd] and [.cfg] to read: partitioned. *)
  | Cfg of Quiver_innodb.Cfg.error
  | Layout of Quiver_innodb.Layout.error
  | Space of Quiver_innodb.Tablespace.error
[@@deriving sexp_of ~portable]

type table : value mod contended portable
(** A table prepared for building. *)

type prepared = Prepared of table | Unsuitable of unsuitable

val prepare :
  Quiver_mysql.Link.t ->
  datadir:string ->
  collations:(int * Quiver_innodb.Layout.charset) list ->
  into:Quiver_core.Schema.Name.t ->
  Quiver_core.Schema.Table.t ->
  create:string ->
  (prepared, [> cause ]) result
  @@ portable
(** [create] is the table's [CREATE TABLE] with its primary key alone. *)

val space : table -> Quiver_innodb.Tablespace.t @@ portable

val import :
  Quiver_mysql.Link.t -> table -> Quiver_innodb.Tablespace.finished list -> (int, [> cause ]) result
  @@ portable
(** Builds, checks and imports the runs, then names the table: the rows it
    holds. Whatever fails leaves no table and no file behind. *)

val abandon : Quiver_mysql.Link.t -> table -> unit @@ portable
(** Drops the table and its files, after a copy that failed. *)
