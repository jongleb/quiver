(** A table loaded by building its tablespace and importing it, instead of
    [LOAD DATA]: MySQL 8.4 with its data directory on this machine. The server
    creates the table with its primary key and the secondary keys the writer can
    build under a name of quiver's, exports it empty for a template, takes the
    built file with [IMPORT TABLESPACE] and only then gets the table's name; a
    table the writer cannot build goes the usual way. *)

type cause : value mod contended portable =
  [ `Link of Quiver_mysql.Link.error
  | `Wire of Quiver_mysql.Wire.error
  | `Transport of Quiver_mysql.Transport.error
  | `File of Quiver_innodb.File.error
  | `Tablespace of Quiver_innodb.Tablespace.error
  | `Check of Quiver_innodb.Check.error
  | `Warned of string list  (** The server's warnings on an import it took: an index it found corrupt. *) ]
[@@deriving sexp_of ~portable]

(** Why a table goes through [LOAD DATA] after all. *)
type unsuitable : immutable_data =
  | Name  (** A name with characters the server encodes in file names. *)
  | Definition  (** The table's [CREATE TABLE] is not one this knows. *)
  | Template of Quiver_innodb.File.error  (** No single [.ibd] and [.cfg] to read: partitioned. *)
  | Cfg of Quiver_innodb.Cfg.error
  | Layout of Quiver_innodb.Layout.error
  | Space of Quiver_innodb.Tablespace.error
  | Masked_key  (** The key is masked, so rows do not come in its order. *)
[@@deriving sexp_of ~portable]

(** Who builds the secondary keys of a table built as a tablespace. *)
type builder =
  | Quiver  (** quiver, into the file, those whose order it knows; the server the rest after the import. *)
  | Server  (** The server, all of them, after the import. *)
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
  definition:Deferred_keys.t ->
  builder:builder ->
  sorting:string option ->
  (prepared, [> cause ]) result
  @@ portable
(** [definition] is the table's [CREATE TABLE] split from its deferred keys: with
    [Quiver] the empty table gets every [KEY] and [UNIQUE KEY], then loses those the writer
    cannot build; sorted secondary records wait in [sorting], beside the built file when
    [None]. *)

val left : table -> Deferred_keys.key list @@ portable
(** The keys the server adds after the import. *)

val space : table -> Quiver_innodb.Tablespace.t @@ portable

type checked : value mod contended portable
(** A table whose built file passed {!Quiver_innodb.Check}. *)

val check :
  Quiver_mysql.Link.t ->
  table ->
  Quiver_innodb.Tablespace.finished list ->
  threads:int ->
  (checked, [> cause ]) result
  @@ portable
(** Builds the upper levels over the runs and checks the whole file, each on at most
    [threads] threads. Whatever fails leaves no table and no file behind. *)

val import : Quiver_mysql.Link.t -> checked -> (int, [> cause ]) result @@ portable
(** Imports the checked file and names the table: the rows it holds. Whatever
    fails leaves no table and no file behind. *)

val abandon : Quiver_mysql.Link.t -> table -> unit @@ portable
(** Drops the table and its files, after a copy that failed. *)
