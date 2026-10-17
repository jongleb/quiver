@@ portable

(** An object whose native DDL a provider can show. *)

(** What a DBMS provider implements. It enters the core as a module argument;
    the core never sees concrete positions or sessions. *)
module type S = sig
  type conn_config

  type position
  (** Snapshot position: binlog file/pos and GTID set, TSO, LSN, … *)

  type session

  type error : value mod contended portable
  (** Provider-specific failure. *)

  val sexp_of_error : error -> Sexplib0.Sexp.t @@ portable

  type snapshot_mode
  (** How the provider opens its group of sessions (a global lock, a backup
      stage, plain transactions, …). *)

  type 'lvl supports
  (** Witness that [snapshot_mode] guarantees ['lvl], obtainable only via
      [supports]. *)

  val supports :
    snapshot_mode ->
    'lvl Consistency.t ->
    ('lvl supports, [> `Consistency of Consistency.error ]) result

  type connections
  (** The sessions of a dump and whatever else the provider holds, before they
      share a snapshot. *)

  val connect :
    conn_config -> n:int -> (connections, [> `Provider of error ]) result
  (** Opens [n] sessions, all at once. *)

  val disconnect : connections -> unit

  val introspect :
    connections ->
    Filter.t ->
    ( Schema.t,
      [> `Provider of error
      | `Schema of Schema.error
      | `Ltype of Ltype.error
      | `Ident of Ident.error ] )
    result

  val plan :
    connections ->
    Schema.t ->
    Plan_opts.t ->
    ( Chunk.t list,
      [> `Provider of error
      | `Chunk of Chunk.error
      | `Key of Key.error
      | `Ident of Ident.error ] )
    result

  val snapshot :
    'lvl supports ->
    connections ->
    ( session Await.Capsule.Await.With_mutex.t array * position,
      [> `Provider of error ] )
    result
  (** Puts the sessions in one snapshot, each in a capsule of its own: whoever
      takes its mutex owns it. Whatever else the connections held is closed,
      and on failure everything is. *)

  val ddl :
    session -> Ddl_target.t -> (string, [> `Provider of error ]) result
    @@ portable

  val read_chunk :
    session ->
    Chunk.t ->
    ([> `Provider of error | `Column of Column.error | `Batch of Batch.error ]
     as
     'e)
    Batch_writer.t ->
    (unit, 'e) result
    @@ portable

  val objects :
    session ->
    Ident.t ->
    ( (Ident.t * Definition.kind) list,
      [> `Provider of error | `Ident of Ident.error ] )
    result
    @@ portable
  (** The database's objects, in the order they are restored in. *)

  val definition :
    session ->
    Ident.t ->
    Ident.t * Definition.kind ->
    (Definition.t, [> `Provider of error | `Ident of Ident.error ]) result
    @@ portable

  val close : session -> unit @@ portable
  val sexp_of_position : position -> Sexplib0.Sexp.t
  val source : position -> string
  val yojson_of_position : position -> Yojson.Safe.t
  val position_of_yojson : Yojson.Safe.t -> position
end
