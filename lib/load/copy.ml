open Base.Result.Let_syntax

type error =
  | Unreachable of Load.cause
  | Rejected of { statement : string; cause : Load.cause }
  | Several_databases of Quiver_core.Ident.t list
  | Unrepresentable_date of {
      table : Quiver_core.Schema.Name.t @@ aliased;
      column : Quiver_core.Ident.t @@ aliased;
    }
  | Unsplittable_definition of Quiver_core.Schema.Name.t
  | Unstarted of Quiver_core.Concurrent.error
[@@deriving sexp_of ~portable]

type rendered = { text : string @@ aliased many; rows : int }

type job = {
  table : Quiver_core.Schema.Name.t;  (** In the dump. *)
  into : Quiver_core.Schema.Name.t;  (** In the target. *)
  file : File_path.Part.t;  (** The name the chunk's data file would have. *)
  statement : string;  (** [LOAD DATA LOCAL] of the chunk. *)
}
(** A chunk for a loading session. *)

type renderer = {
  scratch : Quiver_core.Buffer_pool.t;  (** The copy's text buffers. *)
  lines : Quiver_sink.Csv.lines;
}

type deferred = {
  into : Quiver_core.Schema.Name.t;
  later : string * string list;
  remaining : int Portable.Atomic.t;  (** Parts not committed yet. *)
}
(** Keys of a table added once its last part commits. *)

type t = {
  config : Quiver_mysql.Provider.conn_config;
  binlog : Load.binlog;
  target : Load.target;
  keys : Load.keys;
  control : Quiver_mysql.Link.t Await.Capsule.Await.With_mutex.t;
  loaders : Quiver_mysql.Link.t Await.Capsule.Await.With_mutex.t iarray;
  idle : int Await.Await.Mpmc_queue.t;  (** Loaders no part holds. *)
  deferred :
    (Quiver_core.Schema.Name.t, deferred) Portable_lockfree_htbl.t;
  building : (unit, error) result Await.Await.Ivar.t list Portable.Atomic.t;
      (** Keys being added, one thread per table. *)
  databases : Quiver_core.Ident.t list Portable.Atomic.t;
  objects :
    (Quiver_core.Ident.t * Quiver_sink.Layout.script list) list
    Portable.Atomic.t;
  scratch : Quiver_core.Buffer_pool.t;  (** Text buffers for every part. *)
}

type part = {
  owner : t;
  loader : int;  (** Held from [part] to [close] or [discard]. *)
  job : job;
  renderer : renderer;
  hash : Quiver_sink.Xxh3.t;
  mutable rows : int;
  mutable bytes : int;
}

let dialect = Quiver_core.Output_format.Mysql_load_data

let rejected statement result =
  Result.map_error
    (fun (#Load.cause as cause) ->
      let shown_bytes = 200 in
      Rejected { statement = Base.String.prefix statement shown_bytes; cause })
    result

let run link sql = rejected sql (Quiver_mysql.Link.execute link sql)
let execute link statement = run link (Quiver_mysql.Statement.sql statement)

let session config binlog =
  Result.map_error
    (fun (#Load.cause as cause) -> Unreachable cause)
    (Load.session config binlog)

let retried link statement =
  let rec attempt left =
    match run link statement with
    | Error (Rejected { cause = `Link (Refused { code = 1213 | 1205; _ }); _ })
      when left > 1 ->
        attempt (left - 1)
    | result -> result
  in
  let deadlock_attempts = 5 in
  attempt deadlock_attempts

let add_keys link (into : Quiver_core.Schema.Name.t) (first, rest) =
  let%bind () = execute link (Use into.database) in
  Base.List.fold_result (first :: rest) ~init:() ~f:(fun () statement ->
      retried link statement)

let on_idle_loader t work =
  let loader = Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await t.idle in
  let result = Quiver_core.Guarded.hold (Iarray.get t.loaders loader) work in
  Await.Await.Mpmc_queue.push t.idle loader;
  result

let create config target ~keys ~binlog ~sessions =
  Result.map_error
    (fun error -> `Sink error)
    (let connected () =
       Quiver_core.Guarded.create (fun () -> session config binlog)
     in
     let loaders, control =
       Quiver_core.Concurrent.both
         (fun () ->
           Quiver_core.Concurrent.spread ~workers:sessions
             (Iarray.init sessions Fun.id)
             (fun (_ : int) -> connected ()))
         connected
     in
     let%map control, loaders =
       match
         Base.Result.combine control (Base.Result.all loaders)
           ~ok:(fun control loaders -> (control, loaders))
           ~err:(fun error _ -> error)
       with
       | Ok opened -> Ok opened
       | Error error ->
           List.iter
             (Result.iter (fun link ->
                  Quiver_core.Guarded.hold link (fun link ->
                      Quiver_mysql.Link.close link)))
             (control :: loaders);
           Error error
     in
     let idle = Await.Await.Mpmc_queue.create () in
     List.iteri (fun index _ -> Await.Await.Mpmc_queue.push idle index) loaders;
     {
       config;
       binlog;
       target;
       keys;
       control;
       loaders = Iarray.of_list loaders;
       idle;
       deferred = Portable_lockfree_htbl.create (module Quiver_core.Schema.Name);
       building = Portable.Atomic.make [];
       databases = Portable.Atomic.make [];
       objects = Portable.Atomic.make [];
       scratch = Quiver_core.Buffer_pool.create ();
     })

let shutdown t =
  Iarray.iter
    (fun loader -> Quiver_core.Guarded.hold loader (fun link -> Quiver_mysql.Link.close link))
    t.loaders;
  Quiver_core.Guarded.hold t.control (fun link -> Quiver_mysql.Link.close link)

let format _ = Quiver_core.Output_format.Csv dialect

let database t database ~ddl =
  Result.map_error
    (fun error -> `Sink error)
    (let%bind () =
       match
         ( t.target,
           Portable.Atomic.get_and_update t.databases ~pure_f:(fun seen ->
               database :: seen) )
       with
       | Into _, (_ :: _ as seen) ->
           Error (Several_databases (List.rev (database :: seen)))
       | Into _, [] | Same_names, _ -> Ok ()
     in
     let target = t.target in
     Quiver_core.Guarded.hold t.control (fun control ->
         run control (Load.create_database target database ddl)))

let table t ({ name; _ } : Quiver_core.Schema.Table.t) ~ddl ~parts =
  Result.map_error
    (fun error -> `Sink error)
    (let into = { name with database = Load.renamed t.target name.database } in
     let { Deferred_keys.create; later } =
       match t.keys with
       | With_table -> { Deferred_keys.create = ddl; later = [] }
       | After_data -> Deferred_keys.split name.table ddl
     in
     let%map () =
       on_idle_loader t (fun link ->
           let%bind () = execute link (Use into.database) in
           let%bind () = retried link create in
           match (later, parts) with
           | first :: rest, 0 -> add_keys link into (first, rest)
           | [], _ | _ :: _, _ -> Ok ())
     in
     match later with
     | first :: rest when parts > 0 ->
         ignore
           (Portable_lockfree_htbl.set t.deferred ~key:name
              ~data:
                {
                  into;
                  later = (first, rest);
                  remaining = Portable.Atomic.make parts;
                })
     | [] | _ :: _ -> ())

let objects t database definitions =
  Result.map_error
    (fun (`Unsplittable name) -> `Sink (Unsplittable_definition name))
    (let%map scripts = Quiver_sink.Layout.scripts database definitions in
     Portable.Atomic.update t.objects ~pure_f:(fun objects ->
         (database, scripts) :: objects))

let part t
    ({ name; fields = first, rest; _ } as table : Quiver_core.Schema.Table.t)
    ~index =
  let job =
    {
      table = name;
      into = { name with database = Load.renamed t.target name.database };
      file =
        File_path.Part.of_string
          (Printf.sprintf "%s.%s.%05d"
             (Quiver_sink.Layout.file_name name.database)
             (Quiver_sink.Layout.file_name name.table)
             index);
      statement =
        Quiver_mysql.Statement.sql
          (Load_local
             {
               table = name.table;
               dialect;
               columns =
                 List.map
                   (fun ({ name; ltype; _ } : Quiver_core.Schema.Field.t) ->
                     ( name,
                       match ltype with
                       | Bytes | Geometry -> Quiver_mysql.Statement.Hex_value
                       | Uint64 -> Number_value
                       | Bool | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16
                       | Uint32 | Float32 | Float64 | Decimal _ | String | Date
                       | Year | Unchecked_date | Unchecked_datetime _ | Time _
                       | Datetime _ | Timestamp _ | Json | Enum _ | Set _ ->
                           Text_value ))
                   (first :: rest);
             });
    }
  in
  let loader = Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await t.idle in
  let statement = job.statement and database = job.into.database in
  match
    Quiver_core.Guarded.hold (Iarray.get t.loaders loader) (fun link ->
        match
          let%bind () = execute link (Use database) in
          let%bind () = execute link Begin in
          rejected statement (Quiver_mysql.Link.upload link statement)
        with
        | Ok () -> Ok ()
        | Error error ->
            ignore (execute link Rollback);
            Error error)
  with
  | Error error ->
      Await.Await.Mpmc_queue.push t.idle loader;
      Error (`Sink error)
  | Ok () ->
      Ok
        {
          owner = t;
          loader;
          job;
          renderer =
            { scratch = t.scratch; lines = Quiver_sink.Csv.lines dialect table };
          hash = Quiver_sink.Xxh3.create ();
          rows = 0;
          bytes = 0;
        }

let renderer part = part.renderer

let render (renderer : renderer) (batch : Quiver_core.Batch.sealed @ local) =
  let out = Quiver_core.Buffer_pool.take renderer.scratch in
  let rendered =
    match Quiver_sink.Csv.add_lines renderer.lines batch (borrow_ out) with
    | Ok () ->
        Ok
          {
            text = Quiver_core.Byte_buffer.contents (borrow_ out);
            rows = Quiver_core.Batch.sealed_rows batch;
          }
    | Error (`Unrepresentable_date (table, column)) ->
        Error (`Sink (Unrepresentable_date { table; column }))
  in
  Quiver_core.Buffer_pool.give renderer.scratch out;
  rendered

let append part { text; rows } =
  let statement = part.job.statement in
  match
    Quiver_core.Guarded.hold (Iarray.get part.owner.loaders part.loader) (fun link ->
        rejected statement (Quiver_mysql.Link.send_data link text))
  with
  | Error error -> Error (`Sink error)
  | Ok () ->
      (* Hashed only, never written: the string stays as it is. *)
      Quiver_sink.Xxh3.add part.hash (Bytes.unsafe_of_string text) ~pos:0
        ~len:(String.length text);
      part.rows <- part.rows + rows;
      part.bytes <- part.bytes + String.length text;
      Ok ()

let close part answer =
  let t = part.owner and statement = part.job.statement in
  let outcome =
    Quiver_core.Guarded.hold (Iarray.get t.loaders part.loader) (fun link ->
        match
          let%bind () = rejected statement (Quiver_mysql.Link.complete link) in
          execute link Commit
        with
        | Ok () -> Ok ()
        | Error error ->
            ignore (execute link Rollback);
            Error error)
  in
  Await.Await.Mpmc_queue.push t.idle part.loader;
  match outcome with
  | Error error -> answer (Error error)
  | Ok () ->
      let built =
        match Portable_lockfree_htbl.find t.deferred part.job.table with
        | Null -> Ok ()
        | This { into; later; remaining } -> (
            match Portable.Atomic.fetch_and_add remaining (-1) with
            | 1 ->
                let added = Await.Await.Ivar.create ()
                and config = t.config
                and binlog = t.binlog in
                Portable.Atomic.update t.building ~pure_f:(fun building ->
                    added :: building);
                Result.map ignore
                  (Quiver_core.Concurrent.start (fun () ->
                       Await.Await.Ivar.fill_if_empty added
                         (let%bind link = session config binlog in
                          let added = add_keys link into later in
                          Quiver_mysql.Link.close link;
                          added)))
                |> Result.map_error (fun (`Concurrent error) ->
                       Await.Await.Ivar.fill_if_empty added
                         (Error (Unstarted error));
                       Unstarted error)
            | _ -> Ok ())
      in
      answer
        (Result.map
           (fun () ->
             {
               Quiver_core.Manifest.file = part.job.file;
               rows = part.rows;
               bytes = part.bytes;
               stored_bytes = part.bytes;
               checksum = Xxh3_64 (Quiver_sink.Xxh3.value part.hash);
             })
           built)

let discard part =
  let statement = part.job.statement in
  Quiver_core.Guarded.hold (Iarray.get part.owner.loaders part.loader) (fun link ->
      ignore (rejected statement (Quiver_mysql.Link.complete link));
      ignore (execute link Rollback));
  Await.Await.Mpmc_queue.push part.owner.idle part.loader

let finish t ~source:_ ~tables:_ ~manifest:_ =
  Result.map_error
    (fun error -> `Sink error)
    (let%bind () =
       Base.List.fold_result
         (List.rev (Portable.Atomic.get t.building))
         ~init:()
         ~f:(fun () added ->
           Await.Await.Ivar.read Quiver_core.Blocking.await added)
     in
     let target = t.target and objects = Portable.Atomic.get t.objects in
     Base.Result.all_unit
       (Quiver_core.Concurrent.spread ~workers:(Iarray.length t.loaders)
          (Iarray.of_list objects)
          (fun (database, scripts) ->
            on_idle_loader t (fun link ->
                let%bind () = execute link (Use (Load.renamed target database)) in
                Base.List.fold_result scripts ~init:()
                  ~f:(fun () ({ statements; _ } : Quiver_sink.Layout.script) ->
                    Base.List.fold_result statements ~init:()
                      ~f:(fun () statement -> run link statement))))))
