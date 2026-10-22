open Base.Result.Let_syntax

type need = Mysql_8_4 | Skipped_binlog | Local_datadir of string [@@deriving sexp_of ~portable]

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
  | Tablespaces_need of need
  | Built of { table : Quiver_core.Schema.Name.t @@ aliased; cause : Physical.cause @@ aliased }
  | Lost_rows of { table : Quiver_core.Schema.Name.t; sent : int; built : int }
  | Mismatched_render
[@@deriving sexp_of ~portable]

type ingest = Load_data | Tablespaces of { datadir : string option }

(** Rows of a batch kept out of [LOAD DATA]: a JSON number in them the server
    would misread from text, so an [INSERT] sets it apart ({!Quiver_sink.Json_numbers}). *)
type apart = Nothing_apart | Insert of { statement : string; rows : int }

type rendered =
  | Text of { text : string @@ aliased many; rows : int; apart : apart @@ aliased many }
  | Records of { records : Quiver_innodb.Tablespace.records @@ aliased many; rows : int }

type job = {
  table : Quiver_core.Schema.Name.t;  (** In the dump. *)
  into : Quiver_core.Schema.Name.t;  (** In the target. *)
  file : File_path.Part.t;  (** The name the chunk's data file would have. *)
  statement : string;  (** [LOAD DATA LOCAL] of the chunk. *)
  enums : Quiver_core.Ident.t list;  (** Its [ENUM] columns. *)
}
(** A chunk for a loading session. *)

type csv = {
  scratch : Quiver_core.Buffer_pool.t;  (** The copy's text buffers. *)
  json : Quiver_sink.Csv.json_reader;
  lines : Quiver_sink.Csv.Dialect.lines;
  insert : Quiver_sink.Sql.Insert.lines;
}

type renderer =
  | Csv of csv
  | Images of { space : Quiver_innodb.Tablespace.t; table : Quiver_core.Schema.Name.t }

(** Whether a built table reached the server. *)
type progress = Pending | Imported

type built = {
  physical : Physical.table;
  into : Quiver_core.Schema.Name.t;
  later : string list;  (** Its secondary keys, added once it is imported. *)
  remaining : int Portable.Atomic.t;  (** Parts not closed yet. *)
  finished : Quiver_innodb.Tablespace.finished list Portable.Atomic.t;
  sent : int Portable.Atomic.t;  (** Rows of its closed parts. *)
  progress : progress Portable.Atomic.t;
}
(** A table built as a tablespace. *)

(** Where tables go. *)
type mode =
  | Plain
  | Physical_at of { datadir : string; collations : (int * Quiver_innodb.Layout.charset) list }
      (** The data directory as this machine sees it, and the server's collations of the character sets stored. *)

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
  foreign :
    (Quiver_core.Schema.Name.t * (string * string list)) list Portable.Atomic.t;
      (** Foreign keys of each table, added once all the data is in: adding one
          waits for its parent's open loads. *)
  databases : Quiver_core.Ident.t list Portable.Atomic.t;
  objects :
    (Quiver_core.Ident.t * Quiver_sink.Layout.script list) list
    Portable.Atomic.t;
  scratch : Quiver_core.Buffer_pool.t;  (** Text buffers for every part. *)
  advice : Quiver_core.Advice.t list;
  json : Quiver_sink.Csv.json_reader;  (** How the target reads JSON text. *)
  mode : mode;
  built : (Quiver_core.Schema.Name.t, built) Portable_lockfree_htbl.t;
  builds : built list Portable.Atomic.t;  (** Every built table, to clean up after a failure. *)
  fallbacks : (Quiver_core.Schema.Name.t * Physical.unsuitable) list Portable.Atomic.t;
}

type loading = {
  owner : t;
  loader : int;  (** Held from [part] to [close] or [discard]. *)
  job : job;
  renderer : csv;
  hash : Quiver_sink.Xxh3.t;
  mutable rows : int;
  mutable bytes : int;
  mutable apart : (string * int) list;  (** [INSERT]s to run after the data, last first. *)
}

type building = {
  owner : t;
  table : Quiver_core.Schema.Name.t;
  state : built;
  run : Quiver_innodb.Tablespace.run;
  file : File_path.Part.t;  (** The name the chunk's data file would have. *)
  hash : Quiver_sink.Xxh3.t;
  mutable rows : int;
  mutable bytes : int;
}

type part = Loading of loading | Building of building

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
    (Load.session config binlog Checked_on_mariadb)

let retried link statement =
  Load.retried
    (fun () -> run link statement)
    ~cause:(function Rejected { cause; _ } -> Some cause | _ -> None)

let add_keys link (into : Quiver_core.Schema.Name.t) (first, rest) =
  let%bind () = execute link (Use into.database) in
  Base.List.fold_result (first :: rest) ~init:() ~f:(fun () statement ->
      retried link statement)

let on_idle_loader t work =
  let loader = Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await t.idle in
  let result = Quiver_core.Guarded.hold (Iarray.get t.loaders loader) work in
  Await.Await.Mpmc_queue.push t.idle loader;
  result

let create config target ~keys ~binlog ~ingest ~sessions =
  Result.map_error
    (fun error -> `Sink error)
    (let%bind control, loaders =
       Result.map_error
         (fun (#Load.cause as cause) -> Unreachable cause)
         (Load.sessions config binlog Checked_on_mariadb ~count:sessions
            ~control:(fun () ->
              Quiver_core.Guarded.create (fun () ->
                  (Load.session config binlog Checked_on_mariadb
                    : (Quiver_mysql.Link.t, Load.cause) result)))
            ~close:(fun control ->
              Quiver_core.Guarded.hold control (fun link -> Quiver_mysql.Link.close link)))
     in
     let mode =
       match ingest with
       | Load_data -> Ok Plain
       | Tablespaces { datadir } ->
           Quiver_core.Guarded.hold control (fun link ->
               let%bind () =
                 match Quiver_mysql.Link.server link with
                 | Mysql_8_4 _ -> Ok ()
                 | Mysql_8_0 _ | Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _ ->
                     Error (Tablespaces_need Mysql_8_4)
               in
               let%bind () =
                 match binlog with Skip_binlog -> Ok () | Write_binlog -> Error (Tablespaces_need Skipped_binlog)
               in
               let%bind datadir =
                 match datadir with
                 | Some datadir -> Ok datadir
                 | None -> (
                     match
                       rejected "SELECT @@datadir" (Quiver_mysql.Link.run_query link Datadir)
                     with
                     | Ok [ [| Some datadir |] ] -> Ok datadir
                     | Ok _ -> Error (Tablespaces_need (Local_datadir ""))
                     | Error error -> Error error)
               in
               let%bind () =
                 match Quiver_innodb.File.size datadir with
                 | Ok _ -> Ok ()
                 | Error (`File _) -> Error (Tablespaces_need (Local_datadir datadir))
               in
               let%map collations =
                 rejected "SELECT ID, CHARACTER_SET_NAME FROM information_schema.COLLATIONS"
                   (Quiver_mysql.Link.run_query link Text_collations)
               in
               Physical_at
                 {
                   datadir;
                   collations =
                     List.filter_map
                       (function
                         | [| Some id; Some "utf8mb4" |] -> Option.map (fun id -> (id, Quiver_innodb.Layout.Utf8mb4)) (int_of_string_opt id)
                         | [| Some id; Some "utf8mb3" |] -> Option.map (fun id -> (id, Quiver_innodb.Layout.Utf8mb3)) (int_of_string_opt id)
                         | _ -> None)
                       collations;
                 })
     in
     match mode with
     | Error error ->
         List.iter
           (fun loader -> Quiver_core.Guarded.hold loader (fun link -> Quiver_mysql.Link.close link))
           loaders;
         Quiver_core.Guarded.hold control (fun link -> Quiver_mysql.Link.close link);
         Error error
     | Ok mode ->
         let idle = Await.Await.Mpmc_queue.create () in
         List.iteri (fun index _ -> Await.Await.Mpmc_queue.push idle index) loaders;
         Ok
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
             foreign = Portable.Atomic.make [];
             databases = Portable.Atomic.make [];
             objects = Portable.Atomic.make [];
             scratch = Quiver_core.Buffer_pool.create ();
             advice = Quiver_core.Guarded.hold control (fun link -> Load.advice link binlog);
             json =
               Quiver_core.Guarded.hold control (fun link ->
                   match Quiver_mysql.Link.server link with
                   | Mysql_8_0 _ | Mysql_8_4 _ -> Quiver_sink.Csv.Rapidjson
                   | Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _ -> Verbatim);
             mode;
             built = Portable_lockfree_htbl.create (module Quiver_core.Schema.Name);
             builds = Portable.Atomic.make [];
             fallbacks = Portable.Atomic.make [];
           })

let shutdown t =
  List.iter
    (fun built ->
      match Portable.Atomic.get built.progress with
      | Imported -> ()
      | Pending ->
          Quiver_core.Guarded.hold t.control (fun link -> Physical.abandon link built.physical))
    (Portable.Atomic.get t.builds);
  Iarray.iter
    (fun loader -> Quiver_core.Guarded.hold loader (fun link -> Quiver_mysql.Link.close link))
    t.loaders;
  Quiver_core.Guarded.hold t.control (fun link -> Quiver_mysql.Link.close link)

let fallbacks t = List.rev (Portable.Atomic.get t.fallbacks)

let imported t =
  List.filter_map
    (fun built ->
      match Portable.Atomic.get built.progress with Imported -> Some built.into | Pending -> None)
    (Portable.Atomic.get t.builds)

let advice t = t.advice
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

let table t ({ name; _ } as table : Quiver_core.Schema.Table.t) ~ddl ~parts =
  let into = { name with database = Load.renamed t.target name.database } in
  let remember foreign =
    match foreign with
    | [] -> ()
    | first :: rest ->
        Portable.Atomic.update t.foreign ~pure_f:(fun foreign -> (into, (first, rest)) :: foreign)
  in
  let plain () =
    let deferral : Deferred_keys.deferral =
      match t.keys with With_table -> Foreign_keys | After_data -> All_keys
    in
    let { Deferred_keys.create; later; foreign } = Deferred_keys.split deferral name.table ddl in
    let%map () =
      on_idle_loader t (fun link ->
          let%bind () = execute link (Use into.database) in
          let%bind () = retried link create in
          match (later, parts) with
          | first :: rest, 0 -> add_keys link into (first, rest)
          | [], _ | _ :: _, _ -> Ok ())
    in
    remember foreign;
    match later with
    | first :: rest when parts > 0 ->
        ignore
          (Portable_lockfree_htbl.set t.deferred ~key:name
             ~data:{ into; later = (first, rest); remaining = Portable.Atomic.make parts })
    | [] | _ :: _ -> ()
  in
  let physical datadir collations =
    let { Deferred_keys.create; later; foreign } = Deferred_keys.split All_keys name.table ddl in
    let%bind prepared =
      on_idle_loader t (fun link ->
          Result.map_error
            (fun (#Physical.cause as cause) -> Built { table = name; cause })
            (Physical.prepare link ~datadir ~collations ~into table ~create))
    in
    match prepared with
    | Unsuitable reason ->
        Portable.Atomic.update t.fallbacks ~pure_f:(fun fallbacks -> (into, reason) :: fallbacks);
        plain ()
    | Prepared physical ->
        let state =
          {
            physical;
            into;
            later;
            remaining = Portable.Atomic.make parts;
            finished = Portable.Atomic.make [];
            sent = Portable.Atomic.make 0;
            progress = Portable.Atomic.make Pending;
          }
        in
        ignore (Portable_lockfree_htbl.set t.built ~key:name ~data:state);
        Portable.Atomic.update t.builds ~pure_f:(fun builds -> state :: builds);
        remember foreign;
        Ok ()
  in
  let created =
    match t.mode with
    | Physical_at { datadir; collations } when parts > 0 -> physical datadir collations
    | Physical_at _ | Plain -> plain ()
  in
  Result.map_error (fun error -> `Sink error) created

let objects t database definitions =
  Result.map_error
    (fun (`Unsplittable name) -> `Sink (Unsplittable_definition name))
    (let%map scripts = Quiver_sink.Layout.scripts database definitions in
     Portable.Atomic.update t.objects ~pure_f:(fun objects ->
         (database, scripts) :: objects))

let add_part t ({ name; _ } : Quiver_core.Schema.Table.t) =
  let () =
    match Portable_lockfree_htbl.find t.built name with
    | Null -> ()
    | This { remaining; _ } -> Portable.Atomic.incr remaining
  in
  match Portable_lockfree_htbl.find t.deferred name with
  | Null -> Ok ()
  | This { remaining; _ } -> Ok (Portable.Atomic.incr remaining)

let file_of (name : Quiver_core.Schema.Name.t) index =
  File_path.Part.of_string
    (Printf.sprintf "%s.%s.%05d"
       (Quiver_sink.Layout.file_name name.database)
       (Quiver_sink.Layout.file_name name.table)
       index)

let loading t
    ({ name; fields = first, rest; _ } as table : Quiver_core.Schema.Table.t)
    ~index =
  let columns =
    List.map
      (fun ({ name; ltype; _ } : Quiver_core.Schema.Field.t) ->
        ( name,
          match ltype with
          | Bytes | Geometry -> Quiver_mysql.Statement.Hex_value
          | Uint64 -> Number_value
          | Enum _ -> Enum_value
          | Bool | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32
          | Float32 | Float64 | Decimal _ | String | Date | Year
          | Unchecked_date | Unchecked_datetime _ | Time _ | Datetime _
          | Timestamp _ | Json | Set _ ->
              Text_value ))
      (first :: rest)
  in
  let job =
    {
      table = name;
      into = { name with database = Load.renamed t.target name.database };
      file = file_of name index;
      statement =
        Quiver_mysql.Statement.sql
          (Load_local
             {
               table = name.table;
               dialect;
               columns;
             });
      enums =
        List.filter_map
          (fun (name, (conversion : Quiver_mysql.Statement.conversion)) ->
            match conversion with
            | Enum_value -> Some name
            | Text_value | Hex_value | Number_value -> None)
          columns;
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
        (Loading
           {
             owner = t;
             loader;
             job;
             renderer =
               {
                 scratch = t.scratch;
                 json = t.json;
                 lines = Quiver_sink.Csv.Dialect.lines dialect table;
                 insert = Quiver_sink.Sql.Insert.lines Int.max_int table;
               };
             hash = Quiver_sink.Xxh3.create ();
             rows = 0;
             bytes = 0;
             apart = [];
           })

let part t (table : Quiver_core.Schema.Table.t) ~index =
  match Portable_lockfree_htbl.find t.built table.name with
  | Null -> loading t table ~index
  | This state ->
      Ok
        (Building
           {
             owner = t;
             table = table.name;
             state;
             run = Quiver_innodb.Tablespace.run (Physical.space state.physical);
             file = file_of table.name index;
             hash = Quiver_sink.Xxh3.create ();
             rows = 0;
             bytes = 0;
           })

let renderer = function
  | Loading loading -> Csv loading.renderer
  | Building building -> Images { space = Physical.space building.state.physical; table = building.table }

let render (renderer : renderer) (batch : Quiver_core.Batch.sealed @ local) =
  match renderer with
  | Images { space; table } -> (
      let rows = Quiver_core.Batch.sealed_rows batch in
      match Quiver_innodb.Tablespace.encode space (Quiver_core.Batch.sealed_columns batch) ~rows with
      | Ok records -> Ok (Records { records; rows })
      | Error (`Tablespace error) -> Error (`Sink (Built { table; cause = `Tablespace error })))
  | Csv csv ->
      let out = Quiver_core.Buffer_pool.take csv.scratch in
      let unrepresentable (`Unrepresentable_date (table, column)) = `Sink (Unrepresentable_date { table; column }) in
      let rendered =
        match Quiver_sink.Csv.write_apart csv.json csv.lines batch (borrow_ out) with
        | Error error -> Error (unrepresentable error)
        | Ok kept_out -> (
            let text = Quiver_core.Byte_buffer.contents (borrow_ out) in
            let rows = Quiver_core.Batch.sealed_rows batch - List.length kept_out in
            match kept_out with
            | [] -> Ok (Text { text; rows; apart = Nothing_apart })
            | _ :: _ ->
                let values = Quiver_core.Buffer_pool.take csv.scratch in
                let inserted =
                  match Quiver_sink.Sql.values csv.insert batch (borrow_ values) ~rows:kept_out with
                  | Error error -> Error (unrepresentable error)
                  | Ok () ->
                      let statement = Quiver_core.Byte_buffer.contents (borrow_ values) in
                      let statement = String.sub statement 0 (String.length statement - 2) in
                      Ok (Text { text; rows; apart = Insert { statement; rows = List.length kept_out } })
                in
                Quiver_core.Buffer_pool.give csv.scratch values;
                inserted)
      in
      Quiver_core.Buffer_pool.give csv.scratch out;
      rendered

let append_loading (part : loading) text rows =
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

let append part rendered =
  match (part, rendered) with
  | Loading loading, Text { text; rows; apart } -> (
      let () =
        match apart with
        | Nothing_apart -> ()
        | Insert { statement; rows } ->
            loading.apart <- (statement, rows) :: loading.apart;
            (* Hashed only, never written: the string stays as it is. *)
            Quiver_sink.Xxh3.add loading.hash (Bytes.unsafe_of_string statement) ~pos:0 ~len:(String.length statement)
      in
      append_loading loading text rows)
  | Building building, Records { records; rows } -> (
      match Quiver_innodb.Tablespace.add building.run records with
      | Error (#Physical.cause as cause) -> Error (`Sink (Built { table = building.table; cause }))
      | Ok () ->
          let image = Quiver_innodb.Tablespace.image records in
          (* Hashed only, never written: the string stays as it is. *)
          Quiver_sink.Xxh3.add building.hash (Bytes.unsafe_of_string image) ~pos:0 ~len:(String.length image);
          building.rows <- building.rows + rows;
          building.bytes <- building.bytes + String.length image;
          Ok ())
  | Loading _, Records _ | Building _, Text _ -> Error (`Sink Mismatched_render)

let close_loading (part : loading) answer =
  let t = part.owner and statement = part.job.statement and rows = part.rows
  and enums = part.job.enums and apart = List.rev part.apart in
  let outcome =
    Quiver_core.Guarded.hold (Iarray.get t.loaders part.loader) (fun link ->
        match
          let%bind () = rejected statement (Quiver_mysql.Link.complete link ~rows ~enums) in
          let%bind () =
            Base.List.fold_result apart ~init:() ~f:(fun () (insert, rows) ->
                rejected insert (Quiver_mysql.Link.insert link insert ~rows ~enums))
          in
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
               rows = part.rows + Base.List.sum (module Base.Int) part.apart ~f:snd;
               bytes = part.bytes;
               stored_bytes = part.bytes;
               checksum = Xxh3_64 (Quiver_sink.Xxh3.value part.hash);
             })
           built)

let close_building (part : building) answer =
  let t = part.owner and state = part.state in
  let manifest () =
    {
      Quiver_core.Manifest.file = part.file;
      rows = part.rows;
      bytes = part.bytes;
      stored_bytes = part.bytes;
      checksum = Xxh3_64 (Quiver_sink.Xxh3.value part.hash);
    }
  in
  match Quiver_innodb.Tablespace.close part.run with
  | Error (#Physical.cause as cause) -> answer (Error (Built { table = part.table; cause }))
  | Ok finished -> (
      Portable.Atomic.update state.finished ~pure_f:(fun all -> finished :: all);
      let (_ : int) = Portable.Atomic.fetch_and_add state.sent part.rows in
      match Portable.Atomic.fetch_and_add state.remaining (-1) with
      | 1 ->
          let imported = Await.Await.Ivar.create ()
          and config = t.config
          and binlog = t.binlog
          and table = part.table in
          Portable.Atomic.update t.building ~pure_f:(fun building -> imported :: building);
          let started =
            Quiver_core.Concurrent.start (fun () ->
                Await.Await.Ivar.fill_if_empty imported
                  (let%bind link = session config binlog in
                   let outcome =
                     let%bind built =
                       Result.map_error
                         (fun (#Physical.cause as cause) -> Built { table; cause })
                         (Physical.import link state.physical (Portable.Atomic.get state.finished))
                     in
                     let sent = Portable.Atomic.get state.sent in
                     let%bind () = if built = sent then Ok () else Error (Lost_rows { table; sent; built }) in
                     Portable.Atomic.set state.progress Imported;
                     match state.later with
                     | [] -> Ok ()
                     | first :: rest -> add_keys link state.into (first, rest)
                   in
                   Quiver_mysql.Link.close link;
                   outcome))
          in
          let built =
            Result.map_error
              (fun (`Concurrent error) ->
                Await.Await.Ivar.fill_if_empty imported (Error (Unstarted error));
                Unstarted error)
              (Result.map ignore started)
          in
          answer (Result.map manifest built)
      | _ -> answer (Ok (manifest ())))

let close part answer =
  match part with
  | Loading loading -> close_loading loading answer
  | Building building -> close_building building answer

let discard_loading (part : loading) =
  let statement = part.job.statement and rows = part.rows
  and enums = part.job.enums in
  Quiver_core.Guarded.hold (Iarray.get part.owner.loaders part.loader) (fun link ->
      ignore (rejected statement (Quiver_mysql.Link.complete link ~rows ~enums));
      ignore (execute link Rollback));
  Await.Await.Mpmc_queue.push part.owner.idle part.loader

let discard = function Loading loading -> discard_loading loading | Building _ -> ()

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
     let%bind () =
       Base.Result.all_unit
         (Quiver_core.Concurrent.spread ~workers:(Iarray.length t.loaders)
            (Iarray.of_list (List.rev (Portable.Atomic.get t.foreign)))
            (fun (into, keys) ->
              on_idle_loader t (fun link -> add_keys link into keys)))
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
