open Base.Result.Let_syntax

type need = Mysql_8_4 | Skipped_binlog | Local_datadir of string | Sort_directory of string
[@@deriving sexp_of ~portable]

type error =
  | Unreachable of Load.cause
  | Rejected of { statement : string; cause : Load.cause }
  | Several_databases of Quiver_core.Ident.t list
  | Unrepresentable_date of {
      table : Quiver_core.Schema.Name.t @@ aliased;
      column : Quiver_core.Ident.t @@ aliased;
    }
  | Unsplittable_definition of Quiver_core.Schema.Name.t
  | Tablespaces_need of need
  | Built of { table : Quiver_core.Schema.Name.t @@ aliased; cause : Physical.cause @@ aliased }
  | Lost_rows of { table : Quiver_core.Schema.Name.t; sent : int; built : int }
  | Mismatched_render
  | After_load of After_load.error
[@@deriving sexp_of ~portable]

type tablespaces = {
  datadir : string option;
  builder : Physical.builder;
  sorting : string option;
  threads : int option;
}

type ingest = Load_data | Tablespaces of tablespaces

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

(** A run of a spread part, started when a renderer first takes its slot, and the bytes of
    records it took. *)
type slot = { mutable run : Quiver_innodb.Tablespace.run option; mutable bytes : int }

(** A part of a table whose runs number their own rows, so its batches need no order: each
    renderer adds a batch to the lowest slot no other renderer holds, as it renders. *)
type spread = {
  space : Quiver_innodb.Tablespace.t;
  rank : int;
  slots : slot Await.Capsule.Await.With_mutex.t iarray;
  held : int Portable.Atomic.t;  (** A bit for each slot a renderer holds. *)
  closed : Quiver_innodb.Tablespace.finished list Portable.Atomic.t;
      (** Runs that grew full and closed while the part was read, sorting already. *)
}

type renderer =
  | Csv of csv
  | Images of { space : Quiver_innodb.Tablespace.t; table : Quiver_core.Schema.Name.t }
  | Spread_images of { spread : spread; table : Quiver_core.Schema.Name.t }

(** Whether a built table reached the server. *)
type imported = {
  preparing : Core.Time_ns.Span.t;
  queued : Core.Time_ns.Span.t;
  checking : Core.Time_ns.Span.t;
  importing : Core.Time_ns.Span.t;
  keys : Core.Time_ns.Span.t;
}

type progress = Pending | Imported of imported

type way =
  | Imported of imported
  | Loaded of Physical.unsuitable

type finishing =
  | Unfinished
  | Finished of {
      built : Core.Time_ns.Span.t;
      after_load : Core.Time_ns.Span.t;
      foreign_keys : Core.Time_ns.Span.t;
      objects : Core.Time_ns.Span.t;
    }

type built = {
  physical : Physical.table;
  source : Quiver_core.Schema.Name.t;  (** Its name in the dump. *)
  into : Quiver_core.Schema.Name.t;
  later : string list;  (** Its secondary keys, added once it is imported. *)
  remaining : int Portable.Atomic.t;  (** Parts not closed yet. *)
  finished : Quiver_innodb.Tablespace.finished list Portable.Atomic.t;
  sent : int Portable.Atomic.t;  (** Rows of its closed parts. *)
  bytes : int Portable.Atomic.t;  (** Of its closed parts' records: the more, the sooner what follows its last part runs. *)
  progress : progress Portable.Atomic.t;
  preparing : Core.Time_ns.Span.t;  (** Creating its empty table and exporting the template. *)
}
(** A table built as a tablespace. *)

(** Where tables go. *)
type mode =
  | Plain
  | Physical_at of {
      datadir : string;
      collations : (int * Quiver_innodb.Layout.charset) list;
      builder : Physical.builder;
      sorting : string option;
    }
      (** The data directory as this machine sees it, and the server's collations of the character sets stored. *)

type deferred = {
  into : Quiver_core.Schema.Name.t;
  later : string * string list;
  remaining : int Portable.Atomic.t;  (** Parts not committed yet. *)
  bytes : int Portable.Atomic.t;  (** Of its committed parts. *)
}
(** Keys of a table added once its last part commits. *)

(** What follows a table's last part. *)
type ending =
  | Keys_of of {
      into : Quiver_core.Schema.Name.t;
      later : string * string list;
      added : (unit, error) result Await.Await.Ivar.t;
    }  (** Its deferred keys, loaded through [LOAD DATA]. *)
  | Import_of of {
      state : built;
      table : Quiver_core.Schema.Name.t;
      closed : Quiver_core.Stopwatch.t;  (** Since its last part closed. *)
      imported : (unit, error) result Await.Await.Ivar.t;
    }  (** Its check, import and keys, built as a tablespace. *)

(** A table to create or prepare. *)
type declaration = {
  table : Quiver_core.Schema.Table.t;
  ddl : string;
  parts : int;
  declared : (unit, error) result Await.Await.Ivar.t;
}

type t = {
  target : Load.target;
  keys : Load.keys;
  after : After_load.rule list;
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
  increments : Load.increments;
  turns : (Quiver_core.Schema.Name.t, Await.Await.Semaphore.t) Portable_lockfree_htbl.t;
      (** Tables loaded one part at a time, each part waiting for its turn. *)
  finishing : finishing Portable.Atomic.t;
  declared :
    (Quiver_core.Schema.Name.t, (unit, error) result Await.Await.Ivar.t) Portable_lockfree_htbl.t;
      (** Each table once it is created or prepared, on a thread of its own. *)
  declaring : (unit, error) result Await.Await.Ivar.t list Portable.Atomic.t;
  broken : error option Portable.Atomic.t;  (** The first table that could not be declared. *)
  preparing : declaration Quiver_core.Pool.t;
      (** A few threads creating and preparing tables for the whole copy, the largest first. *)
  spare : Quiver_core.Lease.t;  (** Threads finishing and checking built files, between all tables. *)
  ending : ending Quiver_core.Pool.t;
      (** A thread per loader for what follows tables' last parts, the largest tables first. *)
}


(** Whether a part waited for its table's turn, given back once its statement ends. *)
type turn = Any_time | Taken of Await.Await.Semaphore.t

type loading = {
  owner : t;
  loader : int;  (** Held from [part] to [close] or [discard]. *)
  turn : turn;
  job : job;
  renderer : csv;
  hash : Quiver_sink.Xxh3.t;
  mutable rows : int;
  mutable bytes : int;
  mutable apart : (string * int) list;  (** [INSERT]s to run after the data, last first. *)
}

(** Where a building part's records go. *)
type placing =
  | In_order of Quiver_innodb.Tablespace.run  (** Its one run, batch after batch. *)
  | Spread of spread

type building = {
  owner : t;
  table : Quiver_core.Schema.Name.t;
  state : built;
  placing : placing;
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

let retried link statement =
  Load.retried
    (fun () -> run link statement)
    ~cause:(function Rejected { cause; _ } -> Some cause | _ -> None)

let run_in link (into : Quiver_core.Schema.Name.t) (first, rest) =
  let%bind () = execute link (Use into.database) in
  Base.List.fold_result (first :: rest) ~init:() ~f:(fun () statement ->
      retried link statement)

let give_back = function Any_time -> () | Taken turn -> Await.Await.Semaphore.release turn

let on_idle_loader t work =
  let loader = Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await t.idle in
  let result = Quiver_core.Guarded.hold (Iarray.get t.loaders loader) work in
  Await.Await.Mpmc_queue.push t.idle loader;
  result

let declare t { table = { name; _ } as table; ddl; parts; declared } =
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
    let { Deferred_keys.create; later; foreign; _ } = Deferred_keys.split deferral name.table ddl in
    let increments = t.increments in
    let%map sharing =
      on_idle_loader t (fun link ->
          let%bind () = execute link (Use into.database) in
          let%bind () = retried link create in
          match (later, parts) with
          | first :: rest, 0 ->
              let%map () = run_in link into (first, rest) in
              Load.Together
          | _, 0 -> Ok Load.Together
          | _, _ ->
              rejected
                (Quiver_mysql.Query.sql (Auto_increment into))
                (Load.sharing link increments into))
    in
    let () =
      match sharing with
      | One_at_a_time ->
          ignore
            (Portable_lockfree_htbl.set t.turns ~key:name ~data:(Await.Await.Semaphore.create 1))
      | Together -> ()
    in
    remember foreign;
    match later with
    | first :: rest when parts > 0 ->
        ignore
          (Portable_lockfree_htbl.set t.deferred ~key:name
             ~data:
               {
                 into;
                 later = (first, rest);
                 remaining = Portable.Atomic.make parts;
                 bytes = Portable.Atomic.make 0;
               })
    | [] | _ :: _ -> ()
  in
  let physical datadir collations builder sorting =
    let definition = Deferred_keys.split All_keys name.table ddl in
    let started = Quiver_core.Stopwatch.start () in
    let%bind prepared =
      on_idle_loader t (fun link ->
          Result.map_error
            (fun (#Physical.cause as cause) -> Built { table = name; cause })
            (Physical.prepare link ~datadir ~collations ~into table ~definition ~builder ~sorting))
    in
    match prepared with
    | Unsuitable reason ->
        Portable.Atomic.update t.fallbacks ~pure_f:(fun fallbacks -> (name, reason) :: fallbacks);
        plain ()
    | Prepared physical ->
        let state =
          {
            physical;
            source = name;
            into;
            later = Deferred_keys.adding name.table (Physical.left physical) @ definition.special;
            remaining = Portable.Atomic.make parts;
            finished = Portable.Atomic.make [];
            sent = Portable.Atomic.make 0;
            bytes = Portable.Atomic.make 0;
            progress = Portable.Atomic.make Pending;
            preparing = Quiver_core.Stopwatch.elapsed started;
          }
        in
        ignore (Portable_lockfree_htbl.set t.built ~key:name ~data:state);
        Portable.Atomic.update t.builds ~pure_f:(fun builds -> state :: builds);
        remember definition.foreign;
        Ok ()
  in
  let created =
    match t.mode with
    | Physical_at { datadir; collations; builder; sorting } when parts > 0 ->
        physical datadir collations builder sorting
    | Physical_at _ | Plain -> plain ()
  in
  Result.iter_error
    (fun error ->
      ignore (Portable.Atomic.compare_and_set t.broken ~if_phys_equal_to:None ~replace_with:(Some error)))
    created;
  Await.Await.Ivar.fill_if_empty declared created

let ended t = function
  | Keys_of { into; later; added } ->
      Await.Await.Ivar.fill_if_empty added (on_idle_loader t (fun link -> run_in link into later))
  | Import_of { state; table; closed; imported } ->
      Await.Await.Ivar.fill_if_empty imported
        (on_idle_loader t (fun link ->
             let queued = Quiver_core.Stopwatch.elapsed closed and started = Quiver_core.Stopwatch.start () in
             let%bind checked =
               let wanted = Int.max 1 (Portable.Atomic.get state.bytes / (4 * 1024 * 1024)) in
               let lent = Quiver_core.Lease.take t.spare ~wanted in
               let checked =
                 Physical.check link state.physical (Portable.Atomic.get state.finished) ~threads:(Int.max 1 lent)
               in
               Quiver_core.Lease.give t.spare lent;
               Result.map_error (fun (#Physical.cause as cause) -> Built { table; cause }) checked
             in
             let checking = Quiver_core.Stopwatch.elapsed started and imports = Quiver_core.Stopwatch.start () in
             let%bind built =
               Result.map_error (fun (#Physical.cause as cause) -> Built { table; cause }) (Physical.import link checked)
             in
             let sent = Portable.Atomic.get state.sent in
             let%bind () = if built = sent then Ok () else Error (Lost_rows { table; sent; built }) in
             let importing = Quiver_core.Stopwatch.elapsed imports in
             let keying = Quiver_core.Stopwatch.start () in
             let%map () = match state.later with [] -> Ok () | first :: rest -> run_in link state.into (first, rest) in
             Portable.Atomic.set state.progress
               (Imported
                  { preparing = state.preparing; queued; checking; importing; keys = Quiver_core.Stopwatch.elapsed keying })))

let create config target ~keys ~after ~binlog ~ingest ~sessions =
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
     let settings =
       let%bind increments =
         Quiver_core.Guarded.hold control (fun link ->
             rejected (Quiver_mysql.Query.sql Autoinc_lock_mode) (Load.increments link))
       in
       let%map mode =
         match ingest with
         | Load_data -> Ok Plain
         | Tablespaces { datadir; builder; sorting; threads = _ } ->
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
                 let%bind () =
                   match Option.map Quiver_innodb.File.size sorting with
                   | None | Some (Ok _) -> Ok ()
                   | Some (Error (`File _)) -> Error (Tablespaces_need (Sort_directory (Option.get sorting)))
                 in
                 let%map collations =
                   rejected "SELECT ID, CHARACTER_SET_NAME FROM information_schema.COLLATIONS"
                     (Quiver_mysql.Link.run_query link Text_collations)
                 in
                 Physical_at
                   {
                     datadir;
                     builder;
                     sorting;
                     collations =
                       List.filter_map
                         (function
                           | [| Some id; Some "utf8mb4" |] -> Option.map (fun id -> (id, Quiver_innodb.Layout.Utf8mb4)) (int_of_string_opt id)
                           | [| Some id; Some "utf8mb3" |] -> Option.map (fun id -> (id, Quiver_innodb.Layout.Utf8mb3)) (int_of_string_opt id)
                           | [| Some id; Some "latin1" |] -> Option.map (fun id -> (id, Quiver_innodb.Layout.Latin1)) (int_of_string_opt id)
                           | _ -> None)
                         collations;
                   })
       in
       (mode, increments)
     in
     match settings with
     | Error error ->
         List.iter
           (fun loader -> Quiver_core.Guarded.hold loader (fun link -> Quiver_mysql.Link.close link))
           loaders;
         Quiver_core.Guarded.hold control (fun link -> Quiver_mysql.Link.close link);
         Error error
     | Ok (mode, increments) ->
         let idle = Await.Await.Mpmc_queue.create () in
         List.iteri (fun index _ -> Await.Await.Mpmc_queue.push idle index) loaders;
         let copy = Portable.Atomic.make None in
         let t =
           {
             target;
             keys;
             after;
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
             increments;
             turns = Portable_lockfree_htbl.create (module Quiver_core.Schema.Name);
             finishing = Portable.Atomic.make Unfinished;
             declared = Portable_lockfree_htbl.create (module Quiver_core.Schema.Name);
             declaring = Portable.Atomic.make [];
             broken = Portable.Atomic.make None;
             preparing =
               Quiver_core.Pool.start ~workers:(Int.min 4 (List.length loaders)) (fun declaration ->
                   Option.iter (fun t -> declare t declaration) (Portable.Atomic.get copy));
             spare =
               Quiver_core.Lease.create
                 (match ingest with
                 | Load_data -> 0
                 | Tablespaces { threads; _ } -> Option.value threads ~default:(Quiver_core.Cores.budget Same_machine));
             ending =
               Quiver_core.Pool.start ~workers:(List.length loaders) (fun job ->
                   Option.iter (fun t -> ended t job) (Portable.Atomic.get copy));
           }
         in
         Portable.Atomic.set copy (Some t);
         Ok t)

let shutdown t =
  List.iter
    (fun declared ->
      match Await.Await.Ivar.read Quiver_core.Blocking.await declared with Ok () | Error _ -> ())
    (Portable.Atomic.get t.declaring);
  Quiver_core.Pool.stop t.preparing;
  Quiver_core.Pool.stop t.ending;
  List.iter
    (fun built ->
      match Portable.Atomic.get built.progress with
      | Imported _ -> ()
      | Pending ->
          Quiver_core.Guarded.hold t.control (fun link -> Physical.abandon link built.physical))
    (Portable.Atomic.get t.builds);
  Iarray.iter
    (fun loader -> Quiver_core.Guarded.hold loader (fun link -> Quiver_mysql.Link.close link))
    t.loaders;
  Quiver_core.Guarded.hold t.control (fun link -> Quiver_mysql.Link.close link)

let ways t =
  List.filter_map
    (fun built ->
      match Portable.Atomic.get built.progress with
      | Imported imported -> Some (built.source, Imported imported)
      | Pending -> None)
    (Portable.Atomic.get t.builds)
  @ List.rev_map (fun (name, reason) -> (name, Loaded reason)) (Portable.Atomic.get t.fallbacks)

let advice t = t.advice
let finishing t = Portable.Atomic.get t.finishing
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
  let declared = Await.Await.Ivar.create () in
  ignore (Portable_lockfree_htbl.set t.declared ~key:name ~data:declared);
  Portable.Atomic.update t.declaring ~pure_f:(fun declaring -> declared :: declaring);
  let declaration = { table; ddl; parts; declared } in
  match Quiver_core.Pool.submit t.preparing ~priority:table.estimate.bytes declaration with
  | Queued -> Ok ()
  | Unqueued ->
      declare t declaration;
      Result.map_error (fun error -> `Sink error) (Await.Await.Ivar.read Quiver_core.Blocking.await declared)

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
  let turn =
    match Portable_lockfree_htbl.find t.turns name with
    | Null -> Any_time
    | This turn ->
        Await.Await.Semaphore.acquire Quiver_core.Blocking.await turn;
        Taken turn
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
      give_back turn;
      Error (`Sink error)
  | Ok () ->
      Ok
        (Loading
           {
             owner = t;
             loader;
             turn;
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
  let declared =
    Option.fold
      ~none:
        (match Portable_lockfree_htbl.find t.declared table.name with
        | Null -> Ok ()
        | This declared -> Await.Await.Ivar.read Quiver_core.Blocking.await declared)
      ~some:Result.error (Portable.Atomic.get t.broken)
  in
  let%bind () = Result.map_error (fun error -> `Sink error) declared in
  match Portable_lockfree_htbl.find t.built table.name with
  | Null -> loading t table ~index
  | This state ->
      let space = Physical.space state.physical in
      let%map placing =
        match Quiver_innodb.Tablespace.order space with
        | Key_order -> Ok (In_order (Quiver_innodb.Tablespace.run space ~rank:index))
        | Any_order ->
            let slots = 16 in
            let%map slots =
              Result.map_error
                (fun error -> `Sink error)
                (Base.Result.all
                   (List.init slots (fun _ ->
                        Quiver_core.Guarded.create (fun () -> (Ok { run = None; bytes = 0 } : (slot, error) result)))))
            in
            Spread
              {
                space;
                rank = index;
                slots = Iarray.of_list slots;
                held = Portable.Atomic.make 0;
                closed = Portable.Atomic.make [];
              }
      in
      Building
        {
          owner = t;
          table = table.name;
          state;
          placing;
          file = file_of table.name index;
          hash = Quiver_sink.Xxh3.create ();
          rows = 0;
          bytes = 0;
        }

let renderer = function
  | Loading loading -> Csv loading.renderer
  | Building { placing = In_order _; state; table; _ } -> Images { space = Physical.space state.physical; table }
  | Building { placing = Spread spread; table; _ } -> Spread_images { spread; table }

let render (renderer : renderer) (batch : Quiver_core.Batch.sealed @ local) =
  match renderer with
  | Images { space; table } -> (
      let rows = Quiver_core.Batch.sealed_rows batch in
      match Quiver_innodb.Tablespace.encode space (Quiver_core.Batch.sealed_columns batch) ~rows with
      | Ok records -> Ok (Records { records; rows })
      | Error (`Tablespace error) -> Error (`Sink (Built { table; cause = `Tablespace error })))
  | Spread_images { spread; table } -> (
      let rows = Quiver_core.Batch.sealed_rows batch in
      let add slot records =
        Quiver_core.Guarded.hold slot (fun slot ->
            let run =
              match slot.run with
              | Some run -> run
              | None ->
                  let run = Quiver_innodb.Tablespace.run spread.space ~rank:spread.rank in
                  slot.run <- Some run;
                  run
            in
            let full = 32 lsl 20 in
            (let%bind () = Quiver_innodb.Tablespace.add run records in
             slot.bytes <- slot.bytes + String.length (Quiver_innodb.Tablespace.image records);
             if slot.bytes < full then Ok ()
             else
               let%map finished = Quiver_innodb.Tablespace.close run in
               slot.run <- None;
               slot.bytes <- 0;
               Portable.Atomic.update spread.closed ~pure_f:(fun closed -> finished :: closed)
              : (unit, Physical.cause) result))
      in
      let rec claim () =
        let held = Portable.Atomic.get spread.held in
        match lnot held land ((1 lsl Iarray.length spread.slots) - 1) with
        | 0 -> None
        | free -> (
            let bit = free land -free in
            match Portable.Atomic.compare_and_set spread.held ~if_phys_equal_to:held ~replace_with:(held lor bit) with
            | Set_here -> Some bit
            | Compare_failed -> claim ())
      in
      let failed (#Physical.cause as cause) = Error (`Sink (Built { table; cause })) in
      match Quiver_innodb.Tablespace.encode spread.space (Quiver_core.Batch.sealed_columns batch) ~rows with
      | Error error -> failed error
      | Ok records -> (
          let added =
            match claim () with
            | None -> add (Iarray.get spread.slots 0) records
            | Some bit ->
                let added = add (Iarray.get spread.slots (Base.Int.ctz bit)) records in
                Portable.Atomic.update spread.held ~pure_f:(fun held -> held land lnot bit);
                added
          in
          match added with Error error -> failed error | Ok () -> Ok (Records { records; rows })))
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
      let added =
        match building.placing with
        | In_order run -> Quiver_innodb.Tablespace.add run records
        | Spread _ -> Ok ()
      in
      match added with
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
  give_back part.turn;
  match outcome with
  | Error error -> answer (Error error)
  | Ok () ->
      let built =
        match Portable_lockfree_htbl.find t.deferred part.job.table with
        | Null -> Ok ()
        | This { into; later; remaining; bytes } -> (
            let (_ : int) = Portable.Atomic.fetch_and_add bytes part.bytes in
            match Portable.Atomic.fetch_and_add remaining (-1) with
            | 1 ->
                let added = Await.Await.Ivar.create () in
                Portable.Atomic.update t.building ~pure_f:(fun building ->
                    added :: building);
                let job = Keys_of { into; later; added } in
                let () =
                  match Quiver_core.Pool.submit t.ending ~priority:(Portable.Atomic.get bytes) job with
                  | Queued -> ()
                  | Unqueued -> ended t job
                in
                Ok ()
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
  let closed =
    match part.placing with
    | In_order run -> Result.map (fun finished -> [ finished ]) (Quiver_innodb.Tablespace.close run)
    | Spread spread -> (
        let closed =
          Iarray.to_list spread.slots
          |> List.filter_map (fun slot ->
                 Quiver_core.Guarded.hold slot (fun slot ->
                     (Option.map Quiver_innodb.Tablespace.close slot.run
                       : (Quiver_innodb.Tablespace.finished, Physical.cause) result option)))
        in
        match (closed, Portable.Atomic.get spread.closed) with
        | [], [] ->
            Result.map
              (fun finished -> [ finished ])
              (Quiver_innodb.Tablespace.close (Quiver_innodb.Tablespace.run spread.space ~rank:spread.rank))
        | closed, earlier -> Result.map (fun closed -> closed @ earlier) (Base.Result.all closed))
  in
  match closed with
  | Error (#Physical.cause as cause) -> answer (Error (Built { table = part.table; cause }))
  | Ok finished -> (
      Portable.Atomic.update state.finished ~pure_f:(fun all -> finished @ all);
      let (_ : int) = Portable.Atomic.fetch_and_add state.sent part.rows in
      let (_ : int) = Portable.Atomic.fetch_and_add state.bytes part.bytes in
      match Portable.Atomic.fetch_and_add state.remaining (-1) with
      | 1 ->
          let imported = Await.Await.Ivar.create () and table = part.table
          and closed = Quiver_core.Stopwatch.start () in
          Portable.Atomic.update t.building ~pure_f:(fun building -> imported :: building);
          let job = Import_of { state; table; closed; imported } in
          let () =
            match Quiver_core.Pool.submit t.ending ~priority:(Portable.Atomic.get state.bytes) job with
            | Queued -> ()
            | Unqueued -> ended t job
          in
          answer (Ok (manifest ()))
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
  Await.Await.Mpmc_queue.push part.owner.idle part.loader;
  give_back part.turn

let discard = function Loading loading -> discard_loading loading | Building _ -> ()

let finish t ~source:_ ~tables ~manifest:_ =
  Result.map_error
    (fun error -> `Sink error)
    (let began = Quiver_core.Stopwatch.start () in
     let%bind () =
       Base.List.fold_result
         (List.rev (Portable.Atomic.get t.declaring))
         ~init:()
         ~f:(fun () declared -> Await.Await.Ivar.read Quiver_core.Blocking.await declared)
     in
     let%bind () =
       Base.List.fold_result
         (List.rev (Portable.Atomic.get t.building))
         ~init:()
         ~f:(fun () added ->
           Await.Await.Ivar.read Quiver_core.Blocking.await added)
     in
     let built = Quiver_core.Stopwatch.elapsed began and updating = Quiver_core.Stopwatch.start () in
     let target = t.target in
     let%bind after =
       Result.map_error
         (fun (`After_load error) -> After_load error)
         (After_load.statements t.after (List.map fst tables) ~into:(fun name ->
              { name with database = Load.renamed target name.database }))
     in
     let%bind () =
       Base.Result.all_unit
         (Quiver_core.Concurrent.spread ~workers:(Iarray.length t.loaders) (Iarray.of_list after)
            (fun (into, statements) -> on_idle_loader t (fun link -> run_in link into statements)))
     in
     let after_load = Quiver_core.Stopwatch.elapsed updating and adding = Quiver_core.Stopwatch.start () in
     let%bind () =
       Base.Result.all_unit
         (Quiver_core.Concurrent.spread ~workers:(Iarray.length t.loaders)
            (Iarray.of_list (List.rev (Portable.Atomic.get t.foreign)))
            (fun (into, keys) ->
              on_idle_loader t (fun link -> run_in link into keys)))
     in
     let foreign_keys = Quiver_core.Stopwatch.elapsed adding and creating = Quiver_core.Stopwatch.start () in
     let objects = Portable.Atomic.get t.objects in
     let%map () =
       Base.Result.all_unit
         (Quiver_core.Concurrent.spread ~workers:(Iarray.length t.loaders)
            (Iarray.of_list objects)
            (fun (database, scripts) ->
              on_idle_loader t (fun link ->
                  let%bind () = execute link (Use (Load.renamed target database)) in
                  Base.List.fold_result scripts ~init:()
                    ~f:(fun () ({ statements; _ } : Quiver_sink.Layout.script) ->
                      Base.List.fold_result statements ~init:()
                        ~f:(fun () statement -> run link statement)))))
     in
     Portable.Atomic.set t.finishing
       (Finished { built; after_load; foreign_keys; objects = Quiver_core.Stopwatch.elapsed creating }))
