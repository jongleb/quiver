open Base.Result.Let_syntax

type cause : value mod contended portable =
  [ `Link of Quiver_mysql.Link.error
  | `Wire of Quiver_mysql.Wire.error
  | `Transport of Quiver_mysql.Transport.error ]
[@@deriving sexp_of ~portable]

type error =
  | Unreadable of { file : File_path.Part.t; message : string }
  | Corrupt of File_path.Part.t
  | Missing of File_path.Part.t
  | Rejected of { file : File_path.Part.t; statement : string; cause : cause }
  | Several_databases of Quiver_core.Ident.t list
  | Non_positive_threads of int
  | Unstarted of Quiver_core.Concurrent.error
  | Unreadable_columns of Quiver_core.Schema.Name.t
[@@deriving sexp_of ~portable]

type target = Same_names | Into of Quiver_core.Ident.t
type keys = After_data | With_table
[@@deriving to_string ~capitalize:"kebab-case" ~portable, enumerate]
type binlog = Write_binlog [@rename "write"] | Skip_binlog [@rename "skip"]
[@@deriving to_string ~capitalize:"kebab-case" ~portable, enumerate]
type summary = {
  files : int;
  rows : int;
  bytes : int;
  skipped : int;
  advice : Quiver_core.Advice.t list;
}

(** Whether an earlier load of the same dump into the same target left committed
    files behind. *)
type resume = Fresh | Resuming

(** How a data file reaches the server. *)
type delivery =
  | As_statements
  | As_local_data of { statement : string; enums : Quiver_core.Ident.t list }
      (** With this [LOAD DATA LOCAL] statement into a table with these
          [ENUM] columns. *)

(** Everything a loading thread can fail with. *)
type failure =
  [ `Load of error
  | `Statements of Statements.error
  | `Ident of Quiver_core.Ident.error
  | cause ]

type table_keys = {
  later : string list;  (** Key statements to run once the data is in. *)
  mark : string;  (** Their progress mark. *)
  remaining : int Portable.Atomic.t;  (** Data files of the table to commit. *)
}

type data_file = {
  database : Quiver_core.Ident.t;
  part : Quiver_core.Manifest.part;
  delivery : delivery;
  keys : table_keys;
}

(** Work for a loading thread, which owns one session. *)
type task =
  | Create of (Quiver_core.Schema.Name.t * Quiver_core.Manifest.part list)
  | Fill of Schedule.slot * data_file
  | Run of { database : Quiver_core.Ident.t; files : File_path.Part.t list }
      (** Schema files of the database's objects, one after another. *)
  | Stop

(** What a loading thread made of a task. *)
type report =
  | Created of (Schedule.cap * data_file list)
  | Filled of Schedule.slot
  | Ran
  | Failed of failure

(** Whether [LOAD DATA] into a table with an [AUTO_INCREMENT] column holds a
    table lock to the statement's end, letting one load in at a time. *)
type increments = Interleaved | Table_locked

(** Whether the loading threads still work, or only answer after a failure. *)
type course = Going | Stopping

type uniqueness = Unchecked | Checked_on_mariadb

let session (config : Quiver_mysql.Provider.conn_config) binlog uniqueness =
  let%bind link =
    Quiver_mysql.Link.connect ~local_data:With_local_data config.route
      ~timeouts:config.timeouts ~user:config.user ~password:config.password
  in
  let%bind () = Quiver_mysql.Link.run_statement link Prepare_load in
  let%bind () =
    match (uniqueness, Quiver_mysql.Link.server link) with
    | Checked_on_mariadb, (Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _) ->
        Quiver_mysql.Link.run_statement link Check_unique
    | Unchecked, _ | Checked_on_mariadb, (Mysql_8_0 _ | Mysql_8_4 _) -> Ok ()
  in
  let%map () =
    match binlog with
    | Write_binlog -> Ok ()
    | Skip_binlog -> Quiver_mysql.Link.run_statement link Skip_binlog
  in
  link

let advice link binlog =
  match Quiver_mysql.Link.run_query link Durability with
  | Ok [ [| flush; sync; logged |] ] ->
      let on value = Option.equal String.equal value (Some "1") in
      let logged_load =
        match binlog with
        | Write_binlog when on logged -> Some Quiver_core.Advice.Logged_load
        | Write_binlog | Skip_binlog -> None
      in
      List.filter_map Fun.id
        [
          Base.Option.some_if (on flush) Quiver_core.Advice.Durable_commits;
          Base.Option.some_if (on sync && on logged) Quiver_core.Advice.Synced_binlog;
          logged_load;
        ]
  | Ok _ | Error _ -> []

let sessions config binlog uniqueness ~count ~control ~close =
  let loaders, control =
    Quiver_core.Concurrent.both
      (fun () ->
        Quiver_core.Concurrent.spread ~workers:count (Iarray.init count Fun.id) (fun (_ : int) ->
            Quiver_core.Guarded.create (fun () ->
                (session config binlog uniqueness : (Quiver_mysql.Link.t, cause) result))))
      control
  in
  match
    Base.Result.combine control (Base.Result.all loaders)
      ~ok:(fun control loaders -> (control, loaders))
      ~err:(fun error _ -> error)
  with
  | Ok opened -> Ok opened
  | Error (#cause as error) ->
      Result.iter close control;
      List.iter
        (Result.iter (fun link ->
             Quiver_core.Guarded.hold link (fun link -> Quiver_mysql.Link.close link)))
        loaders;
      Error error

let retried ~cause run =
  let rec attempt left =
    match run () with
    | Ok _ as done_ -> done_
    | Error error as failed -> (
        match (cause error, left) with
        | Some (`Link (Quiver_mysql.Link.Refused { code = 1213 | 1205; _ })), left when left > 1 ->
            attempt (left - 1)
        | _ -> failed)
  in
  attempt 5

let renamed target database =
  match target with Same_names -> database | Into renamed -> renamed

let create_database target database statement =
  Option.fold ~none:statement
    ~some:(fun rest ->
      "CREATE DATABASE "
      ^ Quiver_core.Ident.backquoted (renamed target database)
      ^ rest)
    (Base.String.chop_prefix statement
       ~prefix:("CREATE DATABASE " ^ Quiver_core.Ident.backquoted database))

let run config ~dir target ~keys ~binlog ~threads =
  let read file consume =
    match
      In_channel.open_bin
        (File_path.to_string File_path.Operators.(dir /?. file))
    with
    | exception Sys_error message ->
        Error (`Load (Unreadable { file; message }))
    | channel ->
        let hash = Quiver_sink.Xxh3.create () in
        let unreadable message = `Load (Unreadable { file; message }) in
        let stored bytes ~pos ~len =
          match In_channel.input channel bytes pos len with
          | exception Sys_error message -> Error (unreadable message)
          | length ->
              Quiver_sink.Xxh3.add hash bytes ~pos ~len:length;
              Ok length
        in
        let source =
          if Base.String.is_suffix (File_path.Part.to_string file) ~suffix:".zst" then
            Quiver_sink.Compression.inflate stored ~corrupt:unreadable
          else stored
        in
        let result = consume source in
        In_channel.close channel;
        let%map () = result in
        Quiver_core.Checksum.Xxh3_64 (Quiver_sink.Xxh3.value hash)
  in
  let rejected file statement result =
    Result.map_error
      (function
        | `Load _ as unreadable -> unreadable
        | #cause as cause ->
            `Load
              (Rejected
                 { file; statement = Base.String.prefix statement 200; cause }))
      result
  in
  let statements run link file rewrite =
    read file (fun text ->
        Statements.iter text (fun statement ->
            let sql = rewrite statement in
            rejected file sql (run link sql)))
  in
  let schema_file base =
    Option.to_result
      ~none:(`Load (Missing (File_path.Part.of_string (base ^ ".sql"))))
      (List.find_map
         (fun extension ->
           let file = File_path.Part.of_string (base ^ extension) in
           Base.Option.some_if
             (Sys.file_exists (File_path.to_string File_path.Operators.(dir /?. file)))
             file)
         [ ".sql"; ".sql.zst" ])
  in
  let table_base ({ database; table } : Quiver_core.Schema.Name.t) =
    Quiver_sink.Layout.file_name database
    ^ "."
    ^ Quiver_sink.Layout.file_name table
  in
  let%bind () =
    match threads with
    | n when n < 1 -> Error (`Load (Non_positive_threads n))
    | _ -> Ok ()
  in
  let%bind text, manifest =
    match
      In_channel.with_open_bin
        (File_path.to_string File_path.Operators.(dir /?. ~."quiver.json"))
        In_channel.input_all
    with
    | text -> (
        match Yojson.Safe.from_string text with
        | json -> Ok (text, json)
        | exception Yojson.Json_error message ->
            Error
              (`Load
                 (Unreadable
                    { file = File_path.Part.of_string "quiver.json"; message }))
        )
    | exception Sys_error message ->
        Error
          (`Load
             (Unreadable
                { file = File_path.Part.of_string "quiver.json"; message }))
  in
  let dump =
    Printf.sprintf "%016Lx"
      (Quiver_sink.Xxh3.digest (Bytes.unsafe_of_string text) ~pos:0
         ~len:(String.length text))
  and progress_target =
    match target with Same_names -> "" | Into renamed -> (renamed :> string)
  in
  let%bind { format; tables; parts } = Quiver_core.Manifest.read manifest in
  let tables =
    let grouped = Base.Hashtbl.create (module Quiver_core.Schema.Name) in
    List.iter
      (fun (table, part) -> Base.Hashtbl.add_multi grouped ~key:table ~data:part)
      (List.rev parts);
    List.map
      (fun table ->
        (table, Option.value (Base.Hashtbl.find grouped table) ~default:[]))
      tables
  in
  let databases =
    Base.List.stable_dedup ~compare:Quiver_core.Ident.compare
      (List.map
         (fun ((table : Quiver_core.Schema.Name.t), _) -> table.database)
         tables)
  in
  let%bind () =
    match (target, databases) with
    | Into _, ([] | [ _ ]) | Same_names, _ -> Ok ()
    | Into _, several -> Error (`Load (Several_databases several))
  in
  let%bind entries =
    match Sys.readdir (File_path.to_string dir) with
    | entries -> Ok (List.sort String.compare (Array.to_list entries))
    | exception Sys_error message ->
        Error (`Load (Unreadable { file = File_path.Part.of_string "."; message }))
  in
  let%bind objects =
    Base.Result.all
      (List.map
         (fun database ->
           let prefix = Quiver_sink.Layout.file_name database in
           let views =
             List.filter_map
               (fun entry ->
                 Option.bind
                   (Base.String.chop_prefix entry ~prefix:(prefix ^ "."))
                   (fun rest ->
                     Option.map
                       (fun view -> prefix ^ "." ^ view)
                       (List.find_map
                          (fun suffix -> Base.String.chop_suffix rest ~suffix)
                          [ "-schema-view.sql"; "-schema-view.sql.zst" ])))
               entries
           in
           let files suffix =
             Base.Result.all (List.map (fun view -> schema_file (view ^ suffix)) views)
           in
           let%bind stubs = files "-schema" in
           let%map views = files "-schema-view" in
           let optional base = Result.to_list (schema_file base) in
           ( List.map (fun stub -> Run { database; files = [ stub ] }) stubs,
             List.filter_map
               (function [] -> None | files -> Some (Run { database; files }))
               [ views; optional (prefix ^ "-schema-triggers"); optional (prefix ^ "-schema-post") ] ))
         databases)
  in
  let%bind (control, loaded, increments), links =
    sessions config binlog Unchecked ~count:threads
      ~control:(fun () ->
        let%bind control = session config binlog Unchecked in
        match
          let%bind () = Quiver_mysql.Link.run_statement control Create_progress_schema in
          let%bind () = Quiver_mysql.Link.run_statement control Create_progress_table in
          let%bind loaded =
            Quiver_mysql.Link.run_query control (Loaded_files { dump; target = progress_target })
          in
          let%map mode = Quiver_mysql.Link.run_query control Autoinc_lock_mode in
          let increments =
            match mode with
            | [ [| Some "2" |] ] -> Interleaved
            | _ -> Table_locked
          in
          (List.filter_map (function [| file |] -> file | _ -> None) loaded, increments)
        with
        | Ok (loaded, increments) -> Ok (control, loaded, increments)
        | Error error ->
            Quiver_mysql.Link.close control;
            Error error)
      ~close:(fun (control, _, _) -> Quiver_mysql.Link.close control)
  in
  let committed = Base.Set.of_list (module Base.String) loaded in
  let resume = match loaded with [] -> Fresh | _ -> Resuming in
  let ddl link sql =
    match (resume, Quiver_mysql.Link.execute link sql) with
    | ( Resuming,
        Error
          (`Link
             (Quiver_mysql.Link.Refused
                { code = 1007 | 1050 | 1061 | 1826 | 1359 | 1304 | 1537; _ })) )
      ->
        Ok ()
    | (Fresh | Resuming), result -> result
  in
  let record link file =
    Quiver_mysql.Link.run_statement link (Record_progress { dump; target = progress_target; file })
  in
  let close link = Quiver_core.Guarded.hold link (fun link -> Quiver_mysql.Link.close link) in
  let%bind () =
    match
      let%bind () =
        match resume with Fresh -> record control "(started)" | Resuming -> Ok ()
      in
      Base.List.fold_result databases ~init:() ~f:(fun () database ->
          let%bind file = schema_file (Quiver_sink.Layout.file_name database ^ "-schema-create") in
          let%map _ = statements ddl control file (create_database target database) in
          ())
    with
    | Ok () -> Ok ()
    | Error error ->
        Quiver_mysql.Link.close control;
        List.iter close links;
        Error error
  in
  let cause = function
    | `Load (Rejected { cause; _ }) -> Some cause
    | #cause as cause -> Some cause
    | `Load _ | `Statements _ | `Ident _ -> None
  in
  let size parts =
    Base.List.sum
      (module Base.Int)
      parts
      ~f:(fun (part : Quiver_core.Manifest.part) -> part.stored_bytes)
  in
  let add_keys link { later; mark; _ } =
    if List.is_empty later || Base.Set.mem committed mark then Ok ()
    else
      let%bind () =
        Base.List.fold_result later ~init:() ~f:(fun () statement ->
            retried ~cause (fun () ->
                rejected (File_path.Part.of_string mark) statement (ddl link statement)))
      in
      record link mark
  in
  let create link (({ Quiver_core.Schema.Name.database; table } as name), parts)
      =
    let%bind () = Quiver_mysql.Link.run_statement link (Use (renamed target database)) in
    let%bind file = schema_file (table_base name ^ "-schema") in
    let deferred = ref [] in
    let%bind _ =
      retried ~cause (fun () ->
          statements ddl link file (fun statement ->
              match keys with
              | With_table -> statement
              | After_data -> (
                  let { Deferred_keys.create; later; foreign } =
                    Deferred_keys.split All_keys table statement
                  in
                  match later @ foreign with
                  | [] -> create
                  | keys ->
                      deferred := keys;
                      create)))
    in
    let%bind delivery =
      match format with
      | Sql -> Ok As_statements
      | Csv dialect ->
          let query =
            Quiver_mysql.Query.Load_columns
              { database = renamed target database; table }
          in
          let%bind rows =
            Quiver_mysql.Link.run_query link (query)
          in
          let%map columns =
            Base.Result.all
              (List.map
                 (function
                   | [| Some column; Some data_type |] -> (
                       let%map column = Quiver_core.Ident.of_string column in
                       ( column,
                         match String.lowercase_ascii data_type with
                         | "binary" | "varbinary" | "tinyblob" | "blob"
                         | "mediumblob" | "longblob" | "geometry" | "point"
                         | "linestring" | "polygon" | "multipoint"
                         | "multilinestring" | "multipolygon"
                         | "geometrycollection" | "geomcollection" ->
                             Quiver_mysql.Statement.Hex_value
                         | "bit" -> Number_value
                         | "enum" -> Enum_value
                         | _ -> Text_value ))
                   | _ -> Error (`Load (Unreadable_columns name)))
                 rows)
          in
          As_local_data
            {
              statement = Quiver_mysql.Statement.sql (Load_local { table; dialect; columns });
              enums =
                List.filter_map
                  (fun (name, (conversion : Quiver_mysql.Statement.conversion)) ->
                    match conversion with
                    | Enum_value -> Some name
                    | Text_value | Hex_value | Number_value -> None)
                  columns;
            }
    in
    let pending =
      List.filter
        (fun ({ file; _ } : Quiver_core.Manifest.part) ->
          not (Base.Set.mem committed (File_path.Part.to_string file)))
        parts
    in
    let keys =
      {
        later = !deferred;
        mark = "keys " ^ table_base name;
        remaining = Portable.Atomic.make (List.length pending);
      }
    in
    let%bind serial =
      match increments with
      | Interleaved -> Ok []
      | Table_locked ->
          Quiver_mysql.Link.run_query link
            (Auto_increment { database = renamed target database; table })
    in
    let%map () = match pending with [] -> add_keys link keys | _ -> Ok () in
    let most =
      let rows = Base.List.sum (module Base.Int) parts ~f:(fun part -> part.rows) in
      let bytes = Base.List.sum (module Base.Int) parts ~f:(fun part -> part.bytes) in
      if rows > 0 && bytes / rows > 8192 then 1 else threads
    in
    let cap : Schedule.cap =
      match (serial, Quiver_mysql.Link.server link) with
      | _ :: _, _ -> Alone
      | [], (Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _) -> First_alone most
      | [], (Mysql_8_0 _ | Mysql_8_4 _) -> At_most most
    in
    (cap, List.map (fun part -> { database; part; delivery; keys }) pending)
  in
  let fill link { database; part = { file; checksum; rows; _ }; delivery; keys } =
    let%bind () = Quiver_mysql.Link.run_statement link (Use (renamed target database)) in
    let%bind () =
      retried ~cause (fun () ->
          let%bind () = Quiver_mysql.Link.run_statement link Begin in
          let loaded =
            match%bind
              match delivery with
              | As_statements ->
                  statements Quiver_mysql.Link.execute link file Fun.id
              | As_local_data { statement; enums } ->
                  read file (fun data ->
                      rejected file statement
                        (Quiver_mysql.Link.load_local link statement ~rows ~enums data))
            with
            | found when Quiver_core.Checksum.equal found checksum ->
                let%bind () = record link (File_path.Part.to_string file) in
                Quiver_mysql.Link.run_statement link Commit
            | _ -> Error (`Load (Corrupt file))
          in
          Result.iter_error (fun _ -> ignore (Quiver_mysql.Link.run_statement link Rollback)) loaded;
          loaded)
    in
    match Portable.Atomic.fetch_and_add keys.remaining (-1) with
    | 1 -> add_keys link keys
    | _ -> Ok ()
  in
  let tasks = Await.Await.Mpmc_queue.create ()
  and reports = Await.Await.Mpmc_queue.create ()
  and course = Portable.Atomic.make Going in
  let (work @ portable) link =
    Quiver_core.Guarded.hold link (fun link ->
        let report going stopped =
          let report =
            match Portable.Atomic.get course with
            | Stopping -> stopped
            | Going -> (
                match going () with Ok report -> report | Error failure -> Failed failure)
          in
          Await.Await.Mpmc_queue.push reports report
        in
        let rec serve () =
          match Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await tasks with
          | Stop -> Quiver_mysql.Link.close link
          | Create table ->
              report
                (fun () -> Result.map (fun files -> Created files) (create link table))
                (Created (Alone, []));
              serve ()
          | Fill (slot, file) ->
              report (fun () -> Result.map (fun () -> Filled slot) (fill link file)) (Filled slot);
              serve ()
          | Run { database; files } ->
              report
                (fun () ->
                  let%bind () = Quiver_mysql.Link.run_statement link (Use (renamed target database)) in
                  let%map () =
                    Base.List.fold_result files ~init:() ~f:(fun () file ->
                        let%map _ = statements ddl link file Fun.id in
                        ())
                  in
                  Ran)
                Ran;
              serve ()
        in
        serve ())
  in
  let workers =
    List.map
      (fun (link : Quiver_mysql.Link.t Await.Capsule.Await.With_mutex.t) ->
        match Quiver_core.Concurrent.start (fun () -> work link) with
        | Ok (_ : unit Quiver_core.Concurrent.task) -> Ok ()
        | Error (`Concurrent error) ->
            close link;
            Error error)
      links
  in
  let started = Base.List.count workers ~f:Result.is_ok in
  let phase items task =
    List.iter (fun item -> Await.Await.Mpmc_queue.push tasks (task item)) items;
    let rec collect count created first =
      match count with
      | 0 -> Option.fold ~none:(Ok created) ~some:Result.error first
      | _ -> (
          match Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await reports with
          | Created table -> collect (count - 1) (table :: created) first
          | Filled _ | Ran -> collect (count - 1) created first
          | Failed failure ->
              Portable.Atomic.set course Stopping;
              collect (count - 1) created (Base.Option.first_some first (Some failure)))
    in
    collect (List.length items) [] None
  in
  let fill_all tables =
    let schedule =
      Schedule.create
        (List.map
           (fun (cap, files) ->
             (cap, List.map (fun (file : data_file) -> (file, file.part.stored_bytes)) files))
           tables)
    in
    let rec give busy =
      if busy < started then
        Option.fold ~none:busy
          ~some:(fun (slot, file) ->
            Await.Await.Mpmc_queue.push tasks (Fill (slot, file));
            give (busy + 1))
          (Schedule.take schedule)
      else busy
    in
    let rec collect busy first =
      match busy with
      | 0 -> Option.fold ~none:(Ok ()) ~some:Result.error first
      | _ -> (
          match Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await reports with
          | Filled slot ->
              Schedule.release schedule slot;
              let busy = match first with None -> give (busy - 1) | Some _ -> busy - 1 in
              collect busy first
          | Created _ | Ran -> collect (busy - 1) first
          | Failed failure ->
              Portable.Atomic.set course Stopping;
              collect (busy - 1) (Base.Option.first_some first (Some failure)))
    in
    collect (give 0) None
  in
  let loaded =
    match (started, Base.Result.all_unit workers) with
    | 0, Error error -> Error (`Load (Unstarted error))
    | _, (Ok () | Error _) ->
        let%bind tables =
          phase
            (List.sort (fun (_, a) (_, b) -> Int.compare (size b) (size a)) tables)
            (fun table -> Create table)
        in
        let%bind () = fill_all tables in
        let%bind _ = phase (List.concat_map fst objects) Fun.id in
        let%map _ = phase (List.concat_map snd objects) Fun.id in
        ()
  in
  Base.Fn.apply_n_times ~n:started
    (fun () -> Await.Await.Mpmc_queue.push tasks Stop)
    ();
  let finished =
    let%bind () = Result.map_error (function #failure as failure -> failure) loaded in
    Quiver_mysql.Link.run_statement control (Forget_progress { dump; target = progress_target })
  in
  Quiver_mysql.Link.close control;
  let%map () = finished in
  List.fold_left
    (fun summary ((_, part) : _ * Quiver_core.Manifest.part) ->
      if Base.Set.mem committed (File_path.Part.to_string part.file) then
        { summary with skipped = summary.skipped + 1 }
      else
        {
          summary with
          files = summary.files + 1;
          rows = summary.rows + part.rows;
          bytes = summary.bytes + part.bytes;
        })
    { files = 0; rows = 0; bytes = 0; skipped = 0; advice = advice control binlog }
    parts
