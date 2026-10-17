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
  | Unreadable_columns of Quiver_core.Schema.Name.t
[@@deriving sexp_of ~portable]

type target = Same_names | Into of Quiver_core.Ident.t
type keys = After_data | With_table
type binlog = Write_binlog | Skip_binlog
type summary = { files : int; rows : int; bytes : int; skipped : int }

(** Whether an earlier load of the same dump into the same target left committed
    files behind. *)
type resume = Fresh | Resuming

(** How a data file reaches the server. *)
type delivery =
  | As_statements
  | As_local_data of string  (** With this [LOAD DATA LOCAL] statement. *)

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
  | Fill of data_file
  | Run of { database : Quiver_core.Ident.t; files : File_path.Part.t list }
      (** Schema files of the database's objects, one after another. *)
  | Stop

(** What a loading thread made of a task. *)
type report = Created of data_file list | Filled | Ran | Failed of failure

(** Whether the loading threads still work, or only answer after a failure. *)
type course = Going | Stopping

let session (config : Quiver_mysql.Provider.conn_config) binlog =
  let execute link statement =
    Quiver_mysql.Link.execute link (Quiver_mysql.Statement.sql statement)
  in
  let%bind link =
    Quiver_mysql.Link.connect ~local_data:With_local_data ~tls:config.tls
      ~timeout:config.timeout ~host:config.host ~port:config.port
      ~user:config.user ~password:config.password
  in
  let%bind () = execute link Prepare_load in
  let%map () =
    match binlog with
    | Write_binlog -> Ok ()
    | Skip_binlog -> execute link Skip_binlog
  in
  link

let renamed target database =
  match target with Same_names -> database | Into renamed -> renamed

let create_database target database statement =
  Option.fold ~none:statement
    ~some:(fun rest ->
      "CREATE DATABASE "
      ^ Quiver_sink.Layout.quote (renamed target database)
      ^ rest)
    (Base.String.chop_prefix statement
       ~prefix:("CREATE DATABASE " ^ Quiver_sink.Layout.quote database))

let run config ~dir target ~keys ~binlog ~threads =
  let execute link statement =
    Quiver_mysql.Link.execute link (Quiver_mysql.Statement.sql statement)
  in
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
        let result =
          consume
            (match
               Base.String.is_suffix (File_path.Part.to_string file)
                 ~suffix:".zst"
             with
            | true -> Quiver_sink.Compression.inflate stored ~corrupt:unreadable
            | false -> stored)
        in
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
           match
             Sys.file_exists
               (File_path.to_string File_path.Operators.(dir /?. file))
           with
           | true -> Some file
           | false -> None)
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
  let%bind { format; parts } = Quiver_core.Manifest.read manifest in
  let tables =
    Base.List.Assoc.sort_and_group parts
      ~compare:Quiver_core.Schema.Name.compare
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
  let links, prepared =
    Quiver_core.Concurrent.both
      (fun () ->
        Quiver_core.Concurrent.spread ~workers:threads
          (Iarray.init threads Fun.id)
          (fun (_ : int) ->
            Quiver_core.Guarded.create (fun () ->
                (session config binlog : (Quiver_mysql.Link.t, cause) result))))
      (fun () ->
        let%bind control = session config binlog in
        match
          let%bind () = execute control Create_progress_schema in
          let%bind () = execute control Create_progress_table in
          Quiver_mysql.Link.query control
            (Quiver_mysql.Query.sql
               (Loaded_files { dump; target = progress_target }))
        with
        | Ok loaded -> Ok (control, List.filter_map (fun row -> row.(0)) loaded)
        | Error error ->
            Quiver_mysql.Link.close control;
            Error error)
  in
  let%bind control, loaded, links =
    match
      Base.Result.combine prepared (Base.Result.all links)
        ~ok:(fun (control, loaded) links -> (control, loaded, links))
        ~err:(fun error _ -> error)
    with
    | Ok opened -> Ok opened
    | Error (#cause as error) ->
        Result.iter (fun (control, _) -> Quiver_mysql.Link.close control) prepared;
        List.iter
          (Result.iter (fun link ->
               Quiver_core.Guarded.hold link (fun link ->
                   Quiver_mysql.Link.close link)))
          links;
        Error error
  in
  let committed = Base.Set.of_list (module Base.String) loaded in
  let done_ file = Base.Set.mem committed file in
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
    execute link (Record_progress { dump; target = progress_target; file })
  in
  let started = "(started)" in
  let%bind () =
    match resume with Fresh -> record control started | Resuming -> Ok ()
  in
  let%bind () =
    Base.List.fold_result databases ~init:() ~f:(fun () database ->
        let%bind file =
          schema_file (Quiver_sink.Layout.file_name database ^ "-schema-create")
        in
        let%map _ =
          statements ddl control file (create_database target database)
        in
        ())
  in
  let rec retried attempts run =
    match run () with
    | Error
        ( `Load
            (Rejected { cause = `Link (Refused { code = 1213 | 1205; _ }); _ })
        | `Link (Quiver_mysql.Link.Refused { code = 1213 | 1205; _ }) )
      when attempts > 1 ->
        retried (attempts - 1) run
    | result -> result
  in
  let size parts =
    List.fold_left
      (fun total (part : Quiver_core.Manifest.part) ->
        total + part.stored_bytes)
      0 parts
  in
  let add_keys link { later; mark; _ } =
    match (later, done_ mark) with
    | [], _ | _, true -> Ok ()
    | statements, false ->
        let%bind () =
          Base.List.fold_result statements ~init:() ~f:(fun () statement ->
              retried 5 (fun () ->
                  rejected
                    (File_path.Part.of_string mark)
                    statement (ddl link statement)))
        in
        record link mark
  in
  let create link (({ Quiver_core.Schema.Name.database; table } as name), parts)
      =
    let%bind () = execute link (Use (renamed target database)) in
    let%bind file = schema_file (table_base name ^ "-schema") in
    let deferred = ref [] in
    let%bind _ =
      retried 5 (fun () ->
          statements ddl link file (fun statement ->
              match keys with
              | With_table -> statement
              | After_data -> (
                  let { Deferred_keys.create; later } =
                    Deferred_keys.split table statement
                  in
                  match later with
                  | [] -> create
                  | _ ->
                      deferred := later;
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
            Quiver_mysql.Link.query link (Quiver_mysql.Query.sql query)
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
                         | _ -> Text_value ))
                   | _ -> Error (`Load (Unreadable_columns name)))
                 rows)
          in
          As_local_data
            (Quiver_mysql.Statement.sql (Load_local { table; dialect; columns }))
    in
    let pending =
      List.filter
        (fun ({ file; _ } : Quiver_core.Manifest.part) ->
          not (done_ (File_path.Part.to_string file)))
        parts
    in
    let keys =
      {
        later = !deferred;
        mark = "keys " ^ table_base name;
        remaining = Portable.Atomic.make (List.length pending);
      }
    in
    let%map () = match pending with [] -> add_keys link keys | _ -> Ok () in
    List.map (fun part -> { database; part; delivery; keys }) pending
  in
  let fill link { database; part = { file; checksum; _ }; delivery; keys } =
    let%bind () = execute link (Use (renamed target database)) in
    let%bind () =
      retried 5 (fun () ->
          let%bind () = execute link Begin in
          match
            match
              match delivery with
              | As_statements ->
                  statements Quiver_mysql.Link.execute link file Fun.id
              | As_local_data statement ->
                  read file (fun data ->
                      rejected file statement
                        (Quiver_mysql.Link.load_local link statement data))
            with
            | Ok found when Quiver_core.Checksum.equal found checksum ->
                let%bind () = record link (File_path.Part.to_string file) in
                execute link Commit
            | Ok _ -> Error (`Load (Corrupt file))
            | Error error -> Error error
          with
          | Ok () -> Ok ()
          | Error error ->
              ignore (execute link Rollback);
              Error error)
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
    let rec serve () =
      match Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await tasks with
      | Stop -> Quiver_mysql.Link.close link
      | Create table ->
          Await.Await.Mpmc_queue.push reports
            (match Portable.Atomic.get course with
            | Stopping -> Created []
            | Going -> (
                match create link table with
                | Ok files -> Created files
                | Error failure -> Failed failure));
          serve ()
      | Fill file ->
          Await.Await.Mpmc_queue.push reports
            (match Portable.Atomic.get course with
            | Stopping -> Filled
            | Going -> (
                match fill link file with
                | Ok () -> Filled
                | Error failure -> Failed failure));
          serve ()
      | Run { database; files } ->
          Await.Await.Mpmc_queue.push reports
            (match Portable.Atomic.get course with
            | Stopping -> Ran
            | Going -> (
                match
                  let%bind () = execute link (Use (renamed target database)) in
                  Base.List.fold_result files ~init:() ~f:(fun () file ->
                      let%map _ = statements ddl link file Fun.id in
                      ())
                with
                | Ok () -> Ran
                | Error failure -> Failed failure));
          serve ()
    in
    serve ())
  in
  let started =
    List.length
      (List.filter
         (fun (link : Quiver_mysql.Link.t Await.Capsule.Await.With_mutex.t) ->
           match Quiver_core.Concurrent.start (fun () -> work link) with
           | Ok _ -> true
           | Error (`Concurrent _) ->
               Quiver_core.Guarded.hold link (fun link ->
                   Quiver_mysql.Link.close link);
               false)
         links)
  in
  let phase items task =
    List.iter (fun item -> Await.Await.Mpmc_queue.push tasks (task item)) items;
    let rec collect count created first =
      match count with
      | 0 -> Option.fold ~none:(Ok created) ~some:Result.error first
      | _ -> (
          match Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await reports with
          | Created files -> collect (count - 1) (files @ created) first
          | Filled | Ran -> collect (count - 1) created first
          | Failed failure ->
              Portable.Atomic.set course Stopping;
              collect (count - 1) created
                (Option.fold ~none:(Some failure) ~some:Option.some first))
    in
    collect (List.length items) [] None
  in
  let objects =
    let entries =
      List.sort String.compare
        (Array.to_list (Sys.readdir (File_path.to_string dir)))
    in
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
             Base.Result.all
               (List.map (fun view -> schema_file (view ^ suffix)) views)
           in
           let%bind stubs = files "-schema" in
           let%map views = files "-schema-view" in
           let optional base = Result.to_list (schema_file base) in
           ( List.map (fun stub -> Run { database; files = [ stub ] }) stubs,
             List.filter_map
               (function
                 | [] -> None | files -> Some (Run { database; files }))
               [
                 views;
                 optional (prefix ^ "-schema-triggers");
                 optional (prefix ^ "-schema-post");
               ] ))
         databases)
  in
  let loaded =
    match started with
    | 0 -> Error (`Load (Non_positive_threads 0))
    | _ ->
        let%bind objects = objects in
        let%bind files =
          phase
            (List.sort (fun (_, a) (_, b) -> Int.compare (size b) (size a)) tables)
            (fun table -> Create table)
        in
        let%bind _ =
          phase
            (List.sort
               (fun (a : data_file) b ->
                 Int.compare b.part.stored_bytes a.part.stored_bytes)
               files)
            (fun file -> Fill file)
        in
        let%bind _ = phase (List.concat_map fst objects) Fun.id in
        let%map _ = phase (List.concat_map snd objects) Fun.id in
        ()
  in
  for _ = 1 to started do
    Await.Await.Mpmc_queue.push tasks Stop
  done;
  let%bind () =
    Result.map_error (function #failure as failure -> failure) loaded
  in
  let%bind () =
    execute control (Forget_progress { dump; target = progress_target })
  in
  Quiver_mysql.Link.close control;
  Ok
    (List.fold_left
       (fun summary ((_, part) : _ * Quiver_core.Manifest.part) ->
         match done_ (File_path.Part.to_string part.file) with
         | true -> { summary with skipped = summary.skipped + 1 }
         | false ->
             {
               summary with
               files = summary.files + 1;
               rows = summary.rows + part.rows;
               bytes = summary.bytes + part.bytes;
             })
       { files = 0; rows = 0; bytes = 0; skipped = 0 }
       parts)
