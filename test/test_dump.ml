open Quiver_core
open Sexplib0.Sexp_conv

(** Where a scenario makes the dump fail, if anywhere. *)
type fault =
  | Clean
  | Read of { chunk : int; row : int }  (** The provider fails at this row. *)
  | Render of int  (** The sink fails to render a batch of this chunk. *)
  | Append of int
  | Close of int
  | Table_ddl of int  (** The provider fails to show this table's DDL. *)
[@@deriving sexp_of]

(** Which tables give their rows. *)
type rows =
  | Every
  | No_data
  | Tables of int list  (** These, by number. *)
  | Database of int  (** The tables of this database. *)
  | Missing  (** A table the dump does not have. *)
[@@deriving sexp_of]

(** Whether the dump takes the databases' objects. *)
type objects = With_objects | Without_objects [@@deriving sexp_of]

(** How many sessions the dump reads on. *)
type reading =
  | Readers of int  (** Exactly this many. *)
  | Fitting of { cores : int; suggests : int option }
      (** By the cores, and what the server suggests. *)
[@@deriving sexp_of]

(** How many threads render. *)
type rendering = Renderers of int | Spare [@@deriving sexp_of]

(** How long the provider takes to read a chunk. *)
type pace =
  | Unpaced
  | Each of float  (** Seconds, every read on its own. *)
  | Taking_turns of float  (** Seconds, one read at a time. *)
  | Per_row of float  (** Seconds for each row read. *)
[@@deriving sexp_of]

type scenario = {
  tables : (int list * int list) list;
      (** Per table: its ids, ascending, and the bounds its chunks are cut at.
      *)
  databases : int;  (** Table [n] is in database [n mod databases]. *)
  objects : int list;  (** Per database, how many objects it has. *)
  rows : rows;
  taking : objects;
  reading : reading;
  rendering : rendering;
  batch_bytes : int;
  order : Dump_opts.order;
  declaring : Dump_opts.declaring;
  fault : fault;
  pace : pace;
}
[@@deriving sexp_of]

let ident = Support.ident

let database n = ident ("db" ^ string_of_int n)

let table ~databases n =
  Support.get_ok
    (Schema.Table.make ~generated:[]
       ~name:
         { database = database (n mod databases); table = ident ("t" ^ string_of_int n) }
       ~fields:[ { name = ident "id"; ltype = Int64; nullability = Not_null } ]
       ~key:(Primary (ident "id", []))
       ~estimate:{ rows = 0; bytes = 0 })

(** What a test does with the answer to a close. *)
type commit = Answer of (unit -> unit) @@ portable | Stop

(** A sink that remembers each part's rows, failing where told. *)
module Recorder = struct
  type error = Injected

  let sexp_of_error Injected = Sexplib0.Sexp.Atom "injected"

  type log = {
    mutable opened : (Schema.Name.t * int) list;
    mutable ended : (Schema.Name.t * int) list;  (** Closed or discarded. *)
    mutable closed : ((Schema.Name.t * int) * int64 list) list;
    mutable appended : ((Schema.Name.t * int) * int) list;
        (** Batches of each closed part. *)
    mutable finished : int;
    mutable databases : Ident.t list;
    mutable tables : (Schema.Name.t * int) list;  (** With their parts. *)
    mutable grown : Schema.Name.t list;  (** A part more, once each. *)
    mutable objects : (Ident.t * Ident.t list) list;
    mutable unannounced : int;
        (** Tables announced before their database, parts before their table
            or beyond its parts, parts added with none of the table's open. *)
  }

  type t = {
    fault : fault;
    chunk_of : Schema.Name.t -> int -> int @@ portable;
        (** A part's chunk in the plan, [-1] for a part added while dumping. *)
    doomed : (Schema.Name.t * int64) option;
        (** The row whose batch fails to render or append, as [fault] says. *)
    log : log Await.Capsule.Await.With_mutex.t;
    commits : commit Await.Await.Mpmc_queue.t;
        (** Answers to closes, given later by a thread of the test. *)
  }

  type part = {
    owner : t;
    name : Schema.Name.t;
    index : int;
    chunk : int;
    mutable rows : int64 list;
    mutable batches : int;
  }

  type renderer = { doomed : int64 option }
  type rendered = { values : int64 list @@ aliased many } [@@unboxed]

  let logged t f =
    Guarded.hold t.log f

  let format _ = Output_format.Sql
  let database t database ~ddl:_ =
    logged t (fun log -> log.databases <- database :: log.databases);
    Ok ()

  let table t ({ name; _ } : Schema.Table.t) ~ddl:_ ~parts =
    logged t (fun log ->
        (match List.mem name.database log.databases with
        | true -> ()
        | false -> log.unannounced <- log.unannounced + 1);
        log.tables <- (name, parts) :: log.tables);
    Ok ()

  let objects t database (definitions : Definition.t list) =
    let names = List.map (fun ({ name; _ } : Definition.t) -> name) definitions in
    logged t (fun log -> log.objects <- (database, names) :: log.objects);
    Ok ()

  let add_part t ({ name; _ } : Schema.Table.t) =
    logged t (fun log ->
        let open_parts =
          List.length (List.filter (fun (n, _) -> Schema.Name.equal n name) log.opened)
          - List.length (List.filter (fun (n, _) -> Schema.Name.equal n name) log.ended)
        in
        if open_parts < 1 then log.unannounced <- log.unannounced + 1;
        log.grown <- name :: log.grown);
    Ok ()

  let part t ({ name; _ } : Schema.Table.t) ~(index : int) =
    logged t (fun log ->
        let parts =
          Option.fold ~none:(-1)
            ~some:(fun parts ->
              parts
              + List.length (List.filter (Schema.Name.equal name) log.grown))
            (List.assoc_opt name log.tables)
        in
        if index >= parts then log.unannounced <- log.unannounced + 1;
        log.opened <- (name, index) :: log.opened);
    Ok
      {
        owner = t;
        name;
        index;
        chunk = t.chunk_of name index;
        rows = [];
        batches = 0;
      }

  let ended part =
    let name = part.name and index = part.index in
    logged part.owner (fun log -> log.ended <- (name, index) :: log.ended)

  let doomed part ~(on : fault -> bool) =
    match part.owner.doomed with
    | Some (name, row) when on part.owner.fault && Schema.Name.equal name part.name ->
        Some row
    | Some _ | None -> None

  let renderer part =
    { doomed = doomed part ~on:(function Render _ -> true | _ -> false) }

  let render { doomed } (batch : Batch.sealed @ local) =
    match Support.int64s batch with
    | Some values
      when not (Option.fold ~none:false ~some:(fun row -> List.mem row values) doomed)
      ->
        Ok { values }
    | Some _ | None -> Error (`Sink Injected)

  let append part { values = rows } =
    match doomed part ~on:(function Append _ -> true | _ -> false) with
    | Some row when List.mem row rows -> Error (`Sink Injected)
    | Some _ | None ->
        part.rows <- List.rev_append rows part.rows;
        part.batches <- part.batches + 1;
        Ok ()

  let close part answer =
    ended part;
    let owner = part.owner and name = part.name and index = part.index in
    match owner.fault with
    | Close chunk when chunk = part.chunk ->
        Await.Await.Mpmc_queue.push owner.commits
          (Answer (fun () -> answer (Error Injected)))
    | Clean | Read _ | Render _ | Append _ | Close _ | Table_ddl _ ->
        let rows = List.rev part.rows and batches = part.batches in
        Await.Await.Mpmc_queue.push owner.commits
          (Answer
             (fun () ->
               logged owner (fun log ->
                   log.closed <- ((name, index), rows) :: log.closed;
                   log.appended <- ((name, index), batches) :: log.appended);
               answer
                 (Ok
                    {
                      Manifest.file = File_path.Part.of_string "part";
                      rows = List.length rows;
                      bytes = 0;
                      stored_bytes = 0;
                      checksum = Xxh3_64 0L;
                    })))

  let discard part = ended part

  let finish t ~source:_ ~tables:_ ~manifest:_ =
    logged t (fun log -> log.finished <- log.finished + 1);
    Ok ()
end

let scenario =
  QCheck2.Gen.(
    let* tables =
      list_size (int_range 1 4)
        (let* ids = list_size (int_range 0 120) (int_range 0 1000) in
         let+ bounds = list_size (int_range 0 5) (int_range 1 999) in
         (List.sort_uniq Int.compare ids, List.sort_uniq Int.compare bounds))
    in
    let chunks =
      List.fold_left
        (fun total (_, bounds) -> total + List.length bounds + 1)
        0 tables
    in
    let chunk = int_range 0 (chunks - 1) in
    let* databases = int_range 1 3 in
    let* objects = list_size (pure databases) (int_range 0 4) in
    let* rows =
      oneof_weighted
        [
          (3, pure Every);
          (1, pure No_data);
          ( 2,
            map
              (fun picks ->
                Tables
                  (List.sort_uniq Int.compare
                     (List.map (fun n -> n mod List.length tables) picks)))
              (list_size (int_range 1 3) (int_range 0 3)) );
          (1, map (fun database -> Database database) (int_range 0 2));
          (1, pure Missing);
        ]
    and* taking = oneof_list [ With_objects; Without_objects ] in
    let* reading =
      oneof
        [
          map (fun readers -> Readers readers) (int_range 1 4);
          map2
            (fun cores suggests -> Fitting { cores; suggests })
            (int_range 1 12)
            (option (int_range 1 8));
        ]
    and* rendering =
      oneof [ map (fun renderers -> Renderers renderers) (int_range 1 3); pure Spare ]
    and* batch_bytes = oneof [ int_range 1 64; int_range 64 4096 ]
    and* order = oneof_list Dump_opts.all_of_order
    and* declaring = oneof_list Dump_opts.all_of_declaring in
    let+ fault =
      oneof_weighted
        [
          (4, pure Clean);
          (1, map2 (fun chunk row -> Read { chunk; row }) chunk (int_range 0 60));
          (1, map (fun chunk -> Render chunk) chunk);
          (1, map (fun chunk -> Append chunk) chunk);
          (1, map (fun chunk -> Close chunk) chunk);
          ( 1,
            map
              (fun table -> Table_ddl table)
              (int_range 0 (List.length tables - 1)) );
        ]
    in
    {
      tables;
      databases;
      objects;
      rows;
      taking;
      reading;
      rendering;
      batch_bytes;
      order;
      declaring;
      fault;
      pace = Unpaced;
    })

let print scenario = Sexplib0.Sexp.to_string_hum (sexp_of_scenario scenario)

let holds
    {
      tables;
      databases;
      objects;
      rows;
      taking;
      reading;
      rendering;
      batch_bytes;
      order;
      declaring;
      fault;
      pace;
    } =
      let suggested most =
        match reading with
        | Readers _ | Fitting { suggests = None; _ } -> most
        | Fitting { suggests = Some threads; _ } -> Int.max 1 (Int.min most threads)
      in
      let readers, start =
        match reading with
        | Readers readers -> (readers, readers)
        | Fitting { cores; _ } ->
            let first = Int.max 4 (cores / 2) in
            let sessions = suggested (2 * first) in
            (sessions, Int.min sessions first)
      in
      let cores = match reading with Readers _ -> 4 | Fitting { cores; _ } -> cores in
      let renderers =
        match rendering with
        | Renderers renderers -> renderers
        | Spare -> Int.max 1 (Int.min (start / 2) cores)
      in
      let snapshotted = Atomic.make 0 in
      let schema = List.mapi (fun n _ -> table ~databases n) tables in
      let with_rows n =
        match rows with
        | Every -> true
        | No_data | Missing -> false
        | Tables picked -> List.mem n picked
        | Database database -> n mod databases = database
      and rule n = Filter.Table (table ~databases n).name in
      let filter_rows, unmatched =
        match rows with
        | Every -> (Filter.All_rows, false)
        | No_data -> (No_rows, false)
        | Tables picked ->
            (Rows_of (rule (List.hd picked), List.map rule (List.tl picked)), false)
        | Database n ->
            ( Rows_of (Filter.Database (database n), []),
              not (List.exists (fun t -> t mod databases = n) (List.init (List.length tables) Fun.id)) )
        | Missing ->
            ( Rows_of
                (Table { database = database 0; table = ident "missing" }, []),
              true )
      in
      let database_names = List.init databases database in
      let catalogs =
        List.map
          (fun count -> List.init count (fun n -> ident ("o" ^ string_of_int n)))
          objects
      and failing_ddl =
        match fault with
        | Table_ddl failing -> Some (ident ("t" ^ string_of_int failing))
        | Clean | Read _ | Render _ | Append _ | Close _ -> None
      and context =
        {
          Definition.charset = ident "utf8mb4";
          collation = ident "utf8mb4_bin";
          sql_mode = None;
          time_zone = None;
        }
      in
      let catalog database =
        Option.value ~default:[] (List.nth_opt catalogs database)
      in
      let planned =
        List.concat
          (List.map2
             (fun table (_, bounds) ->
               Support.get_ok
                 (Chunk.of_bounds table
                    (List.map
                       (fun bound -> [ Key.Signed (Int64.of_int bound) ])
                       bounds)))
             schema tables)
      in
      let chunks =
        List.filter
          (fun chunk ->
            match
              List.find_index
                (fun (t : Schema.Table.t) ->
                  Schema.Name.equal t.name (Chunk.table chunk).name)
                schema
            with
            | Some n -> with_rows n
            | None -> false)
          planned
      in
      let numbered =
        List.mapi (fun position chunk -> (position, chunk)) chunks
      in
      let position_of name index =
        let rec find seen = function
          | [] -> -1
          | (position, chunk) :: rest -> (
              match Schema.Name.equal (Chunk.table chunk).name name with
              | true when seen = index -> position
              | true -> find (seen + 1) rest
              | false -> find seen rest)
        in
        find 0 numbered
      in
      let ids_of_table =
        List.map2 (fun (t : Schema.Table.t) (ids, _) -> (t.name, ids)) schema tables
      in
      let (within @ portable) (range : Chunk.Range.t option) id =
        let value (bound : Key.t) =
          match (bound :> Key.value * Key.value list) with
          | Signed value, [] -> Some (Int64.to_int value)
          | _ -> None
        in
        match range with
        | None -> true
        | Some (Below hi) -> Base.Option.exists ~f:(fun hi -> id < hi) (value hi)
        | Some (Between (lo, hi)) ->
            Base.Option.exists ~f:(fun lo -> id >= lo) (value lo)
            && Base.Option.exists ~f:(fun hi -> id < hi) (value hi)
        | Some (From lo) -> Base.Option.exists ~f:(fun lo -> id >= lo) (value lo)
      in
      let (rows_of_chunk @ portable) chunk =
        let range =
          match (chunk : Chunk.t) with Whole _ -> None | Range { range; _ } -> Some range
        in
        List.map Int64.of_int
          (List.filter (within range)
             (Option.value ~default:[]
                (List.find_map
                   (fun (name, ids) ->
                     if Schema.Name.equal name (Chunk.table chunk).name then Some ids
                     else None)
                   ids_of_table)))
      in
      let rows_of position =
        Option.fold ~none:[] ~some:rows_of_chunk (List.assoc_opt position numbered)
      in
      let row_of chunk row = List.nth_opt (rows_of chunk) row in
      let unreadable =
        match fault with
        | Read { chunk; row } -> row_of chunk row
        | Clean | Render _ | Append _ | Close _ | Table_ddl _ -> None
      and doomed =
        match fault with
        | Render chunk | Append chunk ->
            Option.bind (List.assoc_opt chunk numbered) (fun planned ->
                Option.map
                  (fun row -> ((Chunk.table planned).name, row))
                  (row_of chunk 0))
        | Clean | Read _ | Close _ | Table_ddl _ -> None
      in
      let turns = Support.get_ok (Guarded.create (fun () -> Ok ())) in
      let module Provider = struct
        type conn_config = unit
        type position = unit
        type session = int
        type error = Failed_read

        let sexp_of_error Failed_read = Sexplib0.Sexp.Atom "failed read"

        type snapshot_mode = unit
        type 'lvl supports = unit
        type connections = int

        let supports () _ = Ok ()
        let connect () (sessions : Provider_intf.sessions) =
          match sessions with
          | Exactly n -> Ok n
          | At_most most -> Ok (suggested most)

        let opened n = n
        let disconnect _ = ()
        let introspect _ _ = Schema.make schema
        let plan _ (read : Schema.t) _ =
          Ok
            (List.filter
               (fun chunk ->
                 List.exists
                   (fun (t : Schema.Table.t) ->
                     Schema.Name.equal t.name (Chunk.table chunk).name)
                   read.tables)
               planned)

        let snapshot () n =
          Atomic.set snapshotted n;
          Ok
            ( Array.init n (fun session ->
                  Await.Capsule.Await.With_mutex.create (fun () -> session)),
              () )

        let ddl _ (target : Ddl_target.t) =
          match (target, failing_ddl) with
          | Table { table; _ }, Some failing when Ident.equal table failing ->
              Error (`Provider Failed_read)
          | _ -> Ok ""

        let key_span _ chunk =
          Ok
            (Option.bind (Chunk.integer_key chunk) (fun _ ->
                 match rows_of_chunk chunk with
                 | [] -> None
                 | first :: _ as rows ->
                     Some
                       ( Key.Signed first,
                         Key.Signed (List.fold_left Int64.max first rows) )))

        let read_chunk _ chunk (_ : Mask.Table.t) writer =
          let () =
            match pace with
            | Each seconds -> Unix.sleepf seconds
            | Taking_turns seconds ->
                Guarded.hold turns (fun () -> Unix.sleepf seconds)
            | Unpaced | Per_row _ -> ()
          in
          let rows = rows_of_chunk chunk in
          let () =
            match pace with
            | Per_row seconds -> Unix.sleepf (seconds *. Float.of_int (List.length rows))
            | Unpaced | Each _ | Taking_turns _ -> ()
          in
          let pending = ref rows in
          Batch_writer.fill writer (fun columns ->
              match !pending with
              | [] -> Ok Batch_writer.End
              | value :: _ when Option.equal Int64.equal (Some value) unreadable ->
                  Error (`Provider Failed_read)
              | value :: rest ->
                  pending := rest;
                  Support.add_int64 columns value;
                  Ok Row)

        let objects _ database =
          Ok
            (List.map
               (fun name -> (name, Definition.Procedure))
               (catalog
                  (Option.value ~default:0
                     (List.find_index (Ident.equal database) database_names))))

        let definition _ database (name, kind) =
          Ok { Definition.database; name; kind; context; text = "" }

        let close _ = ()
        let sexp_of_position () = Sexplib0.Sexp.Atom "nowhere"
        let source () = ""
        let yojson_of_position () = `Null
        let position_of_yojson _ = ()
      end in
      let module Dump = Dump.Make (Provider) (Recorder) in
      let recorder =
        {
          Recorder.fault;
          chunk_of = position_of;
          doomed;
          log =
            Await.Capsule.Await.With_mutex.create (fun () ->
                {
                  Recorder.opened = [];
                  ended = [];
                  closed = [];
                  appended = [];
                  finished = 0;
                  databases = [];
                  tables = [];
                  grown = [];
                  objects = [];
                  unannounced = 0;
                });
          commits = Await.Await.Mpmc_queue.create ();
        }
      in
      let opts =
        let readers : Dump_opts.readers =
          match reading with Readers readers -> Readers readers | Fitting _ -> Fitting
        and renderers : Dump_opts.renderers =
          match rendering with Renderers renderers -> Renderers renderers | Spare -> Spare
        in
        Support.get_ok
          (Dump_opts.make
             ~plan:(Support.get_ok (Plan_opts.make ~chunk_bytes:1))
             ~readers ~renderers ~cores ~batch_bytes ~order ~declaring)
      in
      let dump_seconds =
        match pace with
        | Unpaced -> 5.
        | Each _ | Taking_turns _ | Per_row _ -> 90.
      in
      let rec commit () =
        match
          Await.Await.Mpmc_queue.pop Blocking.await recorder.commits
        with
        | Stop -> ()
        | Answer answer ->
            for _ = 1 to 3 do
              Thread.yield ()
            done;
            answer ();
            commit ()
      in
      Support.spawn commit;
      let outcome =
        Support.within dump_seconds (fun () ->
            Dump.run
              {
                conn = ();
                mode = ();
                level = Best_effort;
                filter = Everything;
                rows = filter_rows;
                objects =
                  (match taking with
                  | With_objects -> Filter.With_objects
                  | Without_objects -> Without_objects);
                masking = Mask.none;
                opts;
              }
              recorder)
      in
      Await.Await.Mpmc_queue.push recorder.commits Stop;
      let opened, ended, closed, appended, finished =
        Recorder.logged recorder
          (fun { opened; ended; closed; appended; finished; _ } ->
            (opened, ended, closed, appended, finished))
      and announced_databases, announced_tables, announced_objects, unannounced
          =
        Recorder.logged recorder
          (fun { databases; tables; objects; unannounced; _ } ->
            (databases, tables, objects, unannounced))
      in
      match outcome with
      | Error `Timeout -> false
      | Ok outcome -> (
          let ids_of name =
            Option.fold ~none:[]
              ~some:(fun n ->
                if with_rows n then List.map Int64.of_int (fst (List.nth tables n))
                else [])
              (List.find_index
                 (fun (t : Schema.Table.t) -> Schema.Name.equal t.name name)
                 schema)
          and grown = Recorder.logged recorder (fun { grown; _ } -> grown) in
          let paced (manifest : Manifest.t) =
            let read =
              List.filter_map
                (fun ({ state; _ } : Manifest.entry) ->
                  match state with
                  | Done { reading = { reader; started; _ }; _ } -> Some (reader, started)
                  | Pending -> None)
                manifest.chunks
            in
            let after seconds =
              List.sort_uniq Int.compare
                (List.filter_map
                   (fun (reader, started) ->
                     if Core.Time_ns.Span.( >= ) started (Core.Time_ns.Span.of_sec seconds)
                     then Some reader
                     else None)
                   read)
            and joined = List.exists (fun (reader, _) -> reader >= start) read in
            let waited =
              List.for_all
                (fun (reader, started) ->
                  reader < start
                  || Core.Time_ns.Span.( >= ) started (Core.Time_ns.Span.of_sec 1.))
                read
            in
            match pace with
            | Unpaced -> waited
            | Each _ -> waited && joined
            | Taking_turns _ -> waited && joined && List.length (after 6.) <= start
            | Per_row _ ->
                let first = (List.hd schema).name in
                let rows_by (entry : Manifest.entry) =
                  match entry.state with
                  | Done { part = { rows; _ }; reading = { reader; _ }; _ }
                    when rows > 0 && Schema.Name.equal entry.table first ->
                      Some reader
                  | Done _ | Pending -> None
                and planned_with_rows =
                  List.filter
                    (fun chunk ->
                      Schema.Name.equal (Chunk.table chunk).name first
                      && rows_of_chunk chunk <> [])
                    chunks
                in
                List.length
                  (List.sort_uniq Int.compare (List.filter_map rows_by manifest.chunks))
                > List.length planned_with_rows
          in
          let rec ascending = function
            | a :: (b :: _ as rest) -> Int64.compare a b < 0 && ascending rest
            | [ _ ] | [] -> true
          in
          let parts_of name =
            List.sort compare
              (List.filter_map
                 (fun ((n, index), rows) ->
                   if Schema.Name.equal n name then Some (index, rows) else None)
                 closed)
          in
          let triggered =
            match fault with
            | _ when unmatched -> `Fails
            | Clean -> `Clean
            | Read { chunk; row } when row < List.length (rows_of chunk) ->
                `Fails
            | (Render chunk | Append chunk) when rows_of chunk <> [] -> `Fails
            | Close chunk when List.mem_assoc chunk numbered -> `Fails
            | Table_ddl _ -> `Fails
            | Read _ | Render _ | Append _ | Close _ -> `Clean
          in
          let used = List.sort_uniq Int.compare (List.init (List.length tables) (fun n -> n mod databases)) in
          unannounced = 0
          && List.sort compare opened = List.sort compare ended
          && List.length announced_databases
             = List.length (List.sort_uniq Ident.compare announced_databases)
          && List.length announced_tables
             = List.length (List.sort_uniq compare (List.map fst announced_tables))
          && List.for_all
               (fun (name, parts) ->
                 parts
                 = List.length
                     (List.filter
                        (fun chunk -> Schema.Name.equal (Chunk.table chunk).name name)
                        chunks))
               announced_tables
          && (match taking with
             | With_objects -> true
             | Without_objects -> announced_objects = [])
          && List.for_all
               (fun (database, names) ->
                 match List.find_index (Ident.equal database) database_names with
                 | Some n -> List.equal Ident.equal names (catalog n)
                 | None -> false)
               announced_objects
          && List.length announced_objects
             = List.length (List.sort_uniq compare (List.map fst announced_objects))
          && List.for_all
               (fun ((name, _), rows) ->
                 ascending rows
                 && List.for_all (fun row -> List.mem row (ids_of name)) rows)
               closed
          && (let rows =
                List.concat_map
                  (fun ((name, _), rows) -> List.map (fun row -> (name, row)) rows)
                  closed
              in
              List.length rows = List.length (List.sort_uniq compare rows))
          &&
          match (triggered, outcome) with
          | `Clean, Ok manifest ->
              finished = 1
              && Atomic.get snapshotted = readers
              && manifest.threads.sessions = readers
              && manifest.threads.readers = start
              && manifest.threads.renderers = renderers
              && manifest.threads.peak <= readers
              && manifest.threads.peak >= start
              && paced manifest
              && List.length announced_tables = List.length tables
              && List.sort Ident.compare announced_databases
                 = List.map database used
              && List.length announced_objects
                 = (match taking with
                   | With_objects -> List.length used
                   | Without_objects -> 0)
              && List.for_all
                   (fun (t : Schema.Table.t) ->
                     let parts = parts_of t.name in
                     List.sort Int64.compare (List.concat_map snd parts)
                     = ids_of t.name
                     && List.map fst parts
                        = List.init (List.length parts) Fun.id
                     && List.length parts
                        = List.length
                            (List.filter
                               (fun chunk ->
                                 Schema.Name.equal (Chunk.table chunk).name t.name)
                               chunks)
                          + List.length (List.filter (Schema.Name.equal t.name) grown)
                     && List.map snd parts
                        = List.map
                            (fun ({ range; _ } : Manifest.entry) ->
                              List.filter
                                (fun row -> within range (Int64.to_int row))
                                (ids_of t.name))
                            (List.filter
                               (fun ({ table; _ } : Manifest.entry) ->
                                 Schema.Name.equal table t.name)
                               manifest.chunks))
                   schema
              && List.for_all
                   (fun (key, ({ state; _ } : Manifest.entry)) ->
                     match state with
                     | Done { reading = { reader; batches; _ }; _ } ->
                         reader >= 0 && reader < readers
                         && Option.equal Int.equal (Some batches)
                              (List.assoc_opt key appended)
                     | Pending -> false)
                   (List.concat_map
                      (fun (t : Schema.Table.t) ->
                        List.mapi
                          (fun index entry -> ((t.name, index), entry))
                          (List.filter
                             (fun ({ table; _ } : Manifest.entry) ->
                               Schema.Name.equal table t.name)
                             manifest.chunks))
                      schema)
          | `Fails, Error _ -> finished = 0
          | `Clean, Error _ | `Fails, Ok _ -> false)

let dumped =
  QCheck2.Test.make ~count:150
    ~name:
      "a dump hands every chunk's rows to the sink in order and ends each part \
       once, or fails with no finish" ~print scenario holds

let paced pace =
  holds
    {
      tables = [ (List.init 350 Fun.id, List.init 349 (fun n -> n + 1)) ];
      databases = 1;
      objects = [ 0 ];
      rows = Every;
      taking = Without_objects;
      reading = Fitting { cores = 8; suggests = None };
      rendering = Spare;
      batch_bytes = 4096;
      order = Largest_chunks;
      declaring = Before_its_rows;
      fault = Clean;
      pace;
    }

let tests =
  [
    QCheck_alcotest.to_alcotest dumped;
    Alcotest.test_case
      "an idle reader gets the unread half of a chunk another reader is in"
      `Slow (fun () ->
        Alcotest.(check bool) "dumped, chunks divided" true
          (holds
             {
               tables =
                 [ (List.init 2000 Fun.id, []) ];
               databases = 1;
               objects = [ 0 ];
               rows = Every;
               taking = Without_objects;
               reading = Readers 4;
               rendering = Spare;
               batch_bytes = 4096;
               order = Largest_chunks;
               declaring = Before_its_rows;
               fault = Clean;
               pace = Per_row 0.0005;
             }));
    Alcotest.test_case
      "waiting readers join while reading holds the dump back and more readers \
       read more"
      `Slow (fun () ->
        Alcotest.(check bool) "dumped, more readers at once" true
          (paced (Each 0.1)));
    Alcotest.test_case
      "readers that joined for a trial that did not pay wait again, and every \
       reader ends with the dump"
      `Slow (fun () ->
        match Sys.getenv_opt "CI" with
        | Some "true" -> Alcotest.skip ()
        | Some _ | None ->
            Alcotest.(check bool)
              "dumped, a trial taken, readers back to the start in one of three runs" true
              (List.exists (fun () -> paced (Taking_turns 0.02)) [ (); (); () ]));
  ]
