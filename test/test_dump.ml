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

type scenario = {
  tables : (int list * int list) list;
      (** Per table: its ids, ascending, and the bounds its chunks are cut at.
      *)
  databases : int;  (** Table [n] is in database [n mod databases]. *)
  objects : int list;  (** Per database, how many objects it has. *)
  readers : int;
  renderers : int;
  batch_bytes : int;
  fault : fault;
}
[@@deriving sexp_of]

let ident = Support.ident

let database n = ident ("db" ^ string_of_int n)

let table ~databases n =
  Support.get_ok
    (Schema.Table.make
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
    mutable objects : (Ident.t * Ident.t list) list;
    mutable unannounced : int;
        (** Tables announced before their database, parts before their table. *)
  }

  type t = {
    fault : fault;
    chunk_of : Schema.Name.t -> int -> int @@ portable;
        (** A part's chunk in the plan. *)
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

  type renderer = { failing : fault; at : int }
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

  let part t ({ name; _ } : Schema.Table.t) ~index =
    logged t (fun log ->
        (match List.mem_assoc name log.tables with
        | true -> ()
        | false -> log.unannounced <- log.unannounced + 1);
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

  let renderer part = { failing = part.owner.fault; at = part.chunk }

  let render { failing; at } (batch : Batch.sealed @ local) =
    match (failing, Support.int64s batch) with
    | Render chunk, _ when chunk = at -> Error (`Sink Injected)
    | _, Some values -> Ok { values }
    | _, None -> Error (`Sink Injected)

  let append part { values = rows } =
    match part.owner.fault with
    | Append chunk when chunk = part.chunk -> Error (`Sink Injected)
    | Clean | Read _ | Render _ | Append _ | Close _ | Table_ddl _ ->
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
    let* readers = int_range 1 4
    and* renderers = int_range 1 3
    and* batch_bytes = oneof [ int_range 1 64; int_range 64 4096 ] in
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
    { tables; databases; objects; readers; renderers; batch_bytes; fault })

let print scenario = Sexplib0.Sexp.to_string_hum (sexp_of_scenario scenario)

let dumped =
  QCheck2.Test.make ~count:150
    ~name:
      "a dump hands every chunk's rows to the sink in order and ends each part \
       once, or fails with no finish" ~print scenario
    (fun { tables; databases; objects; readers; renderers; batch_bytes; fault } ->
      let schema = List.mapi (fun n _ -> table ~databases n) tables in
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
      let chunks =
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
      let rows_of position =
        let chunk = List.assoc position numbered in
        let table_number =
          match
            List.find_index
              (fun (t : Schema.Table.t) ->
                Schema.Name.equal t.name (Chunk.table chunk).name)
              schema
          with
          | Some number -> number
          | None -> Alcotest.fail "chunk of no table"
        in
        let ids = fst (List.nth tables table_number) in
        let value (bound : Key.t) =
          match (bound :> Key.value * Key.value list) with
          | Signed value, [] -> Int64.to_int value
          | _ -> Alcotest.fail "not an integer bound"
        in
        List.map Int64.of_int
          (List.filter
             (fun id ->
               match chunk with
               | Whole _ -> true
               | Range { range = Below hi; _ } -> id < value hi
               | Range { range = Between (lo, hi); _ } ->
                   id >= value lo && id < value hi
               | Range { range = From lo; _ } -> id >= value lo)
             ids)
      in
      let served =
        List.map (fun (position, chunk) -> (chunk, position, rows_of position))
          numbered
      in
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
        let connect () ~n = Ok n
        let disconnect _ = ()
        let introspect _ _ = Schema.make schema
        let plan _ _ _ = Ok chunks

        let snapshot () n =
          Ok
            ( Array.init n (fun session ->
                  Await.Capsule.Await.With_mutex.create (fun () -> session)),
              () )

        let ddl _ (target : Ddl_target.t) =
          match (target, failing_ddl) with
          | Table { table; _ }, Some failing when Ident.equal table failing ->
              Error (`Provider Failed_read)
          | _ -> Ok ""

        let read_chunk _ chunk writer =
          let position, rows =
            match List.find_opt (fun (c, _, _) -> c == chunk) served with
            | Some (_, position, rows) -> (position, rows)
            | None -> (-1, [])
          in
          let pending = ref rows and at = ref 0 in
          Batch_writer.fill writer (fun columns ->
              match !pending with
              | [] -> Ok Batch_writer.End
              | value :: rest -> (
                  match fault with
                  | Read { chunk; row } when chunk = position && row = !at ->
                      Error (`Provider Failed_read)
                  | Clean | Read _ | Render _ | Append _ | Close _ | Table_ddl _
                    ->
                      pending := rest;
                      incr at;
                      Support.add_int64 columns value;
                      Ok Row))

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
                  objects = [];
                  unannounced = 0;
                });
          commits = Await.Await.Mpmc_queue.create ();
        }
      in
      let opts =
        Support.get_ok
          (Dump_opts.make
             ~plan:(Support.get_ok (Plan_opts.make ~chunk_bytes:1))
             ~threads:readers ~render_threads:renderers ~batch_bytes)
      in
      let dump_seconds = 5. in
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
          let parts =
            List.map
              (fun (position, chunk) ->
                let name = (Chunk.table chunk).name in
                let index =
                  List.length
                    (List.filter
                       (fun (p, c) ->
                         p < position
                         && Schema.Name.equal (Chunk.table c).name name)
                       numbered)
                in
                ((name, index), rows_of position))
              numbered
          in
          let triggered =
            match fault with
            | Clean -> `Clean
            | Read { chunk; row } when row < List.length (rows_of chunk) ->
                `Fails
            | (Render chunk | Append chunk) when rows_of chunk <> [] -> `Fails
            | Close _ | Table_ddl _ -> `Fails
            | Read _ | Render _ | Append _ -> `Clean
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
          && List.for_all
               (fun (database, names) ->
                 match List.find_index (Ident.equal database) database_names with
                 | Some n -> List.equal Ident.equal names (catalog n)
                 | None -> false)
               announced_objects
          && List.length announced_objects
             = List.length (List.sort_uniq compare (List.map fst announced_objects))
          && List.for_all
               (fun (key, rows) ->
                 Option.fold ~none:true ~some:(( = ) rows)
                   (List.assoc_opt key closed))
               parts
          &&
          match (triggered, outcome) with
          | `Clean, Ok manifest ->
              finished = 1
              && List.length announced_tables = List.length tables
              && List.sort Ident.compare announced_databases
                 = List.map database used
              && List.length announced_objects = List.length used
              && List.sort compare closed = List.sort compare parts
              && List.for_all2
                   (fun (key, _) ({ state; _ } : Manifest.entry) ->
                     match state with
                     | Done { reading = { reader; batches; _ }; _ } ->
                         reader >= 0 && reader < readers
                         && Option.equal Int.equal (Some batches)
                              (List.assoc_opt key appended)
                     | Pending -> false)
                   parts manifest.chunks
          | `Fails, Error _ -> finished = 0
          | `Clean, Error _ | `Fails, Ok _ -> false))

let tests = List.map QCheck_alcotest.to_alcotest [ dumped ]
