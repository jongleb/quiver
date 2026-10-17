open Base.Result.Let_syntax

module Make (P : Provider_intf.S) (S : Sink_intf.S) = struct
  module Failure = struct
    type error : value mod contended portable =
      [ Error.t | `Provider of P.error | `Sink of S.error ]
    [@@deriving sexp_of ~portable]
  end

  include Failure

  module Part = Assembly.Make (S)

  (** Work for the render threads. *)
  type job =
    | Render of {
        batch : Batch.sealed;
        part : Part.t @@ aliased many;
        ticket : int;
        pool : Batch_pool.t @@ aliased many;
      }
    | Stop

  (** Who closes the sessions: still the caller, or their readers. *)
  type sessions = Held | Given

  let close session = Guarded.hold session (fun session -> P.close session)

  type 'lvl request = {
    conn : P.conn_config;
    mode : P.snapshot_mode;
    level : 'lvl Consistency.t;
    filter : Filter.t;
    opts : Dump_opts.t;
  }

  (** Work for the readers, in the order they take it. *)
  type work =
    | Database of int
    | Table of int
    | Chunk of int  (** Position in the plan. *)
    | Catalog of int  (** The objects of a database. *)

  (** Whether a database or table reached the sink. *)
  type announcement = Announced | Withheld

  type catalog = {
    entries : (int * (Ident.t * Definition.kind)) iarray;
        (** Each object, after its database. *)
    found : Definition.t option Portable.Atomic_array.t;
  }
  (** Every object of the dump, known once each database's catalog is. *)

  let run { conn; mode; level; filter; opts } sink :
      (Manifest.t, error) result =
    let began = Stopwatch.start () in
    let%bind supports = P.supports mode level in
    let%bind connections = P.connect conn ~n:opts.threads in
    let%bind schema, chunks =
      match
        let%bind schema = P.introspect connections filter in
        let%map chunks =
          P.plan connections schema
            (Plan_opts.shared opts.plan
               ~bytes:
                 (List.fold_left
                    (fun total { Schema.Table.estimate = { bytes; _ }; _ } ->
                      total + bytes)
                    0 schema.tables)
               ~readers:opts.threads)
        in
        (schema, chunks)
      with
      | Ok planned -> Ok planned
      | Error error ->
          P.disconnect connections;
          Error error
    in
    let%bind sessions, position = P.snapshot supports connections in
    let held = ref Held in
    let dump () =
      let databases =
        Iarray.of_list
          (List.sort_uniq Ident.compare
             (List.map
                (fun { Schema.Table.name = { database; _ }; _ } -> database)
                schema.tables))
      and tables = Iarray.of_list schema.tables
      and chunks = Array.of_list chunks in
      let database_index = Hashtbl.create 16
      and table_index = Base.Hashtbl.create (module Schema.Name) in
      Iarray.iteri
        (fun index database -> Hashtbl.replace database_index database index)
        databases;
      Iarray.iteri
        (fun index { Schema.Table.name; _ } ->
          Base.Hashtbl.set table_index ~key:name ~data:index)
        tables;
      let chunk_tables =
        Array.map
          (fun chunk ->
            Base.Hashtbl.find_exn table_index (Chunk.table chunk).name)
          chunks
      in
      let parts = Array.make (Iarray.length tables) 0 in
      let indexes =
        Array.map
          (fun table ->
            let index = parts.(table) in
            parts.(table) <- index + 1;
            index)
          chunk_tables
      in
      let table_databases =
        Iarray.map
          (fun { Schema.Table.name = { database; _ }; _ } ->
            Hashtbl.find database_index database)
          tables
      in
      let order =
        let sizes =
          Array.mapi
            (fun position chunk ->
              (Chunk.table chunk).estimate.bytes
              / Int.max 1 parts.(chunk_tables.(position)))
            chunks
        in
        let order = Array.init (Array.length chunks) Fun.id in
        Array.stable_sort (fun a b -> Int.compare sizes.(b) sizes.(a)) order;
        order
      in
      let items =
        let seen_databases = Hashtbl.create 16
        and seen_tables = Hashtbl.create 64 in
        let first seen key work =
          match Hashtbl.find_opt seen key with
          | Some () -> []
          | None ->
              Hashtbl.replace seen key ();
              [ work ]
        in
        Iarray.of_list
          (List.concat_map
             (fun position ->
               let table = chunk_tables.(position) in
               let database = Iarray.get table_databases table in
               first seen_databases database (Database database)
               @ first seen_tables table (Table table)
               @ [ Chunk position ])
             (Array.to_list order)
          @ List.init (Iarray.length databases) (fun database ->
                Catalog database))
      in
      let shared = Iarray.of_array chunks
      and chunk_tables = Iarray.of_array chunk_tables
      and parts = Iarray.of_array parts
      and indexes = Iarray.of_array indexes
      and database_announcements =
        Iarray.init (Iarray.length databases) (fun _ -> Await.Await.Ivar.create ())
      and table_announcements =
        Iarray.init (Iarray.length tables) (fun _ -> Await.Await.Ivar.create ())
      and catalogs =
        Portable.Atomic_array.create ~len:(Iarray.length databases) []
      and catalogs_left = Portable.Atomic.make (Iarray.length databases)
      and catalogued = Await.Await.Ivar.create ()
      and remaining =
        Iarray.init (Iarray.length databases) (fun _ -> Portable.Atomic.make 0)
      and next = Portable.Atomic.make 0
      and next_definition = Portable.Atomic.make 0
      and failure = Portable.Atomic.make None
      and states =
        Portable.Atomic_array.create ~len:(Array.length chunks) Manifest.Pending
      and committing = Portable.Atomic.make 1
      and all_committed = Await.Await.Ivar.create ()
      and jobs = Portable_mpmc_queue.create ()
      and ready = Await.Await.Semaphore.create 0
      and limit_bytes = opts.batch_bytes in
      let (nothing_catalogued @ portable) () =
        Await.Await.Ivar.fill_if_empty catalogued
          {
            entries = Iarray.of_list [];
            found = Portable.Atomic_array.create ~len:0 None;
          }
      in
      (match Iarray.length databases with
      | 0 -> nothing_catalogued ()
      | _ -> ());
      let (fail @ portable) error =
        ignore
          (Portable.Atomic.compare_and_set failure ~if_phys_equal_to:None
             ~replace_with:(Some error));
        nothing_catalogued ()
      in
      let (committed @ portable) () =
        if Portable.Atomic.fetch_and_add committing (-1) = 1 then
          Await.Await.Ivar.fill_if_empty all_committed ()
      in
      let (render @ portable) () =
        let rec take () =
          Await.Await.Semaphore.acquire Blocking.await ready;
          match Portable_mpmc_queue.pop jobs with
          | Null -> take ()
          | This job -> (
              match Handoff.own job with
              | Stop -> ()
              | Render { batch; part; ticket; pool } ->
                  let rendered = Part.render part (borrow_ batch) in
                  Batch_pool.give pool (Batch.unseal batch);
                  Part.deliver part ticket rendered;
                  take ())
        in
        take ()
      in
      let (reader @ portable) index session =
        let pool =
          let batches_per_reader = 4 in
          Batch_pool.create ~batches:batches_per_reader
        in
        let announce announcement (result : (unit, error) result) =
          Await.Await.Ivar.fill_if_empty announcement
            (match result with
            | Ok () -> Announced
            | Error error ->
                fail error;
                Withheld)
        in
        let announced announcement =
          Await.Await.Ivar.read Blocking.await announcement
        in
        let ddl target =
          Guarded.hold session (fun session ->
              (P.ddl session target : (string, error) result))
        in
        let read position =
          let chunk = Iarray.get shared position in
          match
            Part.create sink (Chunk.table chunk)
              ~index:(Iarray.get indexes position)
          with
          | Error error -> fail (`Sink error)
          | Ok part -> (
              let started = Stopwatch.start ()
              and since = Stopwatch.elapsed began
              and waited = Batch_pool.waited pool
              and allocated = Gc.minor_words () in
              let first, rest = (Chunk.table chunk).fields in
              let ltypes =
                List.map
                  (fun { Schema.Field.ltype; _ } -> ltype)
                  (first :: rest)
              in
              match
                Guarded.hold session (fun session ->
                    let tickets = ref 0 in
                    let writer =
                      Batch_writer.create ~limit_bytes pool
                        (module struct
                          include Failure

                          let ship (batch : Batch.sealed @ unique) pool =
                            let ticket = !tickets in
                            incr tickets;
                            Portable_mpmc_queue.push jobs
                              (Render { batch; part; ticket; pool });
                            Await.Await.Semaphore.release ready;
                            Ok ()
                        end)
                        ltypes
                    in
                    match P.read_chunk session chunk writer with
                    | Ok () -> Ok !tickets
                    | Error error -> Error error)
              with
              | Error error ->
                  Part.abandon part;
                  fail error
              | Ok batches ->
                  let reading =
                    {
                      Manifest.reader = index;
                      started = since;
                      duration = Stopwatch.elapsed started;
                      pool_wait =
                        Core.Time_ns.Span.( - ) (Batch_pool.waited pool) waited;
                      batches;
                      allocated_words =
                        int_of_float (Gc.minor_words () -. allocated);
                    }
                  in
                  Portable.Atomic.incr committing;
                  Part.seal part ~batches (function
                    | Closed { part; writing } ->
                        Portable.Atomic_array.set states position
                          (Manifest.Done { part; reading; writing });
                        committed ()
                    | Failed error ->
                        fail (`Sink error);
                        committed ()))
        in
        let objects database definitions =
          match S.objects sink (Iarray.get databases database) definitions with
          | Ok () -> ()
          | Error error -> fail error
        in
        let catalog database =
          let name = Iarray.get databases database in
          (match announced (Iarray.get database_announcements database) with
          | Withheld -> ()
          | Announced -> (
              match
                Guarded.hold session (fun session ->
                    (P.objects session name
                      : ((Ident.t * Definition.kind) list, error) result))
              with
              | Error error -> fail error
              | Ok [] -> objects database []
              | Ok entries ->
                  Portable.Atomic.set (Iarray.get remaining database)
                    (List.length entries);
                  Portable.Atomic_array.set catalogs database entries));
          match Portable.Atomic.fetch_and_add catalogs_left (-1) with
          | 1 ->
              let entries =
                List.concat
                  (List.init (Iarray.length databases) (fun database ->
                       List.map
                         (fun entry -> (database, entry))
                         (Portable.Atomic_array.get catalogs database)))
              in
              Await.Await.Ivar.fill_if_empty catalogued
                {
                  entries = Iarray.of_list entries;
                  found =
                    Portable.Atomic_array.create ~len:(List.length entries) None;
                }
          | _ -> ()
        in
        let perform = function
          | Database database ->
              let name = Iarray.get databases database in
              announce
                (Iarray.get database_announcements database)
                (let%bind ddl = ddl (Database name) in
                 S.database sink name ~ddl)
          | Table table -> (
              let announcement = Iarray.get table_announcements table in
              match
                announced
                  (Iarray.get database_announcements
                     (Iarray.get table_databases table))
              with
              | Withheld -> Await.Await.Ivar.fill_if_empty announcement Withheld
              | Announced ->
                  let table = Iarray.get tables table
                  and parts = Iarray.get parts table in
                  announce announcement
                    (let%bind ddl = ddl (Table table.name) in
                     S.table sink table ~ddl ~parts))
          | Chunk position -> (
              match
                announced
                  (Iarray.get table_announcements
                     (Iarray.get chunk_tables position))
              with
              | Withheld -> ()
              | Announced -> read position)
          | Catalog database -> catalog database
        in
        let define { entries; found } =
          let rec take () =
            let turn = Portable.Atomic.fetch_and_add next_definition 1 in
            match Portable.Atomic.get failure with
            | Some _ -> ()
            | None when turn >= Iarray.length entries -> ()
            | None ->
                let database, entry = Iarray.get entries turn in
                (match
                   Guarded.hold session (fun session ->
                       (P.definition session (Iarray.get databases database) entry
                         : (Definition.t, error) result))
                 with
                | Ok definition ->
                    Portable.Atomic_array.set found turn (Some definition)
                | Error error -> fail error);
                (match
                   Portable.Atomic.fetch_and_add (Iarray.get remaining database) (-1)
                 with
                | 1 ->
                    Option.iter (objects database)
                      (Base.Option.all
                         (List.filter_map Fun.id
                            (List.init (Iarray.length entries) (fun at ->
                                 match Iarray.get entries at with
                                 | owner, _ when owner = database ->
                                     Some (Portable.Atomic_array.get found at)
                                 | _ -> None))))
                | _ -> ());
                take ()
          in
          take ()
        in
        let rec serve () =
          let turn = Portable.Atomic.fetch_and_add next 1 in
          match Portable.Atomic.get failure with
          | Some _ -> ()
          | None when turn >= Iarray.length items ->
              define (Await.Await.Ivar.read Blocking.await catalogued)
          | None ->
              perform (Iarray.get items turn);
              serve ()
        in
        serve ();
        close session
      in
      let renderers =
        List.init opts.render_threads (fun _ -> Concurrent.start render)
      in
      held := Given;
      let readers =
        Array.to_list
          (Array.mapi
             (fun index
                  (session : P.session Await.Capsule.Await.With_mutex.t) ->
               match Concurrent.start (fun () -> reader index session) with
               | Ok reading -> Some reading
               | Error error ->
                   fail error;
                   close session;
                   None)
             sessions)
      in
      List.iter (Option.iter Concurrent.join) readers;
      List.iter
        (fun _ ->
          Portable_mpmc_queue.push jobs Stop;
          Await.Await.Semaphore.release ready)
        renderers;
      List.iter (Result.iter Concurrent.join) renderers;
      committed ();
      Await.Await.Ivar.read Blocking.await all_committed;
      match Portable.Atomic.get failure with
      | Some error -> Error error
      | None ->
          let manifest =
            Manifest.make ~level:(Any level) ~format:(S.format sink)
              ~position:(P.yojson_of_position position)
              ~schema
              (List.init (Array.length chunks) (fun position ->
                   (chunks.(position), Portable.Atomic_array.get states position)))
          in
          let rows = Base.Hashtbl.create (module Schema.Name) in
          Array.iteri
            (fun position chunk ->
              match Portable.Atomic_array.get states position with
              | Manifest.Done { part; _ } ->
                  Base.Hashtbl.incr rows ~by:part.rows
                    (Chunk.table chunk).Schema.Table.name
              | Manifest.Pending -> ())
            chunks;
          let%map () =
            S.finish sink ~source:(P.source position)
              ~tables:
                (List.map
                   (fun { Schema.Table.name; _ } ->
                     ( name,
                       Option.value (Base.Hashtbl.find rows name) ~default:0 ))
                   schema.tables)
              ~manifest:(Manifest.yojson_of_t manifest)
          in
          manifest
    in
    let result = dump () in
    (match !held with Held -> Array.iter close sessions | Given -> ());
    result
end
