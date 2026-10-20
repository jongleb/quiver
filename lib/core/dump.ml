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

  let close session = Guarded.hold session (fun session -> P.close session)

  type 'lvl request = {
    conn : P.conn_config;
    mode : P.snapshot_mode;
    level : 'lvl Consistency.t;
    filter : Filter.t;
    rows : Filter.rows;
    objects : Filter.objects;
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

  (** Whether readers still take work. *)
  type ending = Going | Ended

  (** What a reader does before its next work. *)
  type gate = Stay | Park

  (** Whether a reader took one of the idle readers' requests for work. *)
  type claim = Claimed | Unclaimed

  (** Whether an idle reader's request for work is counted. *)
  type hunger = Registered | Unregistered

  type catalog = {
    entries : (int * (Ident.t * Definition.kind)) iarray;
        (** Each object, after its database. *)
    found : Definition.t option Portable.Atomic_array.t;
  }
  (** Every object of the dump, known once each database's catalog is. *)

  let run { conn; mode; level; filter; rows; objects; opts } sink :
      (Manifest.t, error) result =
    let began = Stopwatch.start () in
    let%bind supports = P.supports mode level in
    let sessions : Provider_intf.sessions =
      match opts.readers with
      | Readers threads -> Exactly threads
      | Fitting -> At_most (Dump_opts.most_readers opts)
    in
    let%bind connections = P.connect conn sessions in
    let%bind schema, chunks =
      match
        let%bind schema = P.introspect connections filter in
        let%bind { with_rows; _ } = Filter.split rows schema.tables in
        let%bind read = Schema.make with_rows in
        let%map chunks =
          P.plan connections read
            (Plan_opts.shared opts.plan
               ~bytes:
                 (Base.List.sum
                    (module Base.Int)
                    with_rows
                    ~f:(fun { Schema.Table.estimate = { bytes; _ }; _ } -> bytes))
               ~readers:(P.opened connections))
        in
        (schema, chunks)
      with
      | Ok planned -> Ok planned
      | Error error ->
          P.disconnect connections;
          Error error
    in
    let%bind sessions, position = P.snapshot supports connections in
    let dump () =
      let databases =
        Iarray.of_list
          (List.sort_uniq Ident.compare
             (List.map
                (fun { Schema.Table.name = { database; _ }; _ } -> database)
                schema.tables))
      and tables = Iarray.of_list schema.tables
      and chunks = Array.of_list chunks in
      let database_index = Base.Hashtbl.create (module Ident)
      and table_index = Base.Hashtbl.create (module Schema.Name) in
      Iarray.iteri
        (fun index database -> Base.Hashtbl.set database_index ~key:database ~data:index)
        databases;
      Iarray.iteri
        (fun index { Schema.Table.name; _ } ->
          Base.Hashtbl.set table_index ~key:name ~data:index)
        tables;
      let chunk_tables_planned =
        Array.map
          (fun chunk ->
            Base.Hashtbl.find_exn table_index (Chunk.table chunk).name)
          chunks
      in
      let parts = Array.make (Iarray.length tables) 0 in
      let indexes_planned =
        Array.map
          (fun table ->
            let index = parts.(table) in
            parts.(table) <- index + 1;
            index)
          chunk_tables_planned
      in
      let table_databases =
        Iarray.map
          (fun { Schema.Table.name = { database; _ }; _ } ->
            Base.Hashtbl.find_exn database_index database)
          tables
      in
      let order =
        let sizes =
          Array.mapi
            (fun position chunk ->
              (Chunk.table chunk).estimate.bytes
              / Int.max 1 parts.(chunk_tables_planned.(position)))
            chunks
        in
        let order = Array.init (Array.length chunks) Fun.id in
        Array.stable_sort (fun a b -> Int.compare sizes.(b) sizes.(a)) order;
        order
      in
      let items =
        let seen_databases = Base.Hash_set.create (module Base.Int)
        and seen_tables = Base.Hash_set.create (module Base.Int) in
        let first seen key work =
          if Base.Hash_set.mem seen key then []
          else
            let () = Base.Hash_set.add seen key in
            [ work ]
        in
        let announced table =
          let database = Iarray.get table_databases table in
          let before = first seen_databases database (Database database) in
          before @ first seen_tables table (Table table)
        in
        let reads =
          List.concat_map
            (fun position ->
              announced chunk_tables_planned.(position) @ [ Chunk position ])
            (Array.to_list order)
        in
        let schema_only =
          List.concat_map announced
            (List.filter
               (fun table -> parts.(table) = 0)
               (List.init (Iarray.length tables) Fun.id))
        in
        let catalogs =
          match objects with
          | With_objects ->
              List.init (Iarray.length databases) (fun database -> Catalog database)
          | Without_objects -> []
        in
        Iarray.of_list (reads @ schema_only @ catalogs)
      in
      let sessions_count = Array.length sessions in
      let start =
        match opts.readers with
        | Readers _ -> sessions_count
        | Fitting -> Int.min sessions_count (Dump_opts.most_readers opts / 2)
      in
      let rendering =
        match opts.renderers with
        | Renderers threads -> threads
        | Spare -> Int.max 1 (Int.min (start / 2) opts.cores)
      in
      let capacity = Array.length chunks + (64 * sessions_count) in
      let planned = Iarray.of_array chunks
      and divided = Portable.Atomic_array.create ~len:capacity None
      and chunk_tables = Portable.Atomic_array.create ~len:capacity 0
      and indexes = Portable.Atomic_array.create ~len:capacity 0
      and widths = Portable.Atomic_array.create ~len:capacity 0L in
      Array.iteri
        (fun position table ->
          Portable.Atomic_array.set chunk_tables position table;
          Portable.Atomic_array.set indexes position indexes_planned.(position))
        chunk_tables_planned;
      let (shared @ portable) position =
        match Portable.Atomic_array.get divided position with
        | Some chunk -> Some chunk
        | None ->
            if position < Iarray.length planned then
              Some (Iarray.get planned position)
            else None
      in
      let table_parts =
        Iarray.init (Array.length parts) (fun table -> Portable.Atomic.make parts.(table))
      and parts = Iarray.of_array parts
      and used = Portable.Atomic.make (Array.length chunks)
      and hungry = Portable.Atomic.make 0
      and spill = Await.Await.Mpmc_queue.create ()
      and offers = Await.Await.Semaphore.create 0
      and busy = Portable.Atomic.make 0
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
      and states = Portable.Atomic_array.create ~len:capacity Manifest.Pending
      and committing = Portable.Atomic.make 1
      and all_committed = Await.Await.Ivar.create ()
      and jobs = Portable_mpmc_queue.create ()
      and ready = Await.Await.Semaphore.create 0
      and limit_bytes = opts.batch_bytes
      and shipped = Portable.Atomic.make 0
      and starved = Portable.Atomic.make 0
      and running = Portable.Atomic.make start
      and target = Portable.Atomic.make start
      and peak = Portable.Atomic.make start
      and wake = Await.Await.Semaphore.create 0
      and ending = Portable.Atomic.make Going in
      let (nothing_catalogued @ portable) () =
        Await.Await.Ivar.fill_if_empty catalogued
          {
            entries = Iarray.of_list [];
            found = Portable.Atomic_array.create ~len:0 None;
          }
      in
      let () =
        match objects with
        | With_objects when Iarray.length databases > 0 -> ()
        | With_objects | Without_objects -> nothing_catalogued ()
      in
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
          let () =
            match Await.Await.Semaphore.try_acquire ready with
            | Acquired -> ()
            | Would_block ->
                let started = Stopwatch.start () in
                Await.Await.Semaphore.acquire Blocking.await ready;
                ignore
                  (Portable.Atomic.fetch_and_add starved
                     (Core.Time_ns.Span.to_int_ns (Stopwatch.elapsed started)))
          in
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
          let state =
            match result with
            | Ok () -> Announced
            | Error error ->
                fail error;
                Withheld
          in
          Await.Await.Ivar.fill_if_empty announcement state
        in
        let announced announcement =
          Await.Await.Ivar.read Blocking.await announcement
        in
        let ddl target =
          Guarded.hold session (fun session ->
              (P.ddl session target : (string, error) result))
        in
        let rec claimed () =
          let now = Portable.Atomic.get hungry in
          if now <= 0 then Unclaimed
          else
            match
              Portable.Atomic.compare_and_set hungry ~if_phys_equal_to:now
                ~replace_with:(now - 1)
            with
            | Set_here -> Claimed
            | Compare_failed -> claimed ()
        in
        let offered position remaining width =
          let rec claims count =
            if count >= sessions_count then count
            else match claimed () with Claimed -> claims (count + 1) | Unclaimed -> count
          and unclaim count =
            ignore (Portable.Atomic.fetch_and_add hungry count)
          in
          let publish at piece =
            let () =
              match S.add_part sink (Chunk.table piece) with
              | Ok () -> ()
              | Error error -> fail error
            in
            let table = Portable.Atomic_array.get chunk_tables position in
            Portable.Atomic_array.set chunk_tables at table;
            Portable.Atomic_array.set indexes at
              (Portable.Atomic.fetch_and_add (Iarray.get table_parts table) 1);
            Portable.Atomic_array.set divided at (Some piece);
            Portable.Atomic_array.set widths at width;
            Await.Await.Mpmc_queue.push spill at;
            Await.Await.Semaphore.release offers
          in
          match Chunk.bounds remaining with
          | Some (lo, hi)
            when Int64.unsigned_compare (Int64.sub hi lo) (Int64.mul 2L width) >= 0
            -> (
              let middle = Int64.add lo width in
              match (claims 0, Chunk.divide remaining middle) with
              | 0, _ -> remaining
              | wanted, None ->
                  unclaim wanted;
                  remaining
              | wanted, Some (kept, given) ->
                  let steps = Int64.unsigned_div (Int64.sub hi middle) width in
                  let count =
                    if Int64.unsigned_compare steps (Int64.of_int wanted) < 0 then
                      Int64.to_int steps
                    else wanted
                  in
                  let at = Portable.Atomic.fetch_and_add used count in
                  if at + count > capacity then (
                    unclaim wanted;
                    remaining)
                  else
                    let rec deal given dealt =
                      let cut =
                        if dealt = count - 1 then None
                        else
                          Option.bind (Chunk.bounds given) (fun (from, _) ->
                              Chunk.divide given (Int64.add from width))
                      in
                      match cut with
                      | None ->
                          publish (at + dealt) given;
                          dealt + 1
                      | Some (piece, rest) ->
                          publish (at + dealt) piece;
                          deal rest (dealt + 1)
                    in
                    unclaim (wanted - deal given 0);
                    Portable.Atomic_array.set divided position
                      (Option.bind (shared position) (fun whole ->
                           Option.map fst (Chunk.divide whole middle)));
                    kept)
          | Some _ | None -> remaining
        in
        let read_from position chunk =
          match
            Part.create sink (Chunk.table chunk)
              ~index:(Portable.Atomic_array.get indexes position)
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
                            ignore
                              (Portable.Atomic.fetch_and_add shipped
                                 (Batch.sealed_byte_size (borrow_ batch)));
                            let ticket = !tickets in
                            incr tickets;
                            Portable_mpmc_queue.push jobs
                              (Render { batch; part; ticket; pool });
                            Await.Await.Semaphore.release ready;
                            Ok ()
                        end)
                        ltypes
                    in
                    let step_seconds = 0.5 in
                    let rec stepped remaining width =
                      match
                        Option.bind (Chunk.bounds remaining) (fun (lo, _) ->
                            Chunk.divide remaining (Int64.add lo width))
                      with
                      | None -> P.read_chunk session remaining writer
                      | Some (front, rest) ->
                          let step = Stopwatch.start () in
                          let%bind () = P.read_chunk session front writer in
                          let took =
                            Float.max 1e-6
                              (Core.Time_ns.Span.to_sec (Stopwatch.elapsed step))
                          in
                          let width =
                            Int64.of_float
                              (Float.min 4.6e18
                                 (Float.max 1.
                                    (Int64.to_float width
                                    *. Float.min 4.
                                         (Float.max 0.5 (step_seconds /. took)))))
                          in
                          stepped (offered position rest width) width
                    in
                    let%bind bounded =
                      match Chunk.bounds chunk with
                      | Some _ -> Ok chunk
                      | None ->
                          let%map span = P.key_span session chunk in
                          Option.fold ~none:chunk
                            ~some:(fun closed ->
                              Portable.Atomic_array.set divided position (Some closed);
                              closed)
                            (Option.bind span (fun (lo, hi) -> Chunk.closed chunk lo hi))
                    in
                    let%map () =
                      match Chunk.bounds bounded with
                      | None -> P.read_chunk session bounded writer
                      | Some (lo, hi) ->
                          let given = Portable.Atomic_array.get widths position in
                          stepped bounded
                            (if Int64.compare given 0L > 0 then given
                             else
                               Int64.max 1L
                                 (Int64.shift_right_logical (Int64.sub hi lo) 6))
                    in
                    !tickets)
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
        let read position =
          Portable.Atomic.incr busy;
          Option.iter (read_from position) (shared position);
          if
            Portable.Atomic.fetch_and_add busy (-1) = 1
            && Portable.Atomic.get next >= Iarray.length items
          then
            Base.Fn.apply_n_times ~n:sessions_count
              (fun () -> Await.Await.Semaphore.release offers)
              ()
        in
        let rec hunt hunger =
          let idle = Portable.Atomic.get busy = 0 in
          match
            (Portable.Atomic.get failure, Await.Await.Mpmc_queue.pop_nonblocking spill)
          with
          | Some _, _ -> ()
          | None, This position ->
              read position;
              hunt Unregistered
          | None, Null when idle -> (
              match hunger with
              | Registered -> ignore (claimed ())
              | Unregistered -> ())
          | None, Null ->
              let () =
                match hunger with
                | Unregistered -> Portable.Atomic.incr hungry
                | Registered -> ()
              in
              Await.Await.Semaphore.acquire Blocking.await offers;
              hunt Registered
        in
        let objects database definitions =
          match S.objects sink (Iarray.get databases database) definitions with
          | Ok () -> ()
          | Error error -> fail error
        in
        let catalog database =
          let name = Iarray.get databases database in
          let () =
            match announced (Iarray.get database_announcements database) with
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
                    Portable.Atomic_array.set catalogs database entries)
          in
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
                     (Portable.Atomic_array.get chunk_tables position))
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
        let ended () =
          match Portable.Atomic.exchange ending Ended with
          | Ended -> ()
          | Going ->
              Base.Fn.apply_n_times ~n:sessions_count
                (fun () -> Await.Await.Semaphore.release wake)
                ()
        in
        let parked () =
          Await.Await.Semaphore.acquire Blocking.await wake;
          Portable.Atomic.get ending
        in
        let rec gate () =
          let now = Portable.Atomic.get running in
          if now <= Portable.Atomic.get target then Stay
          else
            match
              Portable.Atomic.compare_and_set running ~if_phys_equal_to:now
                ~replace_with:(now - 1)
            with
            | Set_here -> Park
            | Compare_failed -> gate ()
        in
        let rec serve () =
          match gate () with
          | Park -> resume ()
          | Stay -> (
              let turn = Portable.Atomic.fetch_and_add next 1 in
              match Portable.Atomic.get failure with
              | Some _ -> ended ()
              | None when turn >= Iarray.length items ->
                  ended ();
                  define (Await.Await.Ivar.read Blocking.await catalogued);
                  hunt Unregistered
              | None ->
                  perform (Iarray.get items turn);
                  serve ())
        and resume () =
          match parked () with Going -> serve () | Ended -> ()
        in
        let () = if index < start then serve () else resume () in
        close session
      in
      let renderers =
        List.init rendering (fun _ -> Concurrent.start render)
      in
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
      let (tune @ portable) () =
        let interval = 1. and slices = 20 in
        let rec rest slice =
          match Portable.Atomic.get ending with
          | Ended -> Ended
          | Going when slice = 0 -> Going
          | Going ->
              Unix.sleepf (interval /. Float.of_int slices);
              rest (slice - 1)
        in
        let rec step width =
          match rest slices with
          | Ended -> ()
          | Going ->
              let width, decision =
                Width.step width
                  {
                    delivered = Portable.Atomic.exchange shipped 0;
                    waiting =
                      Float.min 1.
                        (Float.of_int (Portable.Atomic.exchange starved 0)
                        /. (interval *. 1e9 *. Float.of_int rendering));
                  }
              in
              let () =
                match decision with
                | Keep -> ()
                | Open readers ->
                    ignore (Portable.Atomic.fetch_and_add running readers);
                    Portable.Atomic.set peak
                      (Int.max (Portable.Atomic.get peak)
                         (Portable.Atomic.fetch_and_add target readers + readers));
                    Base.Fn.apply_n_times ~n:readers
                      (fun () -> Await.Await.Semaphore.release wake)
                      ()
                | Close readers ->
                    ignore (Portable.Atomic.fetch_and_add target (-readers))
              in
              step width
        in
        step (Width.start ~width:start ~limit:sessions_count)
      in
      let tuning =
        if start < sessions_count then Result.to_option (Concurrent.start tune)
        else None
      in
      List.iter (Option.iter Concurrent.join) readers;
      Portable.Atomic.set ending Ended;
      Option.iter Concurrent.join tuning;
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
          let entries =
            let planned, divided =
              List.partition
                (fun position -> position < Array.length chunks)
                (List.init
                   (Int.min capacity (Portable.Atomic.get used))
                   Fun.id)
            in
            List.filter_map
              (fun position ->
                Option.map
                  (fun chunk ->
                    (chunk, Portable.Atomic_array.get states position))
                  (shared position))
              (planned
              @ List.sort
                  (fun a b ->
                    Int.compare
                      (Portable.Atomic_array.get indexes a)
                      (Portable.Atomic_array.get indexes b))
                  divided)
          in
          let manifest =
            Manifest.make ~level:(Any level) ~format:(S.format sink)
              ~position:(P.yojson_of_position position)
              ~threads:
                {
                  sessions = sessions_count;
                  readers = start;
                  peak = Portable.Atomic.get peak;
                  renderers = rendering;
                }
              ~schema entries
          in
          let rows = Base.Hashtbl.create (module Schema.Name) in
          List.iter
            (fun ((chunk : Chunk.t), (state : Manifest.state)) ->
              match state with
              | Done { part; _ } ->
                  Base.Hashtbl.incr rows ~by:part.rows (Chunk.table chunk).name
              | Pending -> ())
            entries;
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
    dump ()
end
