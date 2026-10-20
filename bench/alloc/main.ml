(* Samples allocations of the dump hot path on one domain with Gc.Memprof:
   read_chunk into a writer whose consumer renders each batch with the sql
   sink into [argv.(1)] (or only releases it without an argument), and prints
   the heaviest call sites. *)
let () =
  let conn =
    {
      Quiver_mysql.Provider.route =
        Tcp
          {
            host = "127.0.0.1";
            port = 33200;
            tls = Quiver_mysql.Link.Verified_tls System_roots;
          };
      user = "root";
      password = Some "quiver";
      timeouts =
        {
          connecting = Core.Time_ns.Span.of_int_sec 10;
          silence = Core.Time_ns.Span.of_int_sec 600;
        };
      zero_dates = Reject;
    }
  in
  let fail pp error =
    Format.eprintf "%a@." pp error;
    exit 1
  in
  let connections =
    match Quiver_mysql.Provider.connect conn (Exactly 1) with
    | Ok connections -> connections
    | Error _ -> fail Format.pp_print_string "connect failed"
  in
  let schema =
    match
      Quiver_mysql.Provider.introspect connections
        (Only
           (Database (Result.get_ok (Quiver_core.Ident.of_string "bench")), []))
    with
    | Ok schema -> schema
    | Error _ -> fail Format.pp_print_string "introspect failed"
  in
  let chunks =
    match
      Quiver_mysql.Provider.plan connections schema
        (Result.get_ok (Quiver_core.Plan_opts.make ~chunk_bytes:(64 lsl 20)))
    with
    | Ok chunks -> chunks
    | Error _ -> fail Format.pp_print_string "plan failed"
  in
  let session =
    match
      Quiver_mysql.Provider.snapshot
        (Result.get_ok (Quiver_mysql.Provider.supports Ftwrl Snapshot))
        connections
    with
    | Ok (sessions, _) ->
        Await.Capsule.Await.With_mutex.destroy Quiver_core.Blocking.await
          sessions.(0)
    | Error _ -> fail Format.pp_print_string "snapshot failed"
  in
  let sites = Base.Hashtbl.create (module Base.String) in
  let words = ref 0 in
  let tracker =
    {
      Gc.Memprof.null_tracker with
      alloc_minor =
        (fun info ->
          words := !words + info.n_samples;
          let frames =
            Printexc.backtrace_slots info.callstack
            |> Option.value ~default:[||] |> Array.to_list
            |> List.filter_map Printexc.Slot.name
            |> List.filter (fun name ->
                not
                  (Base.String.is_prefix name ~prefix:"Stdlib"
                  || Base.String.is_prefix name ~prefix:"Base"))
          in
          let key = String.concat " < " (Base.List.take frames 3) in
          Base.Hashtbl.incr sites key ~by:info.n_samples;
          None);
    }
  in
  let rows = ref 0 in
  let pool = Quiver_core.Batch_pool.create ~batches:2 in
  let sink =
    if Array.length Sys.argv > 1 then
      match
        Quiver_sink.Layout.create ~dir:(File_path.of_string Sys.argv.(1)) Plain
      with
      | Ok layout ->
          Some (Quiver_sink.Sql.create layout ~statement_bytes:(1 lsl 20))
      | Error _ -> fail Format.pp_print_string "layout failed"
    else None
  in
  let _profile =
    Gc.Memprof.start ~sampling_rate:1e-3 ~callstack_size:12 tracker
  in
  List.iteri
    (fun index chunk ->
      let part =
        Option.map
          (fun sink ->
            match
              Quiver_sink.Sql.part sink (Quiver_core.Chunk.table chunk) ~index
            with
            | Ok part -> part
            | Error _ -> fail Format.pp_print_string "part failed")
          sink
      in
      let first, rest = (Quiver_core.Chunk.table chunk).fields in
      let writer =
        Quiver_core.Batch_writer.create ~limit_bytes:(4 lsl 20) pool
          (module struct
            type error =
              [ Quiver_core.Error.t | `Provider of Quiver_mysql.Provider.error ]

            let ship (batch : Quiver_core.Batch.sealed @ unique) pool =
              rows := !rows + Quiver_core.Batch.sealed_rows (borrow_ batch);
              (match part with
              | None -> ()
              | Some part -> (
                  match
                    Quiver_sink.Sql.render (Quiver_sink.Sql.renderer part)
                      (borrow_ batch)
                  with
                  | Ok rendered -> ignore (Quiver_sink.Sql.append part rendered)
                  | Error _ -> fail Format.pp_print_string "render failed"));
              Ok
                (Quiver_core.Batch_pool.give pool
                   (Quiver_core.Batch.unseal batch))
          end)
          (List.map
             (fun { Quiver_core.Schema.Field.ltype; _ } -> ltype)
             (first :: rest))
      in
      match Quiver_mysql.Provider.read_chunk session chunk writer with
      | Ok () ->
          Option.iter (fun part -> Quiver_sink.Sql.close part ignore) part
      | Error _ -> fail Format.pp_print_string "read failed")
    chunks;
  Gc.Memprof.stop ();
  Format.printf "%d rows, %.1f sampled words per row (x1000 sampling)@." !rows
    (float_of_int !words *. 1000. /. float_of_int !rows);
  Base.Hashtbl.to_alist sites
  |> List.sort (fun (_, a) (_, b) -> Int.compare b a)
  |> fun sites ->
  Base.List.take sites 15
  |> List.iter (fun (site, w) ->
      Format.printf "%6.1f w/row  %s@."
        (float_of_int w *. 1000. /. float_of_int !rows)
        site)
