open Base.Result.Let_syntax

type output = {
  dir : File_path.t;
  format : Quiver_core.Output_format.t;
  compression : [ `Plain | `Zstd of int ];
  statement_bytes : int;
}
(** Where and how data files are written. *)

type reading = {
  conn : Quiver_mysql.Provider.conn_config;
  mode : Quiver_mysql.Provider.snapshot_mode;
  filter : Quiver_core.Filter.t;
  rows : Quiver_core.Filter.rows;
  objects : Quiver_core.Filter.objects;
  masking : Quiver_core.Mask.t;
  scripted_after : Quiver_load.After_load.rule list;  (** The [(after-load …)] forms of mask scripts. *)
  opts : Quiver_core.Dump_opts.t;
}
(** What a dump reads and how. *)

type 'r consumer = {
  consume :
    ('s : value mod contended portable) ('e : value mod contended portable).
    (module Quiver_core.Sink_intf.S with type t = 's and type error = 'e) -> 's -> 'r;
}
(** What runs with a sink of any kind. *)

let () =
  let options = Hashtbl.create 64 in
  let described ?docv ?doc ?env names =
    let long = List.fold_left (fun best name -> if String.length name > String.length best then name else best) "" names in
    let variable =
      Option.value env ~default:("QUIVER_" ^ String.uppercase_ascii (String.map (function '-' -> '_' | c -> c) long))
    in
    Hashtbl.replace options long (names, variable);
    Cmdliner.Arg.info ?docv ?doc ~env:(Cmdliner.Cmd.Env.info variable) names
  in
  let told message =
    Format.eprintf "quiver: %s@." message;
    1
  and failed sexp =
    Format.eprintf "quiver: %a@." Sexplib0.Sexp.pp_hum sexp;
    1
  and hinted = function
    | None -> ()
    | Some collation ->
        Format.eprintf
          "hint: the target has no collation %s; --collation %s=OTHER creates what uses it in \
           another collation of its character set, such as utf8mb4_0900_ai_ci on MySQL and TiDB@."
          collation collation
  and ident_of_string text =
    Result.map_error
      (fun (`Ident error) ->
        `Msg (Sexplib0.Sexp.to_string_hum (Quiver_core.Ident.sexp_of_error error)))
      (Quiver_core.Ident.of_string text)
  in
  let ident =
    Cmdliner.Arg.conv
      (ident_of_string, fun ppf name -> Format.pp_print_string ppf (name :> string))
  in
  let choices to_string all =
    Cmdliner.Arg.enum (List.map (fun value -> (to_string value, value)) all)
  in
  let target into =
    Option.fold ~none:Quiver_load.Load.Same_names
      ~some:(fun name -> Quiver_load.Load.Into name)
      into
  in
  let file_path text =
    match File_path.of_string text with
    | path -> Ok path
    | exception failure -> Error (`Msg (Printexc.to_string failure))
  in
  let read (type (sink : value mod contended portable) (failure : value mod contended portable))
      (module Sink : Quiver_core.Sink_intf.S with type t = sink and type error = failure) (sink : sink)
      { conn; mode; filter; rows; objects; masking; opts; _ } ~report ~sink:written ~(missing_collation : failure -> string option) =
    let module Dump = Quiver_core.Dump.Make (Quiver_mysql.Provider) (Sink) in
    let result =
      match mode with
      | Auto | Ftwrl | Backup_stage ->
          Dump.run
            { conn; mode; level = Snapshot; filter; rows; objects; masking; opts }
            sink
      | Trx_only ->
          Dump.run
            { conn; mode; level = Best_effort; filter; rows; objects; masking; opts }
            sink
    in
    match result with
    | Ok (manifest : Quiver_core.Manifest.t) ->
        report ~entries:manifest.chunks
          ~rows:
            (Base.List.sum
               (module Base.Int)
               manifest.chunks
               ~f:(fun ({ state; _ } : Quiver_core.Manifest.entry) ->
                 match state with
                 | Done { part = { rows; _ }; _ } -> rows
                 | Pending -> 0))
          ~chunks:(List.length manifest.chunks);
        let spent =
          Quiver_core.Spent.of_states
            (List.map
               (fun ({ state; _ } : Quiver_core.Manifest.entry) -> state)
               manifest.chunks)
        in
        let holding =
          match Quiver_core.Spent.holding spent with
          | Source -> "the source"
          | Rendering -> "rendering"
          | Writing -> written
        in
        Format.printf
          "time over all parts: reading %a, readers held back %a, rendering %a, \
           writing to %s %a, committing %a; %s held the run back@."
          Core.Time_ns.Span.pp spent.reading Core.Time_ns.Span.pp spent.waiting
          Core.Time_ns.Span.pp spent.rendering written Core.Time_ns.Span.pp
          spent.writing Core.Time_ns.Span.pp spent.committing holding;
        List.iter
          (fun (name, ({ parts; spent; began; ended } : Quiver_core.Spent.table)) ->
            let noun = match parts with 1 -> "part" | _ -> "parts" in
            Format.printf
              "read last: %a, %d %s from %a to %a since the start, reading %a, held back %a@."
              Quiver_core.Schema.Name.pp name parts noun Core.Time_ns.Span.pp began
              Core.Time_ns.Span.pp ended Core.Time_ns.Span.pp spent.reading Core.Time_ns.Span.pp
              spent.waiting)
          (Base.List.take (Quiver_core.Spent.of_tables manifest.chunks) 5);
        List.iter
          (fun (masked : Quiver_core.Manifest.masked list) ->
            Option.iter
              (fun ({ table; _ } : Quiver_core.Manifest.masked) ->
                Format.printf "masked in %a: %s@." Quiver_core.Schema.Name.pp table
                  (String.concat ", "
                     (List.map
                        (fun ({ column; action; _ } : Quiver_core.Manifest.masked) ->
                          Printf.sprintf "%s (%s)" (column :> string) action)
                        masked)))
              (Base.List.hd masked))
          (Base.List.group manifest.masked ~break:(fun a b ->
               not (Quiver_core.Schema.Name.equal a.table b.table)));
        let { Quiver_core.Manifest.connected; planned; snapshot; first_part; read; committed } =
          manifest.phases
        in
        Format.printf
          "since the start: sessions open %a, planned %a, in the snapshot %a, %s, reading ended \
           %a, every part taken %a@."
          Core.Time_ns.Span.pp connected Core.Time_ns.Span.pp planned Core.Time_ns.Span.pp snapshot
          (Option.fold ~none:"no rows"
             ~some:(Format.asprintf "first part %a" Core.Time_ns.Span.pp)
             first_part)
          Core.Time_ns.Span.pp read Core.Time_ns.Span.pp committed;
        Format.printf
          "%d readers at the start, %d at most, %d sessions, %d renderers, %d \
           cores to use@."
          manifest.threads.readers manifest.threads.peak manifest.threads.sessions
          manifest.threads.renderers opts.cores;
        Result.iter
          (function
            | Quiver_mysql.Provider.Locked { lock; lock_held; _ } ->
                Format.printf "%a held for %a@." Sexplib0.Sexp.pp_hum
                  (Quiver_mysql.Provider.sexp_of_lock lock)
                  Core.Time_ns.Span.pp lock_held
            | Unlocked ->
                Format.printf "no lock: each session has its own snapshot@."
            | Pinned { tso } ->
                Format.printf "no lock: every session read at TSO %Ld@." tso)
          (Quiver_core.Codec.decode Quiver_mysql.Provider.position_of_yojson
             manifest.position);
        0
    | Error (`Mask (Quiver_core.Mask.Scripted { table; column; error })) ->
        told
          (Format.asprintf "masking %a.%s: %s" Quiver_core.Schema.Name.pp table (column :> string)
             (Quiver_core.Mask_script.describe error))
    | Error (`Mask (Quiver_core.Mask.Unhandled { table; columns })) ->
        told
          (Format.asprintf
             "masking %a: no rule decides on %s; mask them, or keep them with (keep \"%a.COLUMN\") or \
              --mask %a.COLUMN=keep"
             Quiver_core.Schema.Name.pp table
             (String.concat ", " (List.map (fun (column : Quiver_core.Ident.t) -> (column :> string)) columns))
             Quiver_core.Schema.Name.pp table Quiver_core.Schema.Name.pp table)
    | Error (`Mask (Quiver_core.Mask.Not_injective { table; column; action })) ->
        told
          (Format.asprintf
             "masking %a.%s: %s can give two values one, and the column is in a primary or unique \
              key; use permute"
             Quiver_core.Schema.Name.pp table (column :> string) action)
    | Error (`Mask (Quiver_core.Mask.Split_reference { table; column; other; other_column })) ->
        told
          (Format.asprintf
             "masking %a.%s and %a.%s differently, though foreign keys tie them: references would \
              point nowhere; permute one of them and leave the others to follow, or give all the \
              same permute"
             Quiver_core.Schema.Name.pp table (column :> string) Quiver_core.Schema.Name.pp other
             (other_column :> string))
    | Error (`Mask (Quiver_core.Mask.Unfit { table; column; action; ltype })) ->
        let hint =
          if String.starts_with ~prefix:"permute" action then " (a permutation's domain must lie within the column's type)"
          else ""
        in
        told
          (Format.asprintf "masking %a.%s: %s gives no value of its type %a%s" Quiver_core.Schema.Name.pp table
             (column :> string) action Sexplib0.Sexp.pp (Quiver_core.Ltype.sexp_of_t ltype) hint)
    | Error (`Mask (Quiver_core.Mask.Small_domain { table; column; low; high })) ->
        told
          (Format.asprintf
             "masking %a.%s: a permutation from %Ld to %Ld has fewer than a million values, too few \
              to hide one"
             Quiver_core.Schema.Name.pp table (column :> string) low high)
    | Error (`Mask (Quiver_core.Mask.Outside_domain { table; column; low; high })) ->
        told
          (Format.asprintf
             "masking %a.%s: a value lies outside the permutation's %Ld..%Ld; give the rule \
              permute:LOW..HIGH holding every value"
             Quiver_core.Schema.Name.pp table (column :> string) low high)
    | Error error ->
        let code = failed (Dump.sexp_of_error error) in
        let () = match error with `Sink error -> hinted (missing_collation error) | _ -> () in
        code
  in
  let files output { consume } =
    let setup =
      let%bind compression =
        match output.compression with
        | `Plain -> Ok Quiver_sink.Compression.Plain
        | `Zstd level ->
            let%map level = Quiver_sink.Compression.Level.of_int level in
            Quiver_sink.Compression.Zstd level
      in
      Quiver_sink.Layout.create ~dir:output.dir compression
    in
    match setup with
    | Error error ->
        failed
          ([%sexp_of:
             [ `Compression of Quiver_sink.Compression.error
             | `Output of Quiver_sink.Output.error ]]
             error)
    | Ok layout -> (
        match output.format with
        | Sql ->
            consume
              (module Quiver_sink.Sql)
              (Quiver_sink.Sql.create layout
                 ~statement_bytes:output.statement_bytes)
        | Csv dialect ->
            consume
              (module Quiver_sink.Csv)
              (Quiver_sink.Csv.create layout dialect))
  in
  let dump reading output =
    (
        files output
          {
            consume =
              (fun sink_module sink ->
                read sink_module sink reading ~sink:"the files" ~missing_collation:(fun _ -> None)
                  ~report:(fun ~entries:_ ~rows ~chunks ->
                    Format.printf "dumped %d rows in %d chunks into %a@." rows
                      chunks File_path.pp output.dir));
          })
  in
  let resolve text =
    match Base.String.rsplit2 text ~on:':' with
    | None -> Error (`Msg ("expected HOST:PORT, got " ^ text))
    | Some (host, port) -> (
        match Unix.getaddrinfo host port [ AI_SOCKTYPE SOCK_STREAM ] with
        | { ai_addr = address; _ } :: _ -> Ok { Quiver_net.Channel.host; address }
        | [] -> Error (`Msg ("no address for " ^ text))
        | exception Unix.Unix_error (error, _, _) ->
            Error (`Msg (Unix.error_message error)))
  in
  let listening text =
    let%bind { Quiver_net.Channel.address; _ } = resolve text in
    let%map listener =
      Result.map_error
        (fun (`Channel error) ->
          `Msg
            (Sexplib0.Sexp.to_string_hum (Quiver_net.Channel.sexp_of_error error)))
        (Quiver_net.Channel.listen address)
    in
    Format.printf "listening on %s@." text;
    listener
  in
  let tablespaces sink (entries : Quiver_core.Manifest.entry list) =
    let parts name =
      List.filter_map
        (fun ({ table; state; _ } : Quiver_core.Manifest.entry) ->
          if Quiver_core.Schema.Name.equal table name then Some state else None)
        entries
    in
    let sizes states =
      List.fold_left
        (fun (rows, bytes) (state : Quiver_core.Manifest.state) ->
          match state with
          | Done { part; _ } -> (rows + part.rows, bytes + part.bytes)
          | Pending -> (rows, bytes))
        (0, 0) states
    in
    let amount (rows, bytes) =
      match entries with
      | [] -> ""
      | _ :: _ -> Printf.sprintf ": %d rows, %.1f MB sent" rows (Float.of_int bytes /. 1e6)
    in
    let reason why =
      match Quiver_load.Physical.sexp_of_unsuitable why with
      | List (Atom outer :: (Atom inner | List (Atom inner :: _)) :: _) -> outer ^ " " ^ inner
      | List (Atom outer :: _) | Atom outer -> outer
      | List _ -> "other"
    in
    let ways =
      List.map (fun (name, way) -> (name, way, sizes (parts name))) (Quiver_load.Copy.ways sink)
      |> List.stable_sort (fun (_, _, (a, _)) (_, _, (b, _)) -> Int.compare b a)
    in
    List.iter
      (fun (name, (way : Quiver_load.Copy.way), size) ->
        match way with
        | Imported { preparing; queued; checking; importing; keys } ->
            Format.printf
              "imported %a as a built tablespace%s; prepared %a, waited %a for a session, check \
               %a, import %a, secondary keys %a@."
              Quiver_core.Schema.Name.pp name (amount size) Core.Time_ns.Span.pp preparing
              Core.Time_ns.Span.pp queued Core.Time_ns.Span.pp checking Core.Time_ns.Span.pp
              importing Core.Time_ns.Span.pp keys
        | Loaded why ->
            Format.printf "loaded %a with LOAD DATA%s: %s@." Quiver_core.Schema.Name.pp name (amount size)
              (Sexplib0.Sexp.to_string (Quiver_load.Physical.sexp_of_unsuitable why)))
      ways;
    let count group = match group with [ _ ] -> "1 table" | _ -> Printf.sprintf "%d tables" (List.length group) in
    let total group = List.fold_left (fun (r, b) (_, _, (r', b')) -> (r + r', b + b')) (0, 0) group in
    let spent group =
      let { Quiver_core.Spent.rendering; writing; committing; _ } =
        Quiver_core.Spent.of_states (List.concat_map (fun (name, _, _) -> parts name) group)
      in
      Format.asprintf "rendering %a, writing %a, committing %a" Core.Time_ns.Span.pp rendering
        Core.Time_ns.Span.pp writing Core.Time_ns.Span.pp committing
    in
    let built, loaded =
      List.partition_map
        (fun ((_, (way : Quiver_load.Copy.way), _) as table) ->
          match way with Imported imported -> Left (table, imported) | Loaded why -> Right (table, why))
        ways
    in
    let () =
      match built with
      | [] -> ()
      | _ :: _ ->
          let sum f =
            List.fold_left (fun acc (_, imported) -> Core.Time_ns.Span.( + ) acc (f imported)) Core.Time_ns.Span.zero built
          in
          let tables = List.map fst built in
          Format.printf
            "built tablespaces: %s%s; parts: %s; over all tables: prepared %a; after the last \
             part: waited %a for a session, check %a, import %a, secondary keys %a@."
            (count built) (amount (total tables)) (spent tables) Core.Time_ns.Span.pp
            (sum (fun { preparing; _ } -> preparing))
            Core.Time_ns.Span.pp
            (sum (fun { queued; _ } -> queued))
            Core.Time_ns.Span.pp
            (sum (fun { checking; _ } -> checking))
            Core.Time_ns.Span.pp
            (sum (fun { importing; _ } -> importing))
            Core.Time_ns.Span.pp
            (sum (fun { keys; _ } -> keys));
          let after ({ queued; checking; importing; keys; _ } : Quiver_load.Copy.imported) =
            Core.Time_ns.Span.(queued + checking + importing + keys)
          in
          List.iter
            (fun ((name, _, _), imported) ->
              Format.printf "longest after the last part: %a, %a@." Quiver_core.Schema.Name.pp name
                Core.Time_ns.Span.pp (after imported))
            (Base.List.take
               (List.stable_sort
                  (fun (_, a) (_, b) -> Core.Time_ns.Span.compare (after b) (after a))
                  built)
               5)
    in
    let () =
      match Quiver_load.Copy.finishing sink with
      | Unfinished -> ()
      | Finished { built; after_load; foreign_keys; objects } ->
          Format.printf
            "after the parts: waited %a for imports and keys, after-load statements %a, foreign \
             keys %a, objects %a@."
            Core.Time_ns.Span.pp built Core.Time_ns.Span.pp after_load Core.Time_ns.Span.pp
            foreign_keys Core.Time_ns.Span.pp objects
    in
    match loaded with
    | [] -> ()
    | _ :: _ ->
        let tables = List.map fst loaded in
        Format.printf "LOAD DATA instead: %s%s; parts: %s@." (count loaded) (amount (total tables))
          (spent tables);
        let reasons = Base.List.dedup_and_sort ~compare:String.compare (List.map (fun (_, why) -> reason why) loaded) in
        List.iter
          (fun tag ->
            let group = List.filter_map (fun (table, why) -> if reason why = tag then Some table else None) loaded in
            Format.printf "  %s: %s%s@." tag (count group) (amount (total group)))
          reasons
  in
  let advise advice =
    let evidence (evidence : Quiver_core.Advice.evidence) =
      match evidence with
      | Share share ->
          Printf.sprintf "%.0f%% of the time they had data to send" (100. *. share)
      | Near_ceiling reach ->
          Printf.sprintf "at %.0f%% of what it allows in a round trip" (100. *. reach)
    in
    List.iter
      (fun (advice : Quiver_core.Advice.t) ->
        let text =
          match advice with
          | Receive_buffer held ->
              "connections waited for the receiver's window "
              ^ evidence held
              ^ "; on the receiving host raise the kernel's receive buffer \
                 limit: Linux sysctl -w net.core.rmem_max=67108864 \
                 net.ipv4.tcp_rmem='4096 131072 67108864', macOS sysctl -w \
                 net.inet.tcp.autorcvbufmax=67108864"
          | Send_buffer held ->
              "connections were held by their send buffer " ^ evidence held
              ^ "; on this host raise the kernel's send buffer limit: Linux \
                 sysctl -w net.core.wmem_max=67108864 \
                 net.ipv4.tcp_wmem='4096 16384 67108864', macOS sysctl -w \
                 net.inet.tcp.autosndbufmax=67108864"
          | Lossy_path resent ->
              Printf.sprintf
                "%.2f%% of bytes were sent again; where \
                 net.ipv4.tcp_available_congestion_control lists bbr, sysctl \
                 -w net.ipv4.tcp_congestion_control=bbr \
                 net.core.default_qdisc=fq keeps the pace on losses"
                (100. *. resent)
          | Durable_commits ->
              "the target syncs its redo log at every commit; for the load, \
               SET GLOBAL innodb_flush_log_at_trx_commit = 2, and 1 again after"
          | Synced_binlog ->
              "the target syncs its binlog at every commit; for the load, SET \
               GLOBAL sync_binlog = 0, and 1 again after"
          | Logged_load ->
              "the target writes the load to its binlog; --binlog skip leaves \
               it out where replicas need not see it"
        in
        Format.printf "advice: %s@." text)
      advice
  in
  let agent reading protection collector level streams most_streams chunks
      rate traffic reconnect stall =
    (
        (
            let started = Quiver_core.Stopwatch.start () in
            match
              let%bind peer = resolve collector in
              let%bind compression =
                match level with
                | `Auto -> Ok Quiver_net.Remote.Adaptive
                | `Level level ->
                    Result.map
                      (fun level ->
                        Quiver_net.Remote.Fixed (Quiver_sink.Compression.Zstd level))
                      (Quiver_sink.Compression.Level.of_int level)
              in
              let streams =
                Option.value streams ~default:(Quiver_core.Dump_opts.most_readers reading.opts)
              in
              let%map sink =
                Quiver_net.Remote.create protection peer
                  {
                    streams;
                    most_streams;
                    chunks =
                      Option.value chunks ~default:(2 * Quiver_core.Dump_opts.most_readers reading.opts);
                    compression;
                    pace =
                      Option.map
                        (fun megabytes -> Float.to_int (megabytes *. 1e6))
                        rate;
                    traffic;
                    reconnect = Core.Time_ns.Span.of_sec reconnect;
                    stall = Core.Time_ns.Span.of_sec stall;
                  }
              in
              (sink, streams)
            with
            | Error (`Msg message) -> told message
            | Error ((`Compression _ | `Sink _) as error) ->
                failed
                  ([%sexp_of:
                     [ `Compression of Quiver_sink.Compression.error
                     | `Sink of Quiver_net.Remote.error ]]
                     error)
            | Ok (sink, streams) ->
                Option.iter
                  (fun (path : Quiver_core.Tcp.path) ->
                    Format.printf "path to %s: round trip %a, send buffer up to %d KiB%s@."
                      collector Core.Time_ns.Span.pp path.rtt
                      (path.send_buffer / 1024)
                      (Option.fold ~none:""
                         ~some:(fun ceiling ->
                           Printf.sprintf
                             ": one connection carries at most %.0f MB/s, %d \
                              streams %.0f MB/s"
                             (ceiling /. 1e6) streams
                             (Float.of_int streams *. ceiling /. 1e6))
                         (Quiver_core.Tcp.ceiling path)))
                  (Quiver_net.Remote.path sink);
                let generated : Quiver_mysql.Provider.generated =
                  match Quiver_net.Remote.columns sink with Written -> Left_out | Stored_too -> Read
                in
                let code =
                  read
                    (module Quiver_net.Remote)
                    sink
                    { reading with conn = { reading.conn with generated } }
                    ~sink:"the network" ~missing_collation:(fun _ -> None)
                    ~report:(fun ~entries:_ ~rows ~chunks ->
                      Format.printf
                        "sent %d rows in %d chunks to %s in %a, %d streams at \
                         the end, waiting for acks %a in all@."
                        rows chunks collector Core.Time_ns.Span.pp
                        (Quiver_core.Stopwatch.elapsed started)
                        (Quiver_net.Remote.streams sink)
                        Core.Time_ns.Span.pp
                        (Quiver_net.Remote.waited_for_acks sink))
                in
                Quiver_net.Remote.shutdown sink;
                advise (Quiver_net.Remote.advice sink);
                code))
  in
  let warned (config : Quiver_mysql.Provider.conn_config) =
    (match config.route with
    | Tcp _ -> ()
    | Socket _ -> (
        match Quiver_mysql.Local.send_space () with
        | Some space when space < 65536 ->
            Format.eprintf
              "quiver: this system lets a Unix socket hold %d bytes, so the \
               server sends at a fraction of TCP's speed; run sudo sysctl \
               net.local.stream.sendspace=262144 before starting it@."
              space
        | Some _ | None -> ()));
    config
  in
  let collect protection reconnect listen forward output to_server into writing ingest =
    match
      match output with
      | Some output -> Ok (`Files output)
      | None -> Result.map (fun config -> `Server (warned config)) to_server
    with
    | Error (`Msg message) -> told message
    | Ok destination ->
        (
            match
              let%bind listening = listening listen in
              let%bind session =
                Quiver_net.Collector.accept listening protection
                  ~reconnect:(Core.Time_ns.Span.of_sec reconnect)
              in
              let%map downstream =
                match forward with
                | None -> Ok None
                | Some next ->
                    let%bind next = resolve next in
                    let%map downstream =
                      Quiver_net.Collector.forward next session
                    in
                    Some downstream
              in
              (listening, session, downstream)
            with
            | Error (`Msg message) -> told message
            | Error (`Collector error) -> failed (Quiver_net.Collector.sexp_of_error error)
            | Ok (listening, session, downstream) -> (
                let started = Quiver_core.Stopwatch.start () in
                let collected columns (type (s : value mod contended portable) (e : value mod contended portable))
                    (module S : Quiver_core.Sink_intf.S with type t = s and type error = e)
                    (sink : s) ~(missing_collation : e -> string option) =
                  let module Collector = Quiver_net.Collector.Make (S) in
                  match
                    Collector.run ?forward:downstream listening session sink ~columns
                  with
                  | Ok () ->
                      Format.printf "collected in %a@." Core.Time_ns.Span.pp
                        (Quiver_core.Stopwatch.elapsed started);
                      0
                  | Error (`Collector error) ->
                      failed (Quiver_net.Collector.sexp_of_error error)
                  | Error (`Sink error) ->
                      let code = failed (S.sexp_of_error error) in
                      hinted (missing_collation error);
                      code
                  | Error (`Mask error) -> failed (Quiver_core.Mask.sexp_of_error error)
                in
                match destination with
                | `Files output ->
                    files output
                      {
                        consume =
                          (fun (type (s : value mod contended portable) (e : value mod contended portable))
                               (module S : Quiver_core.Sink_intf.S with type t = s and type error = e) (sink : s) ->
                            collected Written (module S) sink ~missing_collation:(fun _ -> None));
                      }
                | `Server to_server -> (
                    match
                      Quiver_load.Copy.create to_server (target into) writing ~ingest
                        ~sessions:(Quiver_net.Collector.chunks session)
                    with
                    | Error (`Sink error) -> failed (Quiver_load.Copy.sexp_of_error error)
                    | Ok sink ->
                        let columns : Quiver_net.Message.columns =
                          match ingest with Load_data -> Written | Tablespaces _ -> Stored_too
                        in
                        let code =
                          collected columns (module Quiver_load.Copy) sink ~missing_collation:Quiver_load.Copy.unknown_collation
                        in
                        Quiver_load.Copy.shutdown sink;
                        tablespaces sink [];
                        advise (Quiver_load.Copy.advice sink);
                        code)))
  in
  let copy reading to_server into (writing : Quiver_load.Load.writing) ingest target_threads =
    let generated : Quiver_mysql.Provider.generated =
      match ingest with Quiver_load.Copy.Load_data -> Left_out | Tablespaces _ -> Read
    in
    (
        (
            let started = Quiver_core.Stopwatch.start () in
            match
              Quiver_load.Copy.create to_server (target into)
                { writing with after = writing.after @ reading.scripted_after }
                ~ingest
                ~sessions:
                  (Option.value target_threads ~default:(Quiver_core.Dump_opts.most_readers reading.opts))
            with
            | Error (`Sink error) -> failed (Quiver_load.Copy.sexp_of_error error)
            | Ok sink ->
                let code =
                  read
                    (module Quiver_load.Copy)
                    sink
                    { reading with conn = { reading.conn with generated } }
                    ~sink:"the target" ~missing_collation:Quiver_load.Copy.unknown_collation
                    ~report:(fun ~entries ~rows ~chunks ->
                      Format.printf "copied %d rows in %d chunks in %a@." rows
                        chunks Core.Time_ns.Span.pp
                        (Quiver_core.Stopwatch.elapsed started);
                      tablespaces sink entries)
                in
                Quiver_load.Copy.shutdown sink;
                advise (Quiver_load.Copy.advice sink);
                code))
  in
  let load conn dir into writing objects threads =
    (
        let started = Quiver_core.Stopwatch.start () in
        match
          Quiver_load.Load.run
            conn ~dir:(File_path.of_string dir) (target into) writing ~objects ~threads
        with
        | Ok { files; rows; bytes; skipped; advice } ->
            Format.printf "loaded %d rows (%d bytes) from %d files in %a@." rows
              bytes files Core.Time_ns.Span.pp
              (Quiver_core.Stopwatch.elapsed started);
            (match skipped with
            | 0 -> ()
            | n ->
                Format.printf
                  "resumed: %d files were loaded by an earlier run@." n);
            advise advice;
            0
        | Error error ->
            let () =
              match error with
              | `Load (Quiver_load.Load.Rejected { cause; _ }) -> hinted (Quiver_load.Load.unknown_collation cause)
              | _ -> ()
            in
            failed
              ([%sexp_of:
                 [ `Load of Quiver_load.Load.error
                 | `Manifest of Quiver_core.Manifest.error
                 | `Statements of Quiver_load.Statements.error
                 | `Ident of Quiver_core.Ident.error
                 | Quiver_load.Load.cause ]]
                 error))
  in
  let mirror (conn : Quiver_mysql.Provider.conn_config) security other database into tables workers stores =
    let open Base.Result.Let_syntax in
    let connect (conn : Quiver_mysql.Provider.conn_config) =
      Quiver_mysql.Link.connect ~local_data:Without_local_data conn.route ~timeouts:conn.timeouts ~user:conn.user
        ~password:conn.password
    in
    let tidb link =
      match Quiver_mysql.Link.server link with
      | Mysql_8_0 _ | Mysql_8_4 _ | Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _ -> Error `Not_tidb
      | Tidb_8_5 _ -> Ok link
    in
    let mirrored =
      let%bind link = connect conn in
      let copied =
        let%bind onto =
          Option.fold other ~none:(Ok Quiver_tikv.Mirror.Same_cluster) ~some:(fun (other, security) ->
              Result.map
                (fun link -> Quiver_tikv.Mirror.Other_cluster { Quiver_tikv.Mirror.link; security })
                (connect other))
        in
        let copied =
          let%bind link = tidb link in
          let%bind target = match onto with Same_cluster -> Ok link | Other_cluster onto -> tidb onto.link in
          let%bind tables =
            match tables with
            | _ :: _ -> Ok tables
            | [] ->
                let%bind rows = Quiver_mysql.Link.run_query link (Base_tables database) in
                Base.Result.all
                  (List.map
                     (fun row ->
                       match row with
                       | [| Some table |] -> Quiver_core.Ident.of_string table
                       | row -> Error (`Region (Quiver_tikv.Region.Malformed (Array.to_list row))))
                     rows)
          in
          let%bind () = Quiver_mysql.Link.run_statement target (Create_database into) in
          Base.List.fold_result tables ~init:() ~f:(fun () table ->
              let started = Quiver_core.Stopwatch.start () in
              let%bind copied =
                Quiver_tikv.Mirror.copy { link; security } ~into:onto ~source:{ database; table } ~target:{ database = into; table }
                  {
                    timeout = conn.timeouts.connecting;
                    sizes = { scan = 1024; job = 16 lsl 20; batch = 16 lsl 10 };
                    workers;
                    stores;
                  }
              in
              Format.printf "%s.%s: %d keys, %d bytes, checksum %016Lx as TiDB's, in %a@." (into :> string)
                (table :> string) copied.keys copied.bytes copied.checksum Core.Time_ns.Span.pp
                (Quiver_core.Stopwatch.elapsed started);
              Ok ())
        in
        let () = match onto with Same_cluster -> () | Other_cluster onto -> Quiver_mysql.Link.close onto.link in
        copied
      in
      Quiver_mysql.Link.close link;
      copied
    in
    match mirrored with
    | Ok () -> 0
    | Error `Not_tidb -> told "mirror copies tables of TiDB clusters; a server is not TiDB"
    | Error error ->
        let () =
          match error with
          | `Renumber (Quiver_tikv.Renumber.Missing _ | Old_row) ->
              Format.eprintf
                "hint: the source has rows its copy would read otherwise: written before a column was added \
                 with a default quiver does not write into them (JSON, TIMESTAMP of TiDB before 3.0, a character \
                 set other than utf8mb4, utf8, ascii, latin1 and binary), or in TiDB's first row format; quiver \
                 copy copies the table through SQL@."
          | `Renumber (Quiver_tikv.Renumber.Changing _ | Rearranging) ->
              Format.eprintf "hint: the table's schema or partitions are being changed; copy it once the change is done@."
          | `Renumber (Quiver_tikv.Renumber.Global_index _) ->
              Format.eprintf "hint: a global index names partitions in its values, which mirror does not move; quiver copy copies the table through SQL@."
          | `Transport (Quiver_mysql.Transport.Io _ | Tls _) ->
              Format.eprintf
                "hint: if PD or a TiKV store ended the connection, the cluster may have TLS between its \
                 components: give --cluster-ca, and --cluster-cert with --cluster-key when it asks clients for \
                 a certificate (--to-cluster-* for another target cluster)@."
          | `Pd (Quiver_tikv.Pd.Gc_passed _) ->
              Format.eprintf
                "hint: the source's GC may have removed versions the copy read (its hold on PD lapsed, as when PD was out of reach for minutes); the half-made copy was dropped, run it again@."
          | _ -> ()
        in
        failed
          ([%sexp_of:
             [ `Mirror of Quiver_tikv.Mirror.error
             | `Link of Quiver_mysql.Link.error
             | `Wire of Quiver_mysql.Wire.error
             | `Ident of Quiver_core.Ident.error
             | `Read of Quiver_tikv.Read.error
             | `Info of Quiver_tikv.Info.error
             | `Renumber of Quiver_tikv.Renumber.error
             | `Ingest of Quiver_tikv.Ingest.error
             | `Pd of Quiver_tikv.Pd.error
             | `Region of Quiver_tikv.Region.error
             | `Transport of Quiver_mysql.Transport.error
             | `Proto of Quiver_tikv.Proto.error
             | `Grpc of Quiver_tikv.Grpc.error
             | `Http2 of Quiver_tikv.Http2.error
             | `Hpack of Quiver_tikv.Hpack.error
             | `Not_tidb ]]
             error)
  in
  let conn side =
    let open Cmdliner in
    let names short long =
      match side with
      | `Source -> short @ [ long ]
      | `Target -> [ "to-" ^ long ]
    in
    let tls =
      let mode =
        Arg.(
          value
          & opt
              (some (enum [ ("verified", `Verified); ("disabled", `Disabled) ]))
              None
          & described (names [] "tls") ~docv:"MODE"
              ~doc:
                "$(b,verified) encrypts the connection and verifies the \
                 server: by the system's CAs (on 127.0.0.1 only the \
                 certificate's validity period), by $(b,--tls-ca) or by \
                 $(b,--tls-fingerprint) (the default); $(b,disabled) is for \
                 local test servers.")
      and ca =
        Arg.(
          value
          & opt (some file) None
          & described (names [] "tls-ca") ~docv:"FILE"
              ~doc:
                "PEM file of the CAs the server's chain must lead to; the host \
                 name is checked too, on every host.")
      and fingerprint =
        Arg.(
          value
          & opt (some string) None
          & info
              (names [] "tls-fingerprint")
              ~docv:"SHA256"
              ~doc:
                "Accept only the server certificate with this SHA-256 digest, \
                 as $(b,openssl x509 -fingerprint -sha256) prints it; chain \
                 and host name are not checked.")
      in
      Term.(
        const (fun mode ca fingerprint ->
            match (mode, ca, fingerprint) with
            | None, None, None -> `Unset
            | (Some _ | None), (Some _ | None), (Some _ | None) ->
                `Set
                  (match
                     (Option.value mode ~default:`Verified, ca, fingerprint)
                   with
                   | `Verified, None, None ->
                       Ok
                         (Quiver_mysql.Link.Verified_tls
                            Quiver_mysql.Transport.System_roots)
                   | `Verified, Some ca, None ->
                       Result.map
                         (fun ca ->
                           Quiver_mysql.Link.Verified_tls
                             (Quiver_mysql.Transport.Ca_file ca))
                         (file_path ca)
                   | `Verified, None, Some fingerprint -> (
                       match Quiver_mysql.Fingerprint.of_string fingerprint with
                       | Ok fingerprint ->
                           Ok
                             (Quiver_mysql.Link.Verified_tls
                                (Quiver_mysql.Transport.Pinned fingerprint))
                       | Error (`Fingerprint error) ->
                           Error
                             (`Msg
                                (Sexplib0.Sexp.to_string_hum
                                   (Quiver_mysql.Fingerprint.sexp_of_error error))))
                   | `Verified, Some _, Some _ ->
                       Error
                         (`Msg "--tls-ca and --tls-fingerprint exclude each other")
                   | `Disabled, None, None -> Ok Quiver_mysql.Link.Plaintext
                   | `Disabled, (Some _ | None), (Some _ | None) ->
                       Error
                         (`Msg
                            "--tls disabled takes neither --tls-ca nor \
                             --tls-fingerprint")))
          $ mode $ ca $ fingerprint)
    and host =
      Arg.(
        value
        & opt (some string) None
        & described (names [ "h" ] "host") ~docv:"HOST"
            ~doc:
              "The server's host. As for the mysql client, without it or with \
               $(b,localhost) quiver connects to this machine's Unix socket: \
               $(b,socket) in the [client] groups of my.cnf, \
               $(b,MYSQL_UNIX_PORT), or where packages put it. With \
               $(b,--port) or $(b,--tls) but no host it is 127.0.0.1 over TCP.")
    and port =
      Arg.(
        value
        & opt (some int) None
        & described (names [ "P" ] "port") ~docv:"PORT"
            ~doc:"The server's TCP port; 3306 unless given.")
    and socket =
      Arg.(
        value
        & opt (some string) None
        & described (names [ "S" ] "socket") ~docv:"PATH"
            ~doc:
              "Connect over this Unix socket instead of TCP, as to a server \
               started with $(b,skip_networking); there is no TLS on it, and \
               it takes no $(b,--host), $(b,--port) or $(b,--tls) options.")
    and user =
      Arg.(
        value & opt string "root"
        & described (names [ "u" ] "user") ~docv:"USER" ~doc:"The account to log in as.")
    and password =
      Arg.(
        value
        & opt (some string) None
        & described (names [ "p" ] "password") ~docv:"PASSWORD"
            ~doc:
              "The account's password; none when not given. Prefer the environment variable: \
               a command line is visible to every user of the machine."
            ~env:
              (match side with
              | `Source -> "MYSQL_PWD"
              | `Target -> "QUIVER_TO_PWD"))
    and timeouts =
      Term.(
        const (fun connecting silence ->
            {
              Quiver_mysql.Link.connecting =
                Core.Time_ns.Span.of_int_sec (Int.max 1 connecting);
              silence = Core.Time_ns.Span.of_int_sec (Int.max 1 silence);
            })
        $ Arg.(
            value & opt int 10
            & described (names [] "connect-timeout") ~docv:"SECONDS"
                ~doc:
                  "Fail when connecting, TLS or authentication waits for the \
                   server so long.")
        $ Arg.(
            value & opt int 600
            & described (names [] "net-timeout") ~docv:"SECONDS"
                ~doc:
                  "Fail when a connection sends and receives nothing for so \
                   long. A global read lock can wait for long queries, so \
                   keep it above the longest one."))
    and zero_dates =
      match side with
      | `Target -> Term.const Quiver_mysql.Provider.Reject
      | `Source ->
          Arg.(
            value
            & opt
                (choices Quiver_mysql.Provider.string_of_zero_dates
                   Quiver_mysql.Provider.all_of_zero_dates)
                Quiver_mysql.Provider.Reject
            & described [ "zero-dates" ] ~docv:"MODE"
                ~doc:
                  "$(b,reject) fails on a zero or invalid date such as \
                   0000-00-00 or 2020-02-30, which has no exact value; \
                   $(b,keep) writes every date as stored, and restoring it \
                   needs a server that accepts it.")
    and keyless_read =
      match side with
      | `Target -> Term.const Quiver_mysql.Provider.Scan
      | `Source ->
          Arg.(
            value
            & opt
                (choices Quiver_mysql.Provider.string_of_keyless_read
                   Quiver_mysql.Provider.all_of_keyless_read)
                Quiver_mysql.Provider.Scan
            & described [ "keyless-read" ] ~docv:"WAY"
                ~doc:
                  "How a table without a primary key or a unique key over NOT NULL columns \
                   is read: $(b,scan) in one pass over the whole table, cheap for the \
                   server; $(b,ranges) by every reader at once, in ranges of another \
                   index's first column, each row looked up through that index.")
    in
    let route =
      Term.(
        const (fun socket host port tls ->
            match (socket, host, port, tls) with
            | Some path, None, None, `Unset ->
                Result.map
                  (fun path -> Quiver_mysql.Link.Socket path)
                  (file_path path)
            | Some _, _, _, _ ->
                Error
                  (`Msg "--socket takes no --host, --port or --tls options")
            | None, (None | Some "localhost"), None, `Unset ->
                Result.map_error
                  (fun (`Local error) ->
                    `Msg
                      ("without --host, or with localhost, quiver connects to \
                        this machine's Unix socket, as the mysql client does, \
                        and none was found: "
                      ^ Sexplib0.Sexp.to_string
                          (Quiver_mysql.Local.sexp_of_error error)
                      ^ "; give --socket, or --host 127.0.0.1 for TCP"))
                  (Result.map
                     (fun path -> Quiver_mysql.Link.Socket path)
                     (Quiver_mysql.Local.socket Quiver_mysql.Local.system))
            | None, Some "localhost", _, _ ->
                Error
                  (`Msg
                     "localhost is this machine's Unix socket, which takes no \
                      --port or --tls options; give 127.0.0.1 for TCP")
            | None, host, port, tls ->
                Result.map
                  (fun tls ->
                    Quiver_mysql.Link.Tcp
                      {
                        host = Option.value host ~default:"127.0.0.1";
                        port = Option.value port ~default:3306;
                        tls;
                      })
                  (match tls with
                  | `Unset ->
                      Ok
                        (Quiver_mysql.Link.Verified_tls
                           Quiver_mysql.Transport.System_roots)
                  | `Set tls -> tls))
        $ socket $ host $ port $ tls)
    in
    let placed =
      Term.(
        const (fun socket host port tls ->
            match (socket, host, port, tls) with None, None, None, `Unset -> `Defaulted | _ -> `Given)
        $ socket $ host $ port $ tls)
    in
    Term.(
      const (fun placed route user password timeouts zero_dates keyless_read ->
          ( placed,
          Result.map
            (fun route ->
              {
                Quiver_mysql.Provider.route;
                user;
                password;
                timeouts;
                zero_dates;
                generated = Left_out;
                keyless_read;
              })
            route ))
      $ placed $ route $ user $ password $ timeouts $ zero_dates $ keyless_read)
  in
  let server side =
    Cmdliner.Term.(term_result (const (fun (_, config) -> Result.map warned config) $ conn side))
  in
  let other_server =
    Cmdliner.Term.(
      term_result
        (const (fun (placed, config) ->
             match placed with `Defaulted -> Ok None | `Given -> Result.map (fun config -> Some (warned config)) config)
        $ conn `Target))
  in
  let mask_key secret =
    match secret with
    | Some secret -> Ok (Quiver_core.Keyed.of_secret secret)
    | None -> (
        match In_channel.with_open_bin "/dev/urandom" (fun random -> really_input_string random 32) with
        | exception (Sys_error message | Failure message) ->
            Error (`Msg ("no random key for masking: " ^ message ^ "; give --mask-secret"))
        | exception End_of_file -> Error (`Msg "no random key for masking; give --mask-secret")
        | random ->
            Format.eprintf
              "quiver: masking with a key of this run only; --mask-secret masks the same values the \
               same way every run@.";
            Ok (Quiver_core.Keyed.of_secret random))
  in
  let load_scripts files =
    Base.List.fold_result files ~init:Quiver_core.Mask_script.empty ~f:(fun script file ->
        match In_channel.with_open_bin file In_channel.input_all with
        | exception Sys_error message -> Error (`Msg message)
        | text ->
            let load path =
              let path = if Filename.is_relative path then Filename.concat (Filename.dirname file) path else path in
              match In_channel.with_open_bin path In_channel.input_all with
              | exception Sys_error message -> Error message
              | words -> Ok words
            in
            Result.map_error
              (fun (`Mask_script error) -> `Msg (Quiver_core.Mask_script.describe error))
              (Quiver_core.Mask_script.parse ~source:file ~within:script ~load text))
  in
  let mask_secret =
    Cmdliner.Arg.(
      value
      & opt (some string) None
      & described [ "mask-secret" ] ~docv:"SECRET"
          ~doc:
            "Key $(b,permute), $(b,shape), $(b,email), $(b,hash) and the keyed functions of \
             $(b,expr:) with $(docv), so a value masks the same way in every column and every run; \
             a random key of this run when not given. Whoever holds $(docv) can undo \
             $(b,permute): keep it apart from the copies, one per environment.")
  in
  let objects =
    let open Cmdliner in
    let kinds : (string * Quiver_core.Filter.kind) list =
      [ ("views", Views); ("triggers", Triggers); ("routines", Routines); ("events", Events) ]
    in
    let parse text : (Quiver_core.Filter.objects, [ `Msg of string ]) result =
      match (text, String.split_on_char ',' text) with
      | "all", _ -> Ok (With_objects Quiver_core.Filter.every_kind)
      | "none", _ -> Ok Without_objects
      | _, names -> (
          match Base.Option.all (List.map (fun name -> List.assoc_opt name kinds) names) with
          | Some (first :: rest) -> Ok (With_objects (first, rest))
          | Some [] | None ->
              Error (`Msg ("expected all, none or a list of " ^ String.concat ", " (List.map fst kinds))))
    in
    let print ppf (objects : Quiver_core.Filter.objects) =
      let named kind = Option.fold ~none:"" ~some:fst (List.find_opt (fun (_, k) -> k = kind) kinds) in
      match objects with
      | Without_objects -> Format.pp_print_string ppf "none"
      | With_objects (first, rest) -> Format.pp_print_string ppf (String.concat "," (List.map named (first :: rest)))
    in
    Arg.(
      value
      & opt (conv (parse, print)) (Quiver_core.Filter.With_objects Quiver_core.Filter.every_kind)
      & described [ "objects" ] ~docv:"WHICH"
          ~doc:
            "$(b,all) takes the databases' views, triggers, routines (procedures and \
             functions) and events; $(b,none) takes none of them; a list such as \
             $(b,views,routines) takes those kinds only, for a target that lacks the others.")
  in
  let reading =
    let open Cmdliner in
    let mode =
      Arg.(
        value
        & opt
            (choices Quiver_mysql.Provider.string_of_snapshot_mode
               Quiver_mysql.Provider.all_of_snapshot_mode)
            Quiver_mysql.Provider.Auto
        & described [ "snapshot" ] ~docv:"MODE"
            ~doc:
              "$(b,auto) takes the server's own consistent snapshot: $(b,ftwrl) on MySQL \
               and MariaDB, the TSO on TiDB (no lock; the sessions hold it from garbage \
               collection while they read). $(b,ftwrl) and $(b,backup-stage) (MariaDB) \
               give one consistent snapshot; $(b,trx-only) takes no lock and is only best \
               effort.")
    in
    let rule parse =
      Arg.conv
        ( parse,
          fun ppf rule ->
            Sexplib0.Sexp.pp_hum ppf (Quiver_core.Filter.sexp_of_rule rule) )
    in
    let database_rule =
      rule (fun text ->
          Result.map
            (fun database -> Quiver_core.Filter.Database database)
            (ident_of_string text))
    and table_rule =
      rule (fun text ->
          match Base.String.lsplit2 text ~on:'.' with
          | None -> Error (`Msg "expected DB.TABLE")
          | Some (database, table) ->
              let%map database = ident_of_string database
              and table = ident_of_string table in
              Quiver_core.Filter.Table { database; table })
    in
    let filter =
      Term.(
        const (fun databases tables ->
            match databases @ tables with
            | [] -> Quiver_core.Filter.Everything
            | first :: rest -> Only (first, rest))
        $ Arg.(
            value & opt_all database_rule []
            & described [ "B"; "database" ] ~docv:"DB"
                ~doc:"Dump only $(docv); repeatable.")
        $ Arg.(
            value & opt_all table_rule []
            & described [ "T"; "table" ] ~docv:"DB.TABLE"
                ~doc:
                  "Dump only $(docv); repeatable. Its database's views, \
                   triggers, routines and events still come along unless \
                   $(b,--objects none)."))
    in
    let rows =
      Term.(
        term_result'
          (const (fun data databases tables ->
               match (data, databases @ tables) with
               | `Rows, [] -> Ok Quiver_core.Filter.All_rows
               | `Rows, first :: rest -> Ok (Rows_of (first, rest))
               | `No_data, [] -> Ok No_rows
               | `No_data, _ :: _ ->
                   Error "--no-data takes no --data-database or --data-table")
          $ Arg.(
              value
              & vflag `Rows
                  [
                    ( `No_data,
                      described [ "no-data" ]
                        ~doc:"Take every table's schema and no rows." );
                  ])
          $ Arg.(
              value & opt_all database_rule []
              & described [ "data-database" ] ~docv:"DB"
                  ~doc:
                    "Take rows only of $(docv)'s tables, repeatable; the other \
                     tables of the dump give only their schema.")
          $ Arg.(
              value & opt_all table_rule []
              & described [ "data-table" ] ~docv:"DB.TABLE"
                  ~doc:
                    "Take rows only of $(docv), repeatable; the other tables of \
                     the dump give only their schema.")))
    in
    let masking =
      let mask_rule =
        Arg.conv
          ( (fun text ->
              Result.map_error
                (function
                  | `Mask_script error -> `Msg (Quiver_core.Mask_script.describe error)
                  | (`Mask _ | `Pattern _) as error ->
                      `Msg
                        (Sexplib0.Sexp.to_string_hum
                           ([%sexp_of:
                              [ `Mask of Quiver_core.Mask.error
                              | `Pattern of Quiver_core.Pattern.error ]]
                              error)))
                (Quiver_core.Mask.rule text)),
            fun ppf rule -> Sexplib0.Sexp.pp_hum ppf (Quiver_core.Mask.sexp_of_rule rule) )
      in
      Term.(
        term_result
          (const (fun rules files strict secret ->
               match (rules, files, strict) with
               | [], [], [] -> Ok (Quiver_core.Mask.none, [])
               | _ -> (
                   match
                     let%bind script = load_scripts files in
                     let%bind scripted =
                       Result.map_error
                         (fun (`Pattern error) ->
                           `Msg (Sexplib0.Sexp.to_string_hum (Quiver_core.Pattern.sexp_of_error error)))
                         (Quiver_core.Mask.scripted script)
                     in
                     let%bind strict =
                       Base.Result.all
                         (List.map
                            (fun (written, table) ->
                              Result.map_error
                                (fun (`Pattern error) ->
                                  `Msg (written ^ ": " ^ Sexplib0.Sexp.to_string_hum (Quiver_core.Pattern.sexp_of_error error)))
                                (Result.map (fun pattern -> (written, pattern)) (Quiver_core.Pattern.table table)))
                            (List.map (fun table -> ("--mask-strict " ^ table, table)) strict
                            @ List.map
                                (fun ({ Quiver_core.Mask_script.source; line; _ }, table) ->
                                  (Printf.sprintf "%s:%d (strict %s)" source line table, table))
                                (Quiver_core.Mask_script.strict script)))
                     in
                     let%bind after =
                       Base.Result.all
                         (List.map
                            (fun ({ Quiver_core.Mask_script.source; line; _ }, table, statement) ->
                              Result.map_error
                                (fun _ -> `Msg (Printf.sprintf "%s:%d: after-load names no DB.TABLE" source line))
                                (Quiver_load.After_load.rule (table ^ "=" ^ statement)))
                            (Quiver_core.Mask_script.after_load script))
                     in
                     let%map key = mask_key secret in
                     (Quiver_core.Mask.make key ~script ~strict (rules @ scripted), after)
                   with
                   | Ok masking -> Ok masking
                   | Error (`Msg message) -> Error (`Msg message)))
          $ Arg.(
              value & opt_all mask_rule []
              & described [ "mask" ] ~docv:"DB.TABLE.COLUMN=ACTION"
                  ~doc:
                    "Mask the column's values before they leave the reader, repeatable; a column \
                     takes the first rule naming it, and a rule naming no column of the dump is \
                     an error. Names take $(b,*) and $(b,?), or backquotes for a name as written. \
                     $(docv) is $(b,permute) (a keyed bijection, FF1 of NIST SP 800-38G: an \
                     integer to another from 1 to half its type's largest, or to 2^53 - 1 for \
                     BIGINT; text keeping each character's kind, case and place; binary of the \
                     same length), $(b,permute:)LOW..HIGH (integers among those); $(b,null); \
                     $(b,text:)VALUE; $(b,shape) (each letter and digit replaced by a keyed one \
                     of its kind, the rest kept); $(b,email) (shape before the last @, the domain \
                     kept); $(b,hash) (a keyed digest of the same length, or an integer of the \
                     column's type); $(b,sql:)EXPRESSION, evaluated by the source for each row \
                     and cast to the column's type; or $(b,expr:)EXPRESSION of the masking \
                     language (see $(b,quiver mask --help)). NULL stays NULL but under $(b,null), \
                     $(b,sql:) and $(b,expr:). A column of a primary or unique key takes only \
                     $(b,permute), $(b,keep) or $(b,null) where it may be NULL, since another \
                     action can give two values one. A $(b,permute) spreads along the foreign keys \
                     of the source to every column tied to the one it masks that no rule names, \
                     so references keep pointing at their rows; tied columns masked differently \
                     are an error.")
          $ Arg.(
              value & opt_all file []
              & described [ "mask-script" ] ~docv:"FILE"
                  ~doc:
                    "Read definitions and $(b,(mask \"DB.TABLE.COLUMN\" EXPRESSION)) rules of the \
                     masking language from $(docv), repeatable: a later file calls an earlier \
                     one's definitions, and its rules come after those of $(b,--mask). Its \
                     $(b,(after-load …)) forms run with $(b,copy). See $(b,quiver mask --help).")
          $ Arg.(
              value & opt_all string []
              & described [ "mask-strict" ] ~docv:"DB.TABLE"
                  ~doc:
                    "Every column of $(docv) must be masked or kept ($(b,=keep)) by a rule, \
                     repeatable; a column nobody decided on stops the run before anything is read.")
          $ mask_secret))
    in
    let threads =
      Arg.(
        value
        & opt (some int) None
        & described [ "t"; "threads" ] ~docv:"N"
            ~doc:
              "Database sessions reading in parallel, one domain each; by \
               default half the cores quiver may use (at least 4), fewer when \
               the server suggests fewer. quiver uses this machine's fast \
               physical cores within its CPU quota, half of them when the \
               server is on this machine.")
    in
    let render_threads =
      Arg.(
        value
        & opt (some int) None
        & described [ "render-threads" ] ~docv:"N"
            ~doc:
              "Domains turning batches into output; by default half the \
               readers, within the cores quiver may use.")
    in
    let chunk_bytes =
      Arg.(
        value
        & opt int (64 * 1024 * 1024)
        & described [ "chunk-bytes" ] ~docv:"BYTES" ~doc:"Target size of one chunk.")
    in
    let batch_bytes =
      Arg.(
        value & opt int (256 * 1024)
        & described [ "batch-bytes" ] ~docv:"BYTES"
            ~doc:
              "A reader hands a batch of rows on once it holds this many bytes; larger batches \
               are not faster, smaller ones cost more per row.")
    in
    let order =
      Arg.(
        value
        & opt
            (enum
               [
                 ("largest-chunks", Quiver_core.Dump_opts.Largest_chunks);
                 ("largest-tables", Largest_tables);
               ])
            Quiver_core.Dump_opts.Largest_chunks
        & described [ "read-order" ] ~docv:"ORDER"
            ~doc:
              "$(b,largest-chunks) reads the largest chunks first, wherever their tables are; \
               $(b,largest-tables) reads the largest tables first, their chunks together, so what \
               a sink does after a table's last part (a tablespace's check, import and keys) \
               starts early.")
    and declaring =
      Arg.(
        value
        & opt
            (enum
               [
                 ("before-rows", Quiver_core.Dump_opts.Before_its_rows);
                 ("all-at-once", All_at_once);
               ])
            Quiver_core.Dump_opts.Before_its_rows
        & described [ "declare-tables" ] ~docv:"WHEN"
            ~doc:
              "$(b,before-rows) tells the sink of a table just before its first chunk; \
               $(b,all-at-once) tells it of every table before any rows, so $(b,copy) creates \
               or prepares them while rows are read.")
    in
    Term.(
      term_result
        (const
           (fun
             conn mode filter rows objects masking threads render_threads chunk_bytes
             batch_bytes order declaring ->
             Result.map_error
               (fun error ->
                 `Msg (Sexplib0.Sexp.to_string_hum (Quiver_core.Error.sexp_of_t error)))
               (let%bind plan = Quiver_core.Plan_opts.make ~chunk_bytes in
                let%map opts =
                  Quiver_core.Dump_opts.make ~plan
                    ~readers:
                      (Option.fold ~none:Quiver_core.Dump_opts.Fitting
                         ~some:(fun threads -> Quiver_core.Dump_opts.Readers threads)
                         threads)
                    ~renderers:
                      (Option.fold ~none:Quiver_core.Dump_opts.Spare
                         ~some:(fun threads -> Quiver_core.Dump_opts.Renderers threads)
                         render_threads)
                    ~cores:
                      (Quiver_core.Cores.budget
                         (Quiver_mysql.Local.machine conn.Quiver_mysql.Provider.route))
                    ~batch_bytes ~order ~declaring
                in
                let masking, scripted_after = masking in
                { conn; mode; filter; rows; objects; masking; scripted_after; opts }))
        $ server `Source $ mode $ filter $ rows $ objects $ masking $ threads $ render_threads
        $ chunk_bytes
        $ batch_bytes $ order $ declaring))
  in
  let output =
    let open Cmdliner in
    let format =
      Arg.(
        value
        & opt (enum [ ("sql", `Sql); ("csv", `Csv) ]) `Sql
        & described [ "format" ] ~docv:"FORMAT"
            ~doc:"$(b,sql): INSERT statements myloader and mysql read; $(b,csv): one CSV file per chunk.")
    and dialect =
      Arg.(
        value
        & opt
            (choices Quiver_core.Output_format.string_of_dialect
               Quiver_core.Output_format.all_of_dialect)
            Quiver_core.Output_format.Rfc4180
        & described [ "csv-dialect" ] ~docv:"DIALECT"
            ~doc:
              "$(b,rfc4180): RFC 4180 with a header line and CRLF; $(b,mysql-load-data): what \
               LOAD DATA reads with backslash escapes and no header.")
    and compress =
      Arg.(
        value
        & opt (enum [ ("none", `None); ("zstd", `Zstd) ]) `None
        & described [ "compress" ] ~docv:"CODEC"
            ~doc:"$(b,zstd) compresses every data file, which zstd -d reads back; $(b,none) does not.")
    and level =
      Arg.(
        value & opt int 3
        & described [ "zstd-level" ] ~docv:"LEVEL"
            ~doc:"The zstd level of $(b,--compress zstd), from 1 (fastest) to 19.")
    and statement_bytes =
      Arg.(
        value & opt int 1_000_000
        & described [ "statement-bytes" ] ~docv:"BYTES"
            ~doc:"Approximate size of one INSERT statement.")
    in
    Term.(
      const (fun format dialect compress level statement_bytes dir ->
          {
            dir;
            format =
              (match format with
              | `Sql -> Quiver_core.Output_format.Sql
              | `Csv -> Csv dialect);
            compression =
              (match compress with `None -> `Plain | `Zstd -> `Zstd level);
            statement_bytes;
          })
      $ format $ dialect $ compress $ level $ statement_bytes)
  in
  let outputdir ~doc =
    Cmdliner.Arg.(
      opt (some (conv (file_path, File_path.pp))) None
      & described [ "o"; "outputdir" ] ~docv:"DIR" ~doc)
  in
  let dump_cmd =
    let open Cmdliner in
    Cmd.v
      (Cmd.info "dump" ~doc:"Dump MySQL or MariaDB into SQL or CSV files")
      Term.(
        const (fun reading dir output -> dump reading (output dir))
        $ reading
        $ Arg.required (outputdir ~doc:"Where the dump goes.")
        $ output)
  in
  let keys =
    let open Cmdliner in
    Arg.(
      value
      & opt
          (choices Quiver_load.Load.string_of_keys Quiver_load.Load.all_of_keys)
          Quiver_load.Load.With_table
      & described [ "secondary-keys" ] ~docv:"WHEN"
          ~doc:
            "$(b,with-table) creates tables as dumped; $(b,after-data) creates \
             them with their primary key only and adds secondary and foreign \
             keys once a table's data is in, so InnoDB builds each index by \
             sorting, which pays off when indexes outgrow memory.")
  in
  let after_load =
    let open Cmdliner in
    let after_rule =
      Arg.conv
        ( (fun text ->
            Result.map_error
              (fun error ->
                `Msg
                  (Sexplib0.Sexp.to_string_hum
                     ([%sexp_of:
                        [ `After_load of Quiver_load.After_load.error
                        | `Pattern of Quiver_core.Pattern.error ]]
                        error)))
              (Quiver_load.After_load.rule text)),
          fun ppf rule -> Sexplib0.Sexp.pp_hum ppf (Quiver_load.After_load.sexp_of_rule rule) )
    in
    Arg.(
      value & opt_all after_rule []
      & described [ "after-load" ] ~docv:"DB.TABLE=STATEMENT"
          ~doc:
            "Run $(docv) on the target once every table's rows and keys are in, before foreign \
             keys, views and triggers, repeatable: in the target's database of the table, \
             $(b,{table}) standing for its name there; a table's statements in the given order, \
             tables at once. Names take $(b,*) and $(b,?); a rule naming no table is an error.")
  in
  let ingest =
    let open Cmdliner in
    Term.(
      const (fun way datadir builder sorting threads ->
          match way with
          | `Load_data -> Quiver_load.Copy.Load_data
          | `Tablespace -> Tablespaces { datadir; builder; sorting; threads })
      $ Arg.(
          value
          & opt (enum [ ("load-data", `Load_data); ("tablespace", `Tablespace) ]) `Load_data
          & described [ "ingest" ] ~docv:"WAY"
              ~doc:
                "$(b,tablespace) builds each table it can as an InnoDB tablespace beside the \
                 target and imports it (MySQL 8.4 only, with $(b,--binlog skip), quiver on the \
                 target's machine); other tables, and every table with $(b,load-data), go \
                 through LOAD DATA.")
      $ Arg.(
          value
          & opt (some string) None
          & described [ "datadir" ] ~docv:"DIR"
              ~doc:
                "The target's data directory as this machine sees it, for $(b,--ingest \
                 tablespace); the server's own when not given.")
      $ Arg.(
          value
          & opt (enum [ ("quiver", Quiver_load.Physical.Quiver); ("server", Server) ]) Quiver_load.Physical.Quiver
          & described [ "tablespace-keys" ] ~docv:"WHO"
              ~doc:
                "Who builds the secondary keys of a table built as a tablespace: $(b,quiver) builds \
                 those ordered by bytes (integers, dates and times, decimals, binary strings) into \
                 the file, the server adds the others after the import (the default); \
                 $(b,server) adds them all after the import.")
      $ Arg.(
          value
          & opt (some dir) None
          & described [ "sort-dir" ] ~docv:"DIR"
              ~doc:
                "Where a table's secondary records wait, sorted, until its file is finished; beside \
                 the file in the target's data directory when not given.")
      $ Arg.(
          value
          & opt (some int) None
          & described [ "build-threads" ] ~docv:"N"
              ~doc:
                "Threads finishing and checking built files, between all tables at once: the \
                 larger a table, the more it borrows; half the fast cores when not given."))
  in
  let binlog =
    let open Cmdliner in
    Arg.(
      value
      & opt
          (choices Quiver_load.Load.string_of_binlog Quiver_load.Load.all_of_binlog)
          Quiver_load.Load.Write_binlog
      & described [ "binlog" ] ~docv:"MODE"
          ~doc:
            "$(b,skip) keeps the load out of the server's binlog (less work \
             for the server, but replicas never see the data; needs the right \
             to set sql_log_bin); $(b,write) logs it as usual.")
  in
  let writing =
    let open Cmdliner in
    let collations =
      Term.term_result'
        Term.(
          const (fun rules ->
              Result.map_error
                (fun (`Collations error) ->
                  Sexplib0.Sexp.to_string_hum (Quiver_load.Collations.sexp_of_error error))
                (Quiver_load.Collations.of_rules rules))
          $ Arg.(
              value & opt_all string []
              & described [ "collation" ] ~docv:"FROM=TO"
                  ~doc:
                    "Create what the source made in collation $(i,FROM) in $(i,TO) of the same \
                     character set, repeatable: for a target that lacks $(i,FROM), such as TiDB \
                     with MariaDB's utf8mb4_uca1400_ai_ci. The bytes stay as read; text then \
                     compares and sorts by $(i,TO), so a unique key may refuse values $(i,FROM) \
                     told apart."))
    in
    Term.(
      const (fun keys after binlog collations -> { Quiver_load.Load.keys; after; binlog; collations })
      $ keys $ after_load $ binlog $ collations)
  in
  let load_cmd =
    let open Cmdliner in
    let dir =
      Arg.(
        required
        & opt (some dir) None
        & described [ "d"; "directory" ] ~docv:"DIR" ~doc:"The dump to load.")
    and into =
      Arg.(
        value
        & opt (some ident) None
        & described [ "B"; "database" ] ~docv:"DB"
            ~doc:"Load the dump's only database under this name.")
    and threads =
      Arg.(
        value & opt int 4
        & described [ "t"; "threads" ] ~docv:"N"
            ~doc:"Sessions loading in parallel, one domain each.")
    in
    Cmd.v
      (Cmd.info "load" ~doc:"Load a quiver dump into MySQL or MariaDB")
      Term.(const load $ server `Source $ dir $ into $ writing $ objects $ threads)
  in
  let mirror_cmd =
    let open Cmdliner in
    let database =
      Arg.(required & opt (some ident) None & described [ "B"; "database" ] ~docv:"DB" ~doc:"The database whose tables are copied.")
    and into =
      Arg.(
        required
        & opt (some ident) None
        & described [ "to-database" ] ~docv:"DB"
            ~doc:"Where the copies go, made if it does not exist; a table already there is refused.")
    and tables =
      Arg.(
        value & opt_all ident []
        & described [ "T"; "table" ] ~docv:"TABLE" ~doc:"Copy only this table, repeatable; all of the database's when not given.")
    and workers =
      Arg.(
        value & opt int 8
        & described [ "w"; "workers" ] ~docv:"N"
            ~doc:"Threads copying a table at once, each with connections of its own to PD and every TiKV store.")
    and stores =
      Arg.(
        value
        & opt (enum [ ("untouched", Quiver_tikv.Mirror.Untouched); ("import", Imported) ]) Quiver_tikv.Mirror.Untouched
        & described [ "store-mode" ] ~docv:"MODE"
            ~doc:
              "$(b,untouched) leaves the TiKV stores as they are; $(b,import) keeps every store in \
               import mode while a table's keys go in, as IMPORT INTO does: compaction never stalls \
               the writes and no region is split by size, but compaction takes more threads and the \
               cluster's other writes may slow down meanwhile (on one machine running every store it \
               made copies slower), and a store left in it goes back by itself after ten minutes.")
    and cluster side =
      let named long = match side with `Source -> long | `Target -> "to-" ^ long in
      let whose = match side with `Source -> "the source's" | `Target -> "the target's" in
      let pem long ~doc = Arg.(value & opt (some file) None & described [ named long ] ~docv:"FILE" ~doc) in
      Term.(
        const (fun ca certificate key ->
            let secured ca identity =
              let refused (unusable : Quiver_openssl.Tls.unusable) =
                let message =
                  match unusable with
                  | Context reason -> "TLS to PD and TiKV: " ^ reason
                  | Certificate reason -> "--" ^ named "cluster-cert" ^ ": " ^ reason
                  | Key reason -> "--" ^ named "cluster-key" ^ ": " ^ reason
                  | Unmatching_keys -> "--" ^ named "cluster-cert" ^ " and its key do not match"
                  | Peers reason -> "--" ^ named "cluster-ca" ^ ": " ^ reason
                in
                `Msg message
              in
              Result.map_error refused
                (Result.map
                   (fun context -> Quiver_tikv.Http2.Secured context)
                   (Quiver_openssl.Tls.http2 (Ca_file ca) identity))
            in
            match (ca, certificate, key) with
            | None, None, None -> Ok Quiver_tikv.Http2.Cleartext
            | Some ca, None, None -> secured ca Anonymous
            | Some ca, Some certificate, Some key -> secured ca (Presenting { certificate; key })
            | Some _, Some _, None | Some _, None, Some _ ->
                Error (`Msg ("--" ^ named "cluster-cert" ^ " and --" ^ named "cluster-key" ^ " go together"))
            | None, (Some _ | None), (Some _ | None) ->
                Error (`Msg ("--" ^ named "cluster-cert" ^ " and its key go with --" ^ named "cluster-ca")))
        $ pem "cluster-ca"
            ~doc:
              ("PEM file of the CAs " ^ whose
             ^ " PD and TiKV certificates lead to, as their security.ca-path: they are then reached over \
                TLS, their chains and host names checked; without it, in the clear")
        $ pem "cluster-cert"
            ~doc:("PEM certificate chain this side presents to " ^ whose ^ " PD and TiKV, as their security.cert-path asks of clients")
        $ pem "cluster-key" ~doc:("PEM key of the $(b,--" ^ named "cluster-cert" ^ ") certificate"))
    in
    let target =
      Term.(
        term_result
          (const (fun other security ->
               match (other, security) with
               | Some other, security -> Ok (Some (other, security))
               | None, Quiver_tikv.Http2.Cleartext -> Ok None
               | None, Secured _ -> Error (`Msg "--to-cluster-ca names another cluster's CAs, which goes with --to-host"))
          $ other_server $ term_result (cluster `Target)))
    in
    Cmd.v
      (Cmd.info "mirror"
         ~doc:
           "Copy tables of a TiDB cluster into another database, of the same cluster or of the one \
            the $(b,--to-host) options name, as TiKV keeps them: rows and indexes read at one TSO \
            and imported under the new tables' ids, with no SQL on the way, checked against TiDB's \
            checksum. Into another cluster the two must agree on new collations, and the copy is \
            made by the source's definition; where its columns and indexes have other ids, as \
            after the source's ALTERs, keys and rows are written under the copy's")
      Term.(const mirror $ server `Source $ term_result (cluster `Source) $ target $ database $ into $ tables $ workers $ stores)
  in
  let copy_cmd =
    let open Cmdliner in
    let into =
      Arg.(
        value
        & opt (some ident) None
        & described [ "to-database" ] ~docv:"DB"
            ~doc:"Copy the only database read under this name.")
    in
    Cmd.v
      (Cmd.info "copy"
         ~doc:
           "Copy MySQL or MariaDB into another server, with no files: each \
            reading session's chunks stream into a loading session of its own")
      Term.(
        const copy $ reading $ server `Target $ into $ writing $ ingest
        $ Arg.(
            value
            & opt (some int) None
            & described [ "target-threads" ] ~docv:"N"
                ~doc:
                  "Sessions loading into the target, one chunk each; defaults \
                   to $(b,--threads). Readers wait for a free one, so loading \
                   at its own pace never stalls rendering."))
  in
  let wire =
    let open Cmdliner in
    let file name ~doc =
      Arg.(value & opt (some file) None & described [ name ] ~docv:"FILE" ~doc)
    in
    Term.(
      term_result
        (const (fun mode certificate key peers ->
             match (mode, certificate, key, peers) with
             | `Verified, Some certificate, Some key, Some peers ->
                 Result.map_error
                   (fun (`Protection error) ->
                     `Msg
                       (Sexplib0.Sexp.to_string_hum
                          (Quiver_net.Protection.sexp_of_error error)))
                   (Quiver_net.Protection.mutual
                      ~certificate:(File_path.of_string certificate)
                      ~key:(File_path.of_string key)
                      ~peers:(File_path.of_string peers))
             | `Verified, (Some _ | None), (Some _ | None), (Some _ | None) ->
                 Error
                   (`Msg
                      "quiver's protocol needs --wire-cert, --wire-key and \
                       --wire-peers, or --wire-tls disabled")
             | `Disabled, None, None, None -> Ok Quiver_net.Protection.plaintext
             | `Disabled, (Some _ | None), (Some _ | None), (Some _ | None) ->
                 Error
                   (`Msg
                      "--wire-tls disabled takes no --wire-cert, --wire-key or \
                       --wire-peers"))
        $ Arg.(
            value
            & opt
                (enum [ ("verified", `Verified); ("disabled", `Disabled) ])
                `Verified
            & described [ "wire-tls" ] ~docv:"MODE"
                ~doc:
                  "$(b,verified): TLS 1.3 on every connection of quiver's \
                   protocol, and each side must present a certificate the \
                   other trusts; $(b,disabled): plaintext, where anyone who \
                   reaches the port can send or read the dump, for private \
                   networks only.")
        $ file "wire-cert"
            ~doc:
              "This node's certificate chain, PEM; it must name the host the \
               connecting side dials, and it serves both ways on a hop."
        $ file "wire-key" ~doc:"The private key of $(b,--wire-cert), PEM."
        $ file "wire-peers"
            ~doc:
              "PEM certificates a peer's must chain to: the CA of your nodes, \
               or the peers' own self-signed certificates."))
  in
  let reconnect =
    Cmdliner.Arg.(
      value & opt float 60.
      & described [ "reconnect" ] ~docv:"SECONDS"
          ~doc:
            "How long a broken control connection may take to be replaced \
             before the session fails; the agent keeps its snapshot and \
             sends again what did not arrive.")
  in
  let agent_cmd =
    let open Cmdliner in
    let collector =
      Arg.(
        required
        & opt (some string) None
        & described [ "collector" ] ~docv:"HOST:PORT"
            ~doc:"The collector that receives the dump ($(b,quiver collect)).")
    and level =
      Arg.(
        value
        & opt
            (conv
               ( (function
                 | "auto" -> Ok `Auto
                 | text -> (
                     match int_of_string_opt text with
                     | Some level -> Ok (`Level level)
                     | None -> Error (`Msg ("not a level: " ^ text)))),
                 fun ppf -> function
                   | `Auto -> Format.pp_print_string ppf "auto"
                   | `Level level -> Format.pp_print_int ppf level ))
            `Auto
        & described [ "zstd-level" ] ~docv:"LEVEL"
            ~doc:
              "Compression of batches on the wire: a zstd level, or $(b,auto) \
               to move between none and zstd -5, 1 and 3 by what holds the \
               dump back.")
    in
    Cmd.v
      (Cmd.info "agent"
         ~doc:
           "Dump MySQL or MariaDB to a collector over quiver's own protocol: \
            batches travel as they lie in memory, compressed, spread over \
            $(b,--streams) connections")
      Term.(
        const agent $ reading $ wire $ collector $ level
        $ Arg.(
            value
            & opt (some int) None
            & described [ "streams" ] ~docv:"N"
                ~doc:
                  "Data connections; defaults to $(b,--threads). Every \
                   connection carries batches of any chunk, so more of them \
                   than readers fill a long or lossy path.")
        $ Arg.(
            value & opt int 64
            & described [ "most-streams" ] ~docv:"N"
                ~doc:
                  "While the network holds the dump back, open more data \
                   connections, up to so many, keeping those that carry \
                   more; as many as $(b,--streams) keeps their number.")
        $ Arg.(
            value
            & opt (some int) None
            & described [ "chunks-in-flight" ] ~docv:"N"
                ~doc:
                  "Chunks begun and not yet committed by the collector; \
                   defaults to twice $(b,--threads), so readers go on while \
                   commits travel back. The collector loads a server with as \
                   many sessions.")
        $ Arg.(
            value
            & opt (some float) None
            & described [ "max-rate" ] ~docv:"MB/S"
                ~doc:
                  "Megabytes a second that all data connections together \
                   send at most, so the dump leaves room on a shared path.")
        $ Arg.(
            value
            & opt
                (some
                   (conv
                      ( (fun text ->
                          Option.fold
                            ~none:(Error (`Msg "expected 0 to 63"))
                            ~some:Result.ok
                            (Option.bind (int_of_string_opt text)
                               Quiver_core.Tcp.Class.of_int)),
                        fun ppf traffic ->
                          Format.pp_print_int ppf (traffic :> int) )))
                None
            & described [ "dscp" ] ~docv:"N"
                ~doc:
                  "The DSCP every packet of the dump carries; 8 (CS1) asks \
                   routers to let other traffic go first.")
        $ reconnect
        $ Arg.(
            value & opt float 2.
            & described [ "stall" ] ~docv:"SECONDS"
                ~doc:
                  "A data connection whose oldest frame waits this long for its \
                   acknowledgement, while another connection waits for none or \
                   eight times less, is taken for a dead path: its frames go to \
                   the other connections and a new one replaces it."))
  in
  let listen =
    Cmdliner.Arg.(
      required
      & opt (some string) None
      & described [ "listen" ] ~docv:"HOST:PORT"
          ~doc:"Where to wait for the agent or the hop before.")
  in
  let relay protection reconnect listen next =
    (match
              let%bind next = resolve next in
              let%bind listening = listening listen in
              let%bind session =
                Quiver_net.Collector.accept listening protection
                  ~reconnect:(Core.Time_ns.Span.of_sec reconnect)
              in
              let%bind downstream =
                Quiver_net.Collector.forward next session
              in
              Quiver_net.Collector.relay listening session downstream
            with
            | Ok () ->
                Format.printf "relayed@.";
                0
            | Error (`Msg message) -> told message
            | Error (`Collector error) -> failed (Quiver_net.Collector.sexp_of_error error))
  in
  let relay_cmd =
    let open Cmdliner in
    Cmd.v
      (Cmd.info "relay"
         ~doc:
           "Pass one dump from $(b,quiver agent) on to the next hop \
            ($(b,quiver collect) or another relay) byte for byte: nothing is \
            decompressed or written, and a chunk still counts only once the \
            last hop committed it")
      Term.(
        const relay $ wire $ reconnect $ listen
        $ Arg.(
            required
            & opt (some string) None
            & described [ "forward" ] ~docv:"HOST:PORT" ~doc:"The next hop."))
  in
  let collect_cmd =
    let open Cmdliner in
    let into =
      Arg.(
        value
        & opt (some ident) None
        & described [ "to-database" ] ~docv:"DB"
            ~doc:"Load the only database received under this name.")
    in
    Cmd.v
      (Cmd.info "collect"
         ~doc:
           "Receive one dump from $(b,quiver agent) and write it into files \
            ($(b,-o)) or load it into a server ($(b,--to-host) and the other \
            $(b,--to-) options)")
      Term.(
        const
          (fun protection reconnect listen forward dir output target into writing ingest ->
            collect protection reconnect listen forward (Option.map output dir) target into writing
              ingest)
        $ wire $ reconnect $ listen
        $ Arg.(
            value
            & opt (some string) None
            & described [ "forward" ] ~docv:"HOST:PORT"
                ~doc:
                  "Also pass everything on to this collector as it arrives, so \
                   one read of the source reaches a chain of targets; a chunk \
                   counts once every target committed it.")
        $ Arg.value
            (outputdir ~doc:"Write files here instead of loading into a server.")
        $ output $ Term.(const snd $ conn `Target) $ into $ writing $ ingest)
  in
  let mask_cmd =
    let open Cmdliner in
    let kind =
      Arg.(
        value
        & opt
            (enum
               [
                 ("text", (Text : Quiver_core.Mask_script.kind));
                 ("int", Int);
                 ("date", Date);
                 ("datetime", Datetime);
               ])
            (Text : Quiver_core.Mask_script.kind)
        & described [ "kind" ] ~docv:"KIND"
            ~doc:
              "The column's kind: $(b,text) (text, binary, JSON, ENUM, SET, DECIMAL), $(b,int), \
               $(b,date) (DATE) or $(b,datetime) (DATETIME, TIMESTAMP).")
    and row =
      let parse text =
        let kinds : (string * Quiver_core.Mask_script.kind) list =
          [ ("text", Text); ("int", Int); ("date", Date); ("datetime", Datetime) ]
        in
        match Base.String.lsplit2 text ~on:'=' with
        | None -> Error (`Msg "expected NAME=VALUE or NAME:KIND=VALUE")
        | Some (named, value) -> (
            let name, kind =
              match Base.String.rsplit2 named ~on:':' with
              | Some (name, kind) -> (name, List.assoc_opt kind kinds)
              | None -> (named, Some Text)
            in
            match kind with
            | None -> Error (`Msg ("the kind of " ^ name ^ " is text, int, date or datetime"))
            | Some kind -> (
                match Quiver_core.Mask_script.read_value kind value with
                | Null -> Error (`Msg (value ^ " is not of that kind"))
                | read -> Ok (name, (kind, read))))
      in
      Arg.(
        value
        & opt_all (conv (parse, fun ppf (name, _) -> Format.pp_print_string ppf name)) []
        & described [ "row" ] ~docv:"NAME=VALUE"
            ~doc:
              "Another column of the row, as $(b,(row NAME)) reads it, repeatable; $(b,NAME:int=), \
               $(b,NAME:date=) or $(b,NAME:datetime=) for another kind than text.")
    and scripts =
      Arg.(
        value & opt_all file []
        & described [ "mask-script" ] ~docv:"FILE"
            ~doc:"Definitions the expression may call, repeatable.")
    and expression =
      Arg.(required & pos 0 (some string) None & info [] ~docv:"EXPRESSION" ~doc:"What a column takes.")
    and values =
      Arg.(
        value & pos_right 0 string []
        & info [] ~docv:"VALUE"
            ~doc:"The column's values, each line of standard input when none is given; $(b,\\\\N) is NULL.")
    in
    let run (kind : Quiver_core.Mask_script.kind) rows scripts expression values secret =
      match
        let%bind script = load_scripts scripts in
        let%bind parsed =
          Result.map_error
            (fun (`Mask_script error) -> `Msg (Quiver_core.Mask_script.describe error))
            (Quiver_core.Mask_script.expression ~source:"EXPRESSION" expression)
        in
        let found name =
          List.find_opt (fun (given, _) -> String.equal (String.lowercase_ascii given) (String.lowercase_ascii name)) rows
        in
        let%bind compiled =
          Result.map_error
            (fun (`Mask_script error) -> `Msg (Quiver_core.Mask_script.describe error))
            (Quiver_core.Mask_script.compile script parsed ~value:kind
               ~result:kind
               ~columns:(fun name ->
                 Option.map (fun (_, (kind, _)) -> kind) (found name)))
        in
        let%map key = mask_key secret in
        (compiled, key)
      with
      | Error (`Msg message) -> told message
      | Ok (compiled, key) ->
          let row =
            Array.of_list
              (List.map
                 (fun name ->
                   Option.fold
                     (List.find_opt
                        (fun (given, _) -> String.equal (String.lowercase_ascii given) (String.lowercase_ascii name))
                        rows)
                     ~none:Quiver_core.Mask_script.Null ~some:(fun (_, (_, value)) -> value))
                 (Quiver_core.Mask_script.reads compiled))
          in
          let masked text =
            let value : Quiver_core.Mask_script.value =
              if String.equal text "\\N" then Null else Quiver_core.Mask_script.read_value kind text
            in
            print_endline (Quiver_core.Mask_script.shown (Quiver_core.Mask_script.run compiled key ~value ~row))
          in
          let () =
            match values with
            | [] -> In_channel.fold_lines (fun () line -> masked line) () stdin
            | _ :: _ -> List.iter masked values
          in
          0
    in
    let man =
           [
             `S "THE MASKING LANGUAGE";
             `P
               "$(b,--mask 'DB.TABLE.COLUMN=expr:EXPRESSION') and $(b,(mask \"DB.TABLE.COLUMN\" \
                EXPRESSION)) in a $(b,--mask-script) file give each row of the column the \
                expression's value. It is typed against the table before anything is read and \
                always ends: there are no loops, and a definition calls only those above it.";
             `P
               "Values are text, integers (64 bits), truth values, dates, dates with times, and \
                NULL. A function given NULL gives NULL, as in SQL, except $(b,is-null), \
                $(b,coalesce), $(b,and), $(b,or) and $(b,if), which take NULL as false. A column of \
                text, binary, JSON, ENUM, SET or DECIMAL is text; one of integers an integer; DATE a \
                date; DATETIME and TIMESTAMP a date with a time; other types cannot take an \
                expression (use $(b,sql:)).";
             `I ("$(b,value)", "The column's value in this row.");
             `I ("$(b,(row NAME))", "Another column of the row, as read, before any masking.");
             `I ("$(b,\"text\"), $(b,42), $(b,true), $(b,false), $(b,null)", "Constants.");
             `I ("$(b,if), $(b,cond), $(b,let)", "(if C A B); (cond (C A) … (else B)); (let ((NAME E) …) BODY).");
           ]
           @ List.concat_map
               (fun ({ title; doc; entries } : Quiver_core.Mask_script.section) ->
                 (`S (String.uppercase_ascii title) :: `P doc
                 :: List.map
                      (fun (entry : Quiver_core.Mask_script.entry) ->
                        `I (Quiver_docs.Docs.signature entry, entry.doc))
                      entries))
               Quiver_core.Mask_script.reference
           @ [
             `S "SCRIPT FORMS";
             `I ("(define (NAME PARAMETER …) BODY)", "A function, calling only those above it.");
             `I
               ( "(mask SELECTOR BODY)",
                 "SELECTOR is \"DB.TABLE.COLUMN\" with * and ?, or (where (table \"DB.TABLE\") \
                  (column \"NAME\") (type TYPE …)) with any clause left out, TYPE one of text, binary, \
                  json, integer, decimal, float, date, datetime, timestamp, time, year, enum, set, \
                  geometry. A column takes the first rule naming it." );
             `I ("(keep SELECTOR)", "Its columns are copied as they are, and count as decided.");
             `I
               ( "(permute SELECTOR [LOW HIGH])",
                 "As $(b,--mask DB.TABLE.COLUMN=permute): a keyed bijection, spreading along foreign keys." );
             `I
               ( "(references \"DB.TABLE.COLUMN\" \"DB.TABLE.COLUMN\")",
                 "The first column refers to the second though no foreign key says so." );
             `I
               ( "(strict \"DB.TABLE\" …)",
                 "Every column of these tables must be masked or kept by some rule, or nothing is read." );
             `I ("(words NAME \"FILE\")", "Lines of FILE, beside the script, for $(b,pick-word); # starts a comment.");
             `I ("(after-load \"DB.TABLE\" \"STATEMENT\")", "With $(b,copy), as $(b,--after-load).");
             `S Manpage.s_examples;
             `Pre
               "(define (phone p) (concat \"+7 (9\" (digits p 2) \") \" (digits (concat p \"/\") 3) \"-\" (right p 4)))\n\
                (words names \"names.txt\")\n\
                (strict \"shop.users\")\n\
                (keep \"shop.users.id\")\n\
                (mask \"shop.users.phone\" (phone value))\n\
                (mask \"shop.users.name\" (concat (pick-word value names) \" \" (shape (after value \" \"))))\n\
                (mask \"shop.users.born\" (keyed-date (str (row id)) (make-date (year value) 1 1) (make-date (year value) 12 31)))\n\
                (mask (where (table \"shop.*\") (column \"*email*\") (type text)) (email value))\n\
                (mask (where (table \"shop.users\") (type binary)) (hash-bytes value))";
           ]
    in
    Cmd.v
      (Cmd.info "mask" ~doc:"Try an expression of the masking language on values, with no server" ~man)
      Term.(const run $ kind $ row $ scripts $ expression $ values $ mask_secret)
  in
  let documented = [ dump_cmd; load_cmd; copy_cmd; mirror_cmd; agent_cmd; collect_cmd; relay_cmd; mask_cmd ] in
  let help name =
    let buffer = Buffer.create 8192 in
    let formatter = Format.formatter_of_buffer buffer in
    let (_ : (int Cmdliner.Cmd.eval_ok, Cmdliner.Cmd.eval_error) result) =
      Cmdliner.Cmd.eval_value ~help:formatter ~err:formatter ~env:(fun _ -> None)
        ~argv:[| "quiver"; name; "--help=plain" |]
        (Cmdliner.Cmd.group (Cmdliner.Cmd.info "quiver" ~version:Version.number) documented)
    in
    Format.pp_print_flush formatter ();
    Quiver_docs.Docs.command ~name ~help:(Buffer.contents buffer)
  in
  let docs_cmd =
    let open Cmdliner in
    let run dir =
      let commands = List.map (fun command -> help (Cmd.name command)) documented in
      match
        let%bind () =
          match List.concat_map (fun command -> List.map (fun line -> command.Quiver_docs.Docs.name ^ ": " ^ line) (Quiver_docs.Docs.undocumented command)) commands with
          | [] -> Ok ()
          | lines -> Error ("options with no description:\n  " ^ String.concat "\n  " lines)
        in
        Base.List.fold_result
          (Quiver_docs.Docs.site ~version:Version.number ~commands)
          ~init:() ~f:(fun () (path, content) ->
            let file = Filename.concat dir path in
            match
              let () = if not (Sys.file_exists dir) then Sys.mkdir dir 0o755 in
              Out_channel.with_open_bin file (fun channel -> Out_channel.output_string channel content)
            with
            | () -> Ok ()
            | exception Sys_error message -> Error message)
      with
      | Ok () ->
          Format.printf "documentation of quiver %s in %s@." Version.number dir;
          0
      | Error message -> told message
    in
    Cmd.v
      (Cmd.info "docs"
         ~doc:
           "Write quiver's documentation as a website into a directory: every command's options and the \
            masking language, from this binary itself")
      Term.(
        const run
        $ Arg.(
            required
            & pos 0 (some string) None
            & info [] ~docv:"DIR" ~doc:"Where the pages go; made when missing."))
  in
  let commands = documented @ [ docs_cmd ] in
  let given = Array.to_list Sys.argv in
  let chosen, argv =
    let rec go acc = function
      | "--quiverfile" :: path :: rest -> (Some path, List.rev_append acc rest)
      | arg :: rest when String.starts_with ~prefix:"--quiverfile=" arg ->
          (Some (String.sub arg 13 (String.length arg - 13)), List.rev_append acc rest)
      | arg :: rest -> go (arg :: acc) rest
      | [] -> (None, List.rev acc)
    in
    go [] given
  in
  let path =
    match (chosen, Sys.getenv_opt "QUIVERFILE") with
    | Some path, _ | None, Some path -> Some path
    | None, None -> if Sys.file_exists "quiverfile.yaml" then Some "quiverfile.yaml" else None
  in
  let settled =
    match (path, argv) with
    | None, _ | _, ([] | [ _ ]) -> Ok (argv, [])
    | Some path, program :: command :: rest -> (
        let read =
          match In_channel.with_open_bin path In_channel.input_all with
          | text -> Ok text
          | exception Sys_error message -> Error message
        in
        match read with
        | Error message -> Error ("quiverfile: " ^ message)
        | Ok text -> (
            match
              Quiver_quiverfile.Quiverfile.of_string ~file:path
                ~commands:
                  (List.filter (fun name -> not (Hashtbl.mem options name)) (List.map Cmdliner.Cmd.name commands))
                ~scoped:[ "database" ] text
            with
            | Error (`Quiverfile error) ->
                Error
                  ("quiverfile " ^ path ^ ": "
                  ^
                  match error with
                  | Unreadable message -> message
                  | Not_settings where -> where ^ " is not a map of options and commands"
                  | Unusable name -> name ^ " takes a value or a list of values"
                  | Scoped name ->
                      name ^ " means different things to different commands: put it under the command's name")
            | Ok file ->
                Format.eprintf "quiver: settings from %s@." path;
                let accepted = lazy (Quiver_docs.Docs.options (help command)) in
                Base.List.fold_result (Quiver_quiverfile.Quiverfile.settings file ~command)
                  ~init:(program :: command :: rest, []) ~f:(fun (argv, variables) (name, value) ->
                    match (Hashtbl.find_opt options name, value) with
                    | None, _ -> Error ("quiverfile " ^ path ^ ": quiver has no option --" ^ name)
                    | Some (_, variable), Text text -> Ok (argv, (variable, text) :: variables)
                    | Some _, Texts _ when not (List.mem name (Lazy.force accepted)) -> Ok (argv, variables)
                    | Some (names, _), Texts texts ->
                        let spelled name = if String.length name = 1 then "-" ^ name else "--" ^ name in
                        let mentioned arg =
                          List.exists
                            (fun name ->
                              String.equal arg (spelled name) || String.starts_with ~prefix:(spelled name ^ "=") arg
                              || (String.length name = 1 && String.starts_with ~prefix:(spelled name) arg))
                            names
                        in
                        if List.exists mentioned argv then Ok (argv, variables)
                        else Ok (argv @ List.map (fun text -> "--" ^ name ^ "=" ^ text) texts, variables))))
  in
  match settled with
  | Error message -> exit (told message)
  | Ok (argv, variables) ->
      let code =
        Cmdliner.Cmd.eval' ~argv:(Array.of_list argv)
           ~env:(fun variable ->
             match Sys.getenv_opt variable with
             | Some text -> Some text
             | None -> List.assoc_opt variable variables)
           (Cmdliner.Cmd.group
              (Cmdliner.Cmd.info "quiver" ~version:Version.number
                 ~doc:"Parallel consistent logical database dumper"
                 ~man:
                   [
                     `S "QUIVERFILE";
                     `P
                       "Defaults for any option can live in a YAML file: $(b,--quiverfile) FILE (before or after the \
                        command), the $(b,QUIVERFILE) variable, or $(b,quiverfile.yaml) in the current directory. \
                        Keys are long option names; those at the top hold for every command, those under a \
                        command's name ($(b,dump:), $(b,copy:), …) for it alone; a list gives an option once per \
                        item. The command line and each option's environment variable win over the file.";
                   ])
              commands)
      in
      if code = Cmdliner.Cmd.Exit.cli_error && not (List.is_empty variables) then
        Option.iter
          (fun path -> Format.eprintf "quiver: the values in %s are read as the QUIVER_* variables they name@." path)
          path;
      exit code
