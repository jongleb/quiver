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
  opts : Quiver_core.Dump_opts.t;
}
(** What a dump reads and how. *)

type 'r consumer = {
  consume :
    ('s : value mod contended portable).
    (module Quiver_core.Sink_intf.S with type t = 's) -> 's -> 'r;
}
(** What runs with a sink of any kind. *)

let () =
  let told message =
    Format.eprintf "quiver: %s@." message;
    1
  and failed sexp =
    Format.eprintf "quiver: %a@." Sexplib0.Sexp.pp_hum sexp;
    1
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
  let read (type (sink : value mod contended portable))
      (module Sink : Quiver_core.Sink_intf.S with type t = sink) (sink : sink)
      { conn; mode; filter; opts } ~report =
    let module Dump = Quiver_core.Dump.Make (Quiver_mysql.Provider) (Sink) in
    let result =
      match mode with
      | Ftwrl | Backup_stage ->
          Dump.run
            { conn; mode; level = Snapshot; filter; opts }
            sink
      | Trx_only ->
          Dump.run
            { conn; mode; level = Best_effort; filter; opts }
            sink
    in
    match result with
    | Ok (manifest : Quiver_core.Manifest.t) ->
        report
          ~rows:
            (Base.List.sum
               (module Base.Int)
               manifest.chunks
               ~f:(fun ({ state; _ } : Quiver_core.Manifest.entry) ->
                 match state with
                 | Done { part = { rows; _ }; _ } -> rows
                 | Pending -> 0))
          ~chunks:(List.length manifest.chunks);
        Result.iter
          (function
            | Quiver_mysql.Provider.Locked { lock; lock_held; _ } ->
                Format.printf "%a held for %a@." Sexplib0.Sexp.pp_hum
                  (Quiver_mysql.Provider.sexp_of_lock lock)
                  Core.Time_ns.Span.pp lock_held
            | Unlocked ->
                Format.printf "no lock: each session has its own snapshot@.")
          (Quiver_core.Codec.decode Quiver_mysql.Provider.position_of_yojson
             manifest.position);
        0
    | Error error -> failed (Dump.sexp_of_error error)
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
                read sink_module sink reading ~report:(fun ~rows ~chunks ->
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
  let agent reading protection collector level streams most_streams chunks
      rate traffic reconnect =
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
                Option.value streams ~default:reading.opts.threads
              in
              let%map sink =
                Quiver_net.Remote.create protection peer
                  {
                    streams;
                    most_streams;
                    chunks =
                      Option.value chunks ~default:(2 * reading.opts.threads);
                    compression;
                    pace =
                      Option.map
                        (fun megabytes -> Float.to_int (megabytes *. 1e6))
                        rate;
                    traffic;
                    reconnect = Core.Time_ns.Span.of_sec reconnect;
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
                let code =
                  read
                    (module Quiver_net.Remote)
                    sink reading
                    ~report:(fun ~rows ~chunks ->
                      Format.printf
                        "sent %d rows in %d chunks to %s in %a, %d streams at \
                         the end@."
                        rows chunks collector Core.Time_ns.Span.pp
                        (Quiver_core.Stopwatch.elapsed started)
                        (Quiver_net.Remote.streams sink))
                in
                Quiver_net.Remote.shutdown sink;
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
  let collect protection reconnect listen forward output to_server into keys
      binlog =
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
                let collected (type (s : value mod contended portable))
                    (module S : Quiver_core.Sink_intf.S with type t = s)
                    (sink : s) =
                  let module Collector = Quiver_net.Collector.Make (S) in
                  match
                    Collector.run ?forward:downstream listening session sink
                  with
                  | Ok () ->
                      Format.printf "collected in %a@." Core.Time_ns.Span.pp
                        (Quiver_core.Stopwatch.elapsed started);
                      0
                  | Error (`Collector error) ->
                      failed (Quiver_net.Collector.sexp_of_error error)
                  | Error (`Sink error) -> failed (S.sexp_of_error error)
                in
                match destination with
                | `Files output -> files output { consume = collected }
                | `Server to_server -> (
                    match
                      Quiver_load.Copy.create to_server (target into) ~keys ~binlog
                        ~sessions:(Quiver_net.Collector.chunks session)
                    with
                    | Error (`Sink error) -> failed (Quiver_load.Copy.sexp_of_error error)
                    | Ok sink ->
                        let code = collected (module Quiver_load.Copy) sink in
                        Quiver_load.Copy.shutdown sink;
                        code)))
  in
  let copy reading to_server into keys binlog target_threads =
    (
        (
            let started = Quiver_core.Stopwatch.start () in
            match
              Quiver_load.Copy.create to_server (target into) ~keys ~binlog
                ~sessions:
                  (Option.value target_threads ~default:reading.opts.threads)
            with
            | Error (`Sink error) -> failed (Quiver_load.Copy.sexp_of_error error)
            | Ok sink ->
                let code =
                  read
                    (module Quiver_load.Copy)
                    sink reading
                    ~report:(fun ~rows ~chunks ->
                      Format.printf "copied %d rows in %d chunks in %a@." rows
                        chunks Core.Time_ns.Span.pp
                        (Quiver_core.Stopwatch.elapsed started))
                in
                Quiver_load.Copy.shutdown sink;
                code))
  in
  let load conn dir into keys binlog threads =
    (
        let started = Quiver_core.Stopwatch.start () in
        match
          Quiver_load.Load.run
            conn ~dir:(File_path.of_string dir) (target into) ~keys ~binlog ~threads
        with
        | Ok { files; rows; bytes; skipped } ->
            Format.printf "loaded %d rows (%d bytes) from %d files in %a@." rows
              bytes files Core.Time_ns.Span.pp
              (Quiver_core.Stopwatch.elapsed started);
            (match skipped with
            | 0 -> ()
            | n ->
                Format.printf
                  "resumed: %d files were loaded by an earlier run@." n);
            0
        | Error error ->
            failed
              ([%sexp_of:
                 [ `Load of Quiver_load.Load.error
                 | `Manifest of Quiver_core.Manifest.error
                 | `Statements of Quiver_load.Statements.error
                 | `Ident of Quiver_core.Ident.error
                 | Quiver_load.Load.cause ]]
                 error))
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
          & info (names [] "tls") ~docv:"MODE"
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
          & info (names [] "tls-ca") ~docv:"FILE"
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
        & info (names [ "h" ] "host") ~docv:"HOST"
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
        & info (names [ "P" ] "port") ~docv:"PORT"
            ~doc:"The server's TCP port; 3306 unless given.")
    and socket =
      Arg.(
        value
        & opt (some string) None
        & info (names [ "S" ] "socket") ~docv:"PATH"
            ~doc:
              "Connect over this Unix socket instead of TCP, as to a server \
               started with $(b,skip_networking); there is no TLS on it, and \
               it takes no $(b,--host), $(b,--port) or $(b,--tls) options.")
    and user =
      Arg.(value & opt string "root" & info (names [ "u" ] "user") ~docv:"USER")
    and password =
      Arg.(
        value
        & opt (some string) None
        & info (names [ "p" ] "password") ~docv:"PASSWORD"
            ~env:
              (Cmd.Env.info
                 (match side with
                 | `Source -> "MYSQL_PWD"
                 | `Target -> "QUIVER_TO_PWD")))
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
            & info (names [] "connect-timeout") ~docv:"SECONDS"
                ~doc:
                  "Fail when connecting, TLS or authentication waits for the \
                   server so long.")
        $ Arg.(
            value & opt int 600
            & info (names [] "net-timeout") ~docv:"SECONDS"
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
            & info [ "zero-dates" ] ~docv:"MODE"
                ~doc:
                  "$(b,reject) fails on a zero or invalid date such as \
                   0000-00-00 or 2020-02-30, which has no exact value; \
                   $(b,keep) writes every date as stored, and restoring it \
                   needs a server that accepts it.")
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
    Term.(
      const (fun route user password timeouts zero_dates ->
          Result.map
            (fun route ->
              { Quiver_mysql.Provider.route; user; password; timeouts; zero_dates })
            route)
      $ route $ user $ password $ timeouts $ zero_dates)
  in
  let server side =
    Cmdliner.Term.(term_result (const (Result.map warned) $ conn side))
  in
  let reading =
    let open Cmdliner in
    let mode =
      Arg.(
        value
        & opt
            (choices Quiver_mysql.Provider.string_of_snapshot_mode
               Quiver_mysql.Provider.all_of_snapshot_mode)
            Quiver_mysql.Provider.Ftwrl
        & info [ "snapshot" ] ~docv:"MODE"
            ~doc:
              "$(b,ftwrl) and $(b,backup-stage) (MariaDB) give one consistent \
               snapshot; $(b,trx-only) takes no lock and is only best effort.")
    in
    let filter =
      Term.(
        const (fun databases tables ->
            match databases @ tables with
            | [] -> Quiver_core.Filter.Everything
            | first :: rest -> Only (first, rest))
        $ Arg.(
            value
            & opt_all
                (conv
                   ( (fun text ->
                       Result.map
                         (fun database -> Quiver_core.Filter.Database database)
                         (ident_of_string text)),
                     fun ppf rule ->
                       Sexplib0.Sexp.pp_hum ppf
                         (Quiver_core.Filter.sexp_of_rule rule) ))
                []
            & info [ "B"; "database" ] ~docv:"DB"
                ~doc:"Dump only $(docv); repeatable.")
        $ Arg.(
            value
            & opt_all
                (conv
                   ( (fun text ->
                       match Base.String.lsplit2 text ~on:'.' with
                       | None -> Error (`Msg "expected DB.TABLE")
                       | Some (database, table) ->
                           let%map database = ident_of_string database
                           and table = ident_of_string table in
                           Quiver_core.Filter.Table { database; table }),
                     fun ppf rule ->
                       Sexplib0.Sexp.pp_hum ppf
                         (Quiver_core.Filter.sexp_of_rule rule) ))
                []
            & info [ "T"; "table" ] ~docv:"DB.TABLE"
                ~doc:"Dump only $(docv); repeatable."))
    in
    let threads =
      Arg.(
        value & opt int 4
        & info [ "t"; "threads" ] ~docv:"N"
            ~doc:"Database sessions reading in parallel, one domain each.")
    in
    let render_threads =
      Arg.(
        value
        & opt (some int) None
        & info [ "render-threads" ] ~docv:"N"
            ~doc:
              "Domains turning batches into output; defaults to half the \
               readers, within the cores left after them.")
    in
    let chunk_bytes =
      Arg.(
        value
        & opt int (64 * 1024 * 1024)
        & info [ "chunk-bytes" ] ~docv:"BYTES" ~doc:"Target size of one chunk.")
    in
    let batch_bytes =
      Arg.(value & opt int (256 * 1024) & info [ "batch-bytes" ] ~docv:"BYTES")
    in
    Term.(
      term_result
        (const
           (fun
             conn mode filter threads render_threads chunk_bytes batch_bytes ->
             Result.map_error
               (fun error ->
                 `Msg (Sexplib0.Sexp.to_string_hum (Quiver_core.Error.sexp_of_t error)))
               (let%bind plan = Quiver_core.Plan_opts.make ~chunk_bytes in
                let%map opts =
                  Quiver_core.Dump_opts.make ~plan ~threads
                    ~render_threads:
                      (Option.value render_threads
                         ~default:
                           (Int.max 1
                              (Int.min (threads / 2)
                                 (Domain.recommended_domain_count () - threads))))
                    ~batch_bytes
                in
                { conn; mode; filter; opts }))
        $ server `Source $ mode $ filter $ threads $ render_threads $ chunk_bytes
        $ batch_bytes))
  in
  let output =
    let open Cmdliner in
    let format =
      Arg.(
        value
        & opt (enum [ ("sql", `Sql); ("csv", `Csv) ]) `Sql
        & info [ "format" ] ~docv:"FORMAT")
    and dialect =
      Arg.(
        value
        & opt
            (choices Quiver_core.Output_format.string_of_dialect
               Quiver_core.Output_format.all_of_dialect)
            Quiver_core.Output_format.Rfc4180
        & info [ "csv-dialect" ] ~docv:"DIALECT")
    and compress =
      Arg.(
        value
        & opt (enum [ ("none", `None); ("zstd", `Zstd) ]) `None
        & info [ "compress" ] ~docv:"CODEC")
    and level = Arg.(value & opt int 3 & info [ "zstd-level" ] ~docv:"LEVEL")
    and statement_bytes =
      Arg.(
        value & opt int 1_000_000
        & info [ "statement-bytes" ] ~docv:"BYTES"
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
      & info [ "o"; "outputdir" ] ~docv:"DIR" ~doc)
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
      & info [ "secondary-keys" ] ~docv:"WHEN"
          ~doc:
            "$(b,with-table) creates tables as dumped; $(b,after-data) creates \
             them with their primary key only and adds secondary and foreign \
             keys once a table's data is in, so InnoDB builds each index by \
             sorting, which pays off when indexes outgrow memory.")
  in
  let binlog =
    let open Cmdliner in
    Arg.(
      value
      & opt
          (choices Quiver_load.Load.string_of_binlog Quiver_load.Load.all_of_binlog)
          Quiver_load.Load.Write_binlog
      & info [ "binlog" ] ~docv:"MODE"
          ~doc:
            "$(b,skip) keeps the load out of the server's binlog (less work \
             for the server, but replicas never see the data; needs the right \
             to set sql_log_bin); $(b,write) logs it as usual.")
  in
  let load_cmd =
    let open Cmdliner in
    let dir =
      Arg.(
        required
        & opt (some dir) None
        & info [ "d"; "directory" ] ~docv:"DIR" ~doc:"The dump to load.")
    and into =
      Arg.(
        value
        & opt (some ident) None
        & info [ "B"; "database" ] ~docv:"DB"
            ~doc:"Load the dump's only database under this name.")
    and threads =
      Arg.(
        value & opt int 4
        & info [ "t"; "threads" ] ~docv:"N"
            ~doc:"Sessions loading in parallel, one domain each.")
    in
    Cmd.v
      (Cmd.info "load" ~doc:"Load a quiver dump into MySQL or MariaDB")
      Term.(const load $ server `Source $ dir $ into $ keys $ binlog $ threads)
  in
  let copy_cmd =
    let open Cmdliner in
    let into =
      Arg.(
        value
        & opt (some ident) None
        & info [ "to-database" ] ~docv:"DB"
            ~doc:"Copy the only database read under this name.")
    in
    Cmd.v
      (Cmd.info "copy"
         ~doc:
           "Copy MySQL or MariaDB into another server, with no files: each \
            reading session's chunks stream into a loading session of its own")
      Term.(
        const copy $ reading $ server `Target $ into $ keys $ binlog
        $ Arg.(
            value
            & opt (some int) None
            & info [ "target-threads" ] ~docv:"N"
                ~doc:
                  "Sessions loading into the target, one chunk each; defaults \
                   to $(b,--threads). Readers wait for a free one, so loading \
                   at its own pace never stalls rendering."))
  in
  let wire =
    let open Cmdliner in
    let file name ~doc =
      Arg.(value & opt (some file) None & info [ name ] ~docv:"FILE" ~doc)
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
            & info [ "wire-tls" ] ~docv:"MODE"
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
      & info [ "reconnect" ] ~docv:"SECONDS"
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
        & info [ "collector" ] ~docv:"HOST:PORT"
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
        & info [ "zstd-level" ] ~docv:"LEVEL"
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
            & info [ "streams" ] ~docv:"N"
                ~doc:
                  "Data connections; defaults to $(b,--threads). Every \
                   connection carries batches of any chunk, so more of them \
                   than readers fill a long or lossy path.")
        $ Arg.(
            value & opt int 64
            & info [ "most-streams" ] ~docv:"N"
                ~doc:
                  "While the network holds the dump back, open more data \
                   connections, up to so many, keeping those that carry \
                   more; as many as $(b,--streams) keeps their number.")
        $ Arg.(
            value
            & opt (some int) None
            & info [ "chunks-in-flight" ] ~docv:"N"
                ~doc:
                  "Chunks begun and not yet committed by the collector; \
                   defaults to twice $(b,--threads), so readers go on while \
                   commits travel back. The collector loads a server with as \
                   many sessions.")
        $ Arg.(
            value
            & opt (some float) None
            & info [ "max-rate" ] ~docv:"MB/S"
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
            & info [ "dscp" ] ~docv:"N"
                ~doc:
                  "The DSCP every packet of the dump carries; 8 (CS1) asks \
                   routers to let other traffic go first.")
        $ reconnect)
  in
  let listen =
    Cmdliner.Arg.(
      required
      & opt (some string) None
      & info [ "listen" ] ~docv:"HOST:PORT"
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
            & info [ "forward" ] ~docv:"HOST:PORT" ~doc:"The next hop."))
  in
  let collect_cmd =
    let open Cmdliner in
    let into =
      Arg.(
        value
        & opt (some ident) None
        & info [ "to-database" ] ~docv:"DB"
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
          (fun protection reconnect listen forward dir output target into keys
               binlog ->
            collect protection reconnect listen forward (Option.map output dir)
              target into keys binlog)
        $ wire $ reconnect $ listen
        $ Arg.(
            value
            & opt (some string) None
            & info [ "forward" ] ~docv:"HOST:PORT"
                ~doc:
                  "Also pass everything on to this collector as it arrives, so \
                   one read of the source reaches a chain of targets; a chunk \
                   counts once every target committed it.")
        $ Arg.value
            (outputdir ~doc:"Write files here instead of loading into a server.")
        $ output $ conn `Target $ into $ keys $ binlog)
  in
  exit
    (Cmdliner.Cmd.eval'
       (Cmdliner.Cmd.group
          (Cmdliner.Cmd.info "quiver" ~version:Version.number
             ~doc:"Parallel consistent logical database dumper")
          [ dump_cmd; load_cmd; copy_cmd; agent_cmd; collect_cmd; relay_cmd ]))
