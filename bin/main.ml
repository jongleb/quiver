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
            (List.fold_left
               (fun total ({ state; _ } : Quiver_core.Manifest.entry) ->
                 match state with
                 | Done { part = { rows; _ }; _ } -> total + rows
                 | Pending -> total)
               0 manifest.chunks)
          ~chunks:(List.length manifest.chunks);
        Result.iter
          (function
            | Quiver_mysql.Provider.Locked { lock; lock_held; _ } ->
                Format.printf "%a held for %a@."
                  (fun ppf value ->
                    Sexplib0.Sexp.pp_hum ppf
                      (Quiver_mysql.Provider.sexp_of_lock value))
                  lock Core.Time_ns.Span.pp lock_held
            | Unlocked ->
                Format.printf "no lock: each session has its own snapshot@.")
          (Quiver_core.Codec.decode Quiver_mysql.Provider.position_of_yojson
             manifest.position);
        0
    | Error error ->
        Format.eprintf "quiver: %a@."
          (fun ppf value -> Sexplib0.Sexp.pp_hum ppf (Dump.sexp_of_error value))
          error;
        1
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
        Format.eprintf "quiver: %a@."
          (fun ppf -> function
            | `Compression error ->
                (fun ppf value ->
                  Sexplib0.Sexp.pp_hum ppf
                    (Quiver_sink.Compression.sexp_of_error value))
                  ppf error
            | `Output error ->
                (fun ppf value ->
                  Sexplib0.Sexp.pp_hum ppf
                    (Quiver_sink.Output.sexp_of_error value))
                  ppf error)
          error;
        1
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
  let agent reading protection collector level streams chunks =
    (
        (
            let started = Quiver_core.Stopwatch.start () in
            match
              let%bind peer = resolve collector in
              let%bind level = Quiver_sink.Compression.Level.of_int level in
              Quiver_net.Remote.create protection peer
                ~streams:(Option.value streams ~default:reading.opts.threads)
                ~chunks:
                  (Option.value chunks ~default:(2 * reading.opts.threads))
                level
            with
            | Error error ->
                Format.eprintf "quiver: %a@."
                  (fun ppf -> function
                    | `Msg message -> Format.pp_print_string ppf message
                    | `Compression error ->
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf
                            (Quiver_sink.Compression.sexp_of_error value))
                          ppf error
                    | `Sink error ->
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf
                            (Quiver_net.Remote.sexp_of_error value))
                          ppf error)
                  error;
                1
            | Ok sink ->
                let code =
                  read
                    (module Quiver_net.Remote)
                    sink reading
                    ~report:(fun ~rows ~chunks ->
                      Format.printf "sent %d rows in %d chunks to %s in %a@."
                        rows chunks collector Core.Time_ns.Span.pp
                        (Quiver_core.Stopwatch.elapsed started))
                in
                Quiver_net.Remote.shutdown sink;
                code))
  in
  let collect protection listen forward output target into keys binlog =
    (
        (
            match
              let%bind listening = listening listen in
              let%bind into =
                Option.fold ~none:(Ok Quiver_load.Load.Same_names)
                  ~some:(fun name ->
                    let%map name = Quiver_core.Ident.of_string name in
                    Quiver_load.Load.Into name)
                  into
              in
              let%bind session =
                Quiver_net.Collector.accept listening protection
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
              (listening, session, into, downstream)
            with
            | Error error ->
                Format.eprintf "quiver: %a@."
                  (fun ppf -> function
                    | `Msg message -> Format.pp_print_string ppf message
                    | `Ident error ->
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf
                            (Quiver_core.Ident.sexp_of_error value))
                          ppf error
                    | `Collector error ->
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf
                            (Quiver_net.Collector.sexp_of_error value))
                          ppf error)
                  error;
                1
            | Ok (listening, session, into, downstream) -> (
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
                      Format.eprintf "quiver: %a@."
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf
                            (Quiver_net.Collector.sexp_of_error value))
                        error;
                      1
                  | Error (`Sink error) ->
                      Format.eprintf "quiver: %a@."
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf (S.sexp_of_error value))
                        error;
                      1
                in
                match output with
                | Some output -> files output { consume = collected }
                | None -> (
                    match
                      Quiver_load.Copy.create target into ~keys ~binlog
                        ~sessions:(Quiver_net.Collector.chunks session)
                    with
                    | Error (`Sink error) ->
                        Format.eprintf "quiver: %a@."
                          (fun ppf value ->
                            Sexplib0.Sexp.pp_hum ppf
                              (Quiver_load.Copy.sexp_of_error value))
                          error;
                        1
                    | Ok sink ->
                        let code = collected (module Quiver_load.Copy) sink in
                        Quiver_load.Copy.shutdown sink;
                        code))))
  in
  let copy reading target into keys binlog target_threads =
    (
        (
            let started = Quiver_core.Stopwatch.start () in
            match
              let%bind into =
                Option.fold ~none:(Ok Quiver_load.Load.Same_names)
                  ~some:(fun name ->
                    let%map name = Quiver_core.Ident.of_string name in
                    Quiver_load.Load.Into name)
                  into
              in
              Quiver_load.Copy.create target into ~keys ~binlog
                ~sessions:
                  (Option.value target_threads ~default:reading.opts.threads)
            with
            | Error error ->
                Format.eprintf "quiver: %a@."
                  (fun ppf -> function
                    | `Ident error ->
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf
                            (Quiver_core.Ident.sexp_of_error value))
                          ppf error
                    | `Sink error ->
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf
                            (Quiver_load.Copy.sexp_of_error value))
                          ppf error)
                  error;
                1
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
        let pp out = function
          | `Load error ->
              (fun ppf value ->
                Sexplib0.Sexp.pp_hum ppf (Quiver_load.Load.sexp_of_error value))
                out error
          | `Manifest error ->
              (fun ppf value ->
                Sexplib0.Sexp.pp_hum ppf
                  (Quiver_core.Manifest.sexp_of_error value))
                out error
          | `Statements error ->
              (fun ppf value ->
                Sexplib0.Sexp.pp_hum ppf
                  (Quiver_load.Statements.sexp_of_error value))
                out error
          | `Ident error ->
              (fun ppf value ->
                Sexplib0.Sexp.pp_hum ppf (Quiver_core.Ident.sexp_of_error value))
                out error
          | #Quiver_load.Load.cause as cause ->
              (fun ppf value ->
                Sexplib0.Sexp.pp_hum ppf (Quiver_load.Load.sexp_of_cause value))
                out cause
        in
        let started = Quiver_core.Stopwatch.start () in
        match
          let%bind target =
            Option.fold ~none:(Ok Quiver_load.Load.Same_names)
              ~some:(fun name ->
                let%map name = Quiver_core.Ident.of_string name in
                Quiver_load.Load.Into name)
              into
          in
          Quiver_load.Load.run
            conn ~dir:(File_path.of_string dir) target ~keys ~binlog ~threads
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
            Format.eprintf "quiver: %a@." pp error;
            1)
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
              (enum [ ("verified", `Verified); ("disabled", `Disabled) ])
              `Verified
          & info (names [] "tls") ~docv:"MODE"
              ~doc:
                "$(b,verified) encrypts the connection and verifies the \
                 server: by the system's CAs (on 127.0.0.1 only the \
                 certificate's validity period), by $(b,--tls-ca) or by \
                 $(b,--tls-fingerprint); $(b,disabled) is for local test \
                 servers.")
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
        term_result
          (const (fun mode ca fingerprint ->
               match (mode, ca, fingerprint) with
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
                            ((fun value ->
                               Sexplib0.Sexp.to_string_hum
                                 (Quiver_mysql.Fingerprint.sexp_of_error value))
                               error)))
               | `Verified, Some _, Some _ ->
                   Error
                     (`Msg "--tls-ca and --tls-fingerprint exclude each other")
               | `Disabled, None, None -> Ok Quiver_mysql.Link.Plaintext
               | `Disabled, (Some _ | None), (Some _ | None) ->
                   Error
                     (`Msg
                        "--tls disabled takes neither --tls-ca nor \
                         --tls-fingerprint"))
          $ mode $ ca $ fingerprint))
    and host =
      Arg.(
        value & opt string "127.0.0.1"
        & info (names [ "h" ] "host") ~docv:"HOST")
    and port =
      Arg.(value & opt int 3306 & info (names [ "P" ] "port") ~docv:"PORT")
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
    and timeout =
      Arg.(
        value & opt int 600
        & info (names [] "net-timeout") ~docv:"SECONDS"
            ~doc:
              "Fail when a connection sends and receives nothing for so long. \
               A global read lock can wait for long queries, so keep it above \
               the longest one.")
    and zero_dates =
      match side with
      | `Target -> Term.const Quiver_mysql.Provider.Reject
      | `Source ->
          Arg.(
            value
            & opt
                (enum
                   [ ("reject", Quiver_mysql.Provider.Reject); ("keep", Keep) ])
                Quiver_mysql.Provider.Reject
            & info [ "zero-dates" ] ~docv:"MODE"
                ~doc:
                  "$(b,reject) fails on a zero or invalid date such as \
                   0000-00-00 or 2020-02-30, which has no exact value; \
                   $(b,keep) writes every date as stored, and restoring it \
                   needs a server that accepts it.")
    in
    Term.(
      const (fun tls host port user password seconds zero_dates ->
          {
            Quiver_mysql.Provider.tls;
            host;
            port;
            user;
            password;
            timeout = Core.Time_ns.Span.of_int_sec (Int.max 1 seconds);
            zero_dates;
          })
      $ tls $ host $ port $ user $ password $ timeout $ zero_dates)
  in
  let reading =
    let open Cmdliner in
    let mode =
      Arg.(
        value
        & opt
            (enum
               [
                 ("ftwrl", Quiver_mysql.Provider.Ftwrl);
                 ("backup-stage", Backup_stage);
                 ("trx-only", Trx_only);
               ])
            Quiver_mysql.Provider.Ftwrl
        & info [ "snapshot" ] ~docv:"MODE"
            ~doc:
              "$(b,ftwrl) and $(b,backup-stage) (MariaDB) give one consistent \
               snapshot; $(b,trx-only) takes no lock and is only best effort.")
    in
    let filter =
      let ident name =
        Result.map_error
          (fun (`Ident error) ->
            `Msg
              ((fun value ->
                 Sexplib0.Sexp.to_string_hum
                   (Quiver_core.Ident.sexp_of_error value))
                 error))
          (Quiver_core.Ident.of_string name)
      in
      Term.(
        const (fun databases tables ->
            match databases @ tables with
            | [] -> Quiver_core.Filter.Everything
            | first :: rest -> Only (first, rest))
        $ Arg.(
            value
            & opt_all
                (conv
                   ( (fun name ->
                       Result.map
                         (fun database -> Quiver_core.Filter.Database database)
                         (ident name)),
                     fun ppf value ->
                       Sexplib0.Sexp.pp_hum ppf
                         (Quiver_core.Filter.sexp_of_rule value) ))
                []
            & info [ "B"; "database" ] ~docv:"DB"
                ~doc:"Dump only $(docv); repeatable.")
        $ Arg.(
            value
            & opt_all
                (conv
                   ( (fun name ->
                       match Base.String.lsplit2 name ~on:'.' with
                       | None -> Error (`Msg "expected DB.TABLE")
                       | Some (database, table) ->
                           let%map database = ident database
                           and table = ident table in
                           Quiver_core.Filter.Table { database; table }),
                     fun ppf value ->
                       Sexplib0.Sexp.pp_hum ppf
                         (Quiver_core.Filter.sexp_of_rule value) ))
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
                 `Msg
                   ((fun value ->
                      Sexplib0.Sexp.to_string_hum
                        (Quiver_core.Error.sexp_of_t value))
                      error))
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
        $ conn `Source $ mode $ filter $ threads $ render_threads $ chunk_bytes
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
            (enum
               [
                 ("rfc4180", Quiver_core.Output_format.Rfc4180);
                 ("mysql-load-data", Mysql_load_data);
               ])
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
          (enum
             [
               ("after-data", Quiver_load.Load.After_data);
               ("with-table", With_table);
             ])
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
          (enum
             [ ("write", Quiver_load.Load.Write_binlog); ("skip", Skip_binlog) ])
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
        & opt (some string) None
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
      Term.(const load $ conn `Source $ dir $ into $ keys $ binlog $ threads)
  in
  let copy_cmd =
    let open Cmdliner in
    let into =
      Arg.(
        value
        & opt (some string) None
        & info [ "to-database" ] ~docv:"DB"
            ~doc:"Copy the only database read under this name.")
    in
    Cmd.v
      (Cmd.info "copy"
         ~doc:
           "Copy MySQL or MariaDB into another server, with no files: each \
            reading session's chunks stream into a loading session of its own")
      Term.(
        const copy $ reading $ conn `Target $ into $ keys $ binlog
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
                       ((fun value ->
                          Sexplib0.Sexp.to_string_hum
                            (Quiver_net.Protection.sexp_of_error value))
                          error))
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
        value & opt int 3
        & info [ "zstd-level" ] ~docv:"LEVEL"
            ~doc:"Compression of batches on the wire.")
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
            value
            & opt (some int) None
            & info [ "chunks-in-flight" ] ~docv:"N"
                ~doc:
                  "Chunks begun and not yet committed by the collector; \
                   defaults to twice $(b,--threads), so readers go on while \
                   commits travel back. The collector loads a server with as \
                   many sessions."))
  in
  let listen =
    Cmdliner.Arg.(
      required
      & opt (some string) None
      & info [ "listen" ] ~docv:"HOST:PORT"
          ~doc:"Where to wait for the agent or the hop before.")
  in
  let relay protection listen next =
    (match
              let%bind next = resolve next in
              let%bind listening = listening listen in
              let%bind session =
                Quiver_net.Collector.accept listening protection
              in
              let%bind downstream =
                Quiver_net.Collector.forward next session
              in
              Quiver_net.Collector.relay listening session downstream
            with
            | Ok () ->
                Format.printf "relayed@.";
                0
            | Error error ->
                Format.eprintf "quiver: %a@."
                  (fun ppf -> function
                    | `Msg message -> Format.pp_print_string ppf message
                    | `Collector error ->
                        (fun ppf value ->
                          Sexplib0.Sexp.pp_hum ppf
                            (Quiver_net.Collector.sexp_of_error value))
                          ppf error)
                  error;
                1)
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
        const relay $ wire $ listen
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
        & opt (some string) None
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
          (fun protection listen forward dir output target into keys binlog ->
            collect protection listen forward (Option.map output dir) target
              into keys binlog)
        $ wire $ listen
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
