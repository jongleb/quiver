(** How a fake server answers the client's handshake response. *)
type server_auth =
  | Accepts  (** At once, with an OK packet. *)
  | Wants_full of string
      (** Asks for full [caching_sha2_password] authentication, then accepts
          only this password. *)
  | Wants_sealed of { password : string; public_key : string }
      (** Switches to [sha256_password], hands out [public_key] when asked,
          then accepts only this password sealed under it. *)
  | Switches_short  (** Switches plugin with a 5-byte challenge. *)
  | Accepts_without_lenenc
      (** Greets as ProxySQL, without [CLIENT_PLUGIN_AUTH_LENENC_CLIENT_DATA],
          and accepts a response whose auth data has a one-byte length. *)

(** Where the fake server listens. *)
type place = Over_tcp | Over_socket

let packet sequence payload =
  let length = String.length payload in
  let header = Bytes.create 4 in
  Bytes.set_uint8 header 0 (length land 0xff);
  Bytes.set_uint8 header 1 ((length lsr 8) land 0xff);
  Bytes.set_uint8 header 2 ((length lsr 16) land 0xff);
  Bytes.set_uint8 header 3 sequence;
  Bytes.to_string header ^ payload

let greeting ?(without = 0) plugin =
  let capabilities =
    (0x1 lor 0x4 lor 0x200 lor 0x2000 lor 0x8000 lor 0x20000 lor 0x40000
    lor 0x80000 lor 0x200000 lor 0x1000000)
    land lnot without
  in
  let buffer = Buffer.create 128 in
  Buffer.add_uint8 buffer 10;
  Buffer.add_string buffer "8.4.3\000";
  Buffer.add_int32_le buffer 7l;
  Buffer.add_string buffer "abcdefgh";
  Buffer.add_uint8 buffer 0;
  Buffer.add_uint16_le buffer (capabilities land 0xffff);
  Buffer.add_uint8 buffer 45;
  Buffer.add_uint16_le buffer 2;
  Buffer.add_uint16_le buffer (capabilities lsr 16);
  Buffer.add_uint8 buffer 21;
  Buffer.add_string buffer (String.make 10 '\000');
  Buffer.add_string buffer "ijklmnopqrst\000";
  Buffer.add_string buffer plugin;
  Buffer.add_char buffer '\000';
  packet 0 (Buffer.contents buffer)

let ok sequence = packet sequence "\000\000\000\002\000\000\000"

let rec exactly fd bytes off len =
  match len with
  | 0 -> Ok ()
  | _ -> (
      match Unix.read fd bytes off len with
      | 0 -> Error ()
      | count -> exactly fd bytes (off + count) (len - count)
      | exception Unix.Unix_error _ -> Error ())

let receive fd =
  let header = Bytes.create 4 in
  Result.bind (exactly fd header 0 4) (fun () ->
      let length =
        Bytes.get_uint8 header 0
        lor (Bytes.get_uint8 header 1 lsl 8)
        lor (Bytes.get_uint8 header 2 lsl 16)
      in
      let payload = Bytes.create length in
      Result.map (fun () -> Bytes.to_string payload) (exactly fd payload 0 length))

let send fd text =
  try ignore (Unix.write_substring fd text 0 (String.length text))
  with Unix.Unix_error _ -> ()

let serve listener auth =
  match Quiver_core.Blocking.wait ~seconds:2. Readable listener with
  | Timed_out -> `Nothing
  | Ready ->
      let client, _ = Unix.accept listener in
      send client
        (match auth with
        | Accepts | Switches_short -> greeting "mysql_native_password"
        | Accepts_without_lenenc ->
            greeting ~without:0x200000 "mysql_native_password"
        | Wants_full _ | Wants_sealed _ -> greeting "caching_sha2_password");
      let heard =
        match (receive client, auth) with
        | Error (), _ -> `Nothing
        | Ok _, Accepts ->
            send client (ok 2);
            `Accepted
        | Ok response, Accepts_without_lenenc -> (
            let fixed = 4 + 4 + 1 + 23 in
            let user_end = String.index_from response fixed '\000' in
            let length = Char.code response.[user_end + 1] in
            let plugin_at = user_end + 2 + length in
            match
              String.sub response plugin_at
                (String.length response - plugin_at)
            with
            | "mysql_native_password\000" when length = 20 ->
                send client (ok 2);
                `Accepted
            | _ | (exception Invalid_argument _) -> `Wrong_password)
        | Ok _, Switches_short ->
            send client (packet 2 "\254mysql_native_password\000short\000");
            `Switched
        | Ok _, Wants_full password -> (
            send client (packet 2 "\001\004");
            match receive client with
            | Ok sent when String.equal sent (password ^ "\000") ->
                send client (ok 4);
                `Password
            | Ok _ -> `Wrong_password
            | Error () -> `Nothing)
        | Ok _, Wants_sealed { password; public_key } -> (
            let scramble = "0123456789abcdefghij" in
            let opened sealed =
              let input = Filename.temp_file "quiver" ".sealed"
              and output = Filename.temp_file "quiver" ".opened" in
              Out_channel.with_open_bin input (fun channel ->
                  Out_channel.output_string channel sealed);
              let status =
                Sys.command
                  (Filename.quote_command "openssl"
                     [ "pkeyutl"; "-decrypt"; "-inkey"; "tls/rsa.key";
                       "-pkeyopt"; "rsa_padding_mode:oaep"; "-in"; input;
                       "-out"; output ]
                     ~stderr:Filename.null)
              in
              let text = In_channel.with_open_bin output In_channel.input_all in
              Sys.remove input;
              Sys.remove output;
              String.mapi
                (fun i c ->
                  Char.chr (Char.code c lxor Char.code scramble.[i mod 20]))
                (if status = 0 then text else "")
            in
            send client
              (packet 2 ("\254sha256_password\000" ^ scramble ^ "\000"));
            match receive client with
            | Ok "\001" -> (
                send client (packet 4 ("\001" ^ public_key));
                match receive client with
                | Ok sealed when String.equal (opened sealed) (password ^ "\000") ->
                    send client (ok 6);
                    `Password
                | Ok _ -> `Wrong_password
                | Error () -> `Nothing)
            | Ok _ -> `Wrong_password
            | Error () -> `Nothing)
      in
      Unix.close client;
      heard

let listening place =
  let listener, route =
    match place with
    | Over_tcp ->
        let listener = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
        Unix.setsockopt listener SO_REUSEADDR true;
        Unix.bind listener Support.loopback;
        ( listener,
          match Unix.getsockname listener with
          | ADDR_INET (_, port) ->
              Quiver_mysql.Link.Tcp { host = "127.0.0.1"; port; tls = Plaintext }
          | ADDR_UNIX _ -> Alcotest.fail "a TCP listener on a Unix address" )
    | Over_socket ->
        let path = Filename.temp_file "quiver" ".sock" in
        Sys.remove path;
        let listener = Unix.socket ~cloexec:true PF_UNIX SOCK_STREAM 0 in
        Unix.bind listener (ADDR_UNIX path);
        (listener, Quiver_mysql.Link.Socket (File_path.of_string path))
  in
  Unix.listen listener 1;
  (listener, route)

let connect place auth ~password =
  let listener, route = listening place in
  let heard = Support.start (fun () -> serve listener auth) in
  let connected =
    Quiver_mysql.Link.connect ~local_data:Without_local_data route
      ~timeouts:
        { connecting = Core.Time_ns.Span.of_int_sec 5; silence = Core.Time_ns.Span.of_int_sec 5 }
      ~user:"root" ~password
  in
  Result.iter Quiver_mysql.Link.close connected;
  let heard = Support.join heard in
  Unix.close listener;
  (match route with
  | Socket path -> Sys.remove (File_path.to_string path)
  | Tcp _ -> ());
  (connected, heard)

let handshake =
  Alcotest.test_case "the client authenticates over TCP and over a Unix socket"
    `Quick (fun () ->
      List.iter
        (fun place ->
          match connect place Accepts ~password:(Some "secret") with
          | Ok link, `Accepted ->
              Alcotest.(check bool)
                "MySQL 8.4" true
                (match Quiver_mysql.Link.server link with
                | Mysql_8_4 _ -> true
                | _ -> false)
          | (Ok _ | Error _), _ -> Alcotest.fail "no session")
        [ Over_tcp; Over_socket ])

let proxysql =
  Alcotest.test_case
    "a server without lenenc auth data, as ProxySQL, gets a one-byte length"
    `Quick (fun () ->
      Alcotest.(check bool)
        "accepted" true
        (match connect Over_tcp Accepts_without_lenenc ~password:(Some "secret") with
        | Ok _, `Accepted -> true
        | (Ok _ | Error _), _ -> false))

let cleartext =
  Alcotest.test_case
    "full caching_sha2 authentication sends the password over a Unix socket \
     and refuses to over TCP without TLS"
    `Quick (fun () ->
      Alcotest.(check bool)
        "over the socket" true
        (match connect Over_socket (Wants_full "secret") ~password:(Some "secret") with
        | Ok _, `Password -> true
        | (Ok _ | Error _), _ -> false);
      Alcotest.(check bool)
        "over TCP" true
        (match connect Over_tcp (Wants_full "secret") ~password:(Some "secret") with
        | Error (`Link Quiver_mysql.Link.Cleartext_refused), `Nothing -> true
        | (Ok _ | Error _), _ -> false))

let sealed =
  Alcotest.test_case
    "sha256_password seals the password under the server's key over a Unix \
     socket and refuses to send it over TCP without TLS"
    `Quick (fun () ->
      let public_key = In_channel.with_open_bin "tls/rsa.pub" In_channel.input_all in
      let password = "a password longer than the twenty-byte scramble" in
      let sealed = Wants_sealed { password; public_key } in
      Alcotest.(check bool)
        "over the socket" true
        (match connect Over_socket sealed ~password:(Some password) with
        | Ok _, `Password -> true
        | (Ok _ | Error _), _ -> false);
      Alcotest.(check bool)
        "under a key that is not one" true
        (match
           connect Over_socket
             (Wants_sealed { password; public_key = "not a key" })
             ~password:(Some password)
         with
        | Error (`Link Quiver_mysql.Link.Unusable_public_key), `Nothing -> true
        | (Ok _ | Error _), _ -> false);
      Alcotest.(check bool)
        "over TCP" true
        (match connect Over_tcp sealed ~password:(Some password) with
        | Error (`Link Quiver_mysql.Link.Cleartext_refused), `Nothing -> true
        | (Ok _ | Error _), _ -> false))

let unreachable =
  Alcotest.test_case "a Unix socket nobody listens on is unreachable" `Quick
    (fun () ->
      let path = Filename.temp_file "quiver" ".sock" in
      Sys.remove path;
      Alcotest.(check bool)
        "unreachable" true
        (match
           Quiver_mysql.Link.connect ~local_data:Without_local_data
             (Socket (File_path.of_string path))
             ~timeouts:
        { connecting = Core.Time_ns.Span.of_int_sec 5; silence = Core.Time_ns.Span.of_int_sec 5 }
             ~user:"root" ~password:None
         with
        | Error (`Transport (Quiver_mysql.Transport.Unreachable { endpoint = Socket _; _ })) ->
            true
        | Ok _ | Error _ -> false))

let short_scramble =
  Alcotest.test_case "a challenge under 20 bytes is an error, not an exception"
    `Quick (fun () ->
      Alcotest.(check bool)
        "short scramble" true
        (match connect Over_tcp Switches_short ~password:(Some "secret") with
        | Error (`Link Quiver_mysql.Link.Short_scramble), `Switched -> true
        | (Ok _ | Error _), _ -> false))

let timeouts =
  Alcotest.test_case
    "the connect timeout bounds a server silent before its greeting, and an \
     authenticated connection waits up to the net timeout"
    `Quick (fun () ->
      let timeouts =
        {
          Quiver_mysql.Link.connecting = Core.Time_ns.Span.of_int_ms 200;
          silence = Core.Time_ns.Span.of_int_sec 5;
        }
      in
      let connect route =
        Quiver_mysql.Link.connect ~local_data:Without_local_data route ~timeouts
          ~user:"root" ~password:None
      in
      let silent, route = listening Over_tcp in
      let started = Unix.gettimeofday () in
      let unanswered = connect route in
      let waited = Unix.gettimeofday () -. started in
      Unix.close silent;
      Alcotest.(check bool)
        "silent greeting" true
        (match unanswered with
        | Error (`Transport (Quiver_mysql.Transport.Timed_out span)) ->
            Core.Time_ns.Span.equal span timeouts.connecting && waited < 2.
        | Ok _ | Error _ -> false);
      let slow, route = listening Over_tcp in
      let server =
        Support.start (fun () ->
            let client, _ = Unix.accept slow in
            send client (greeting "mysql_native_password");
            let answered =
              Result.bind (receive client) (fun (_ : string) ->
                  send client (ok 2);
                  Result.map
                    (fun (_ : string) ->
                      Unix.sleepf 0.6;
                      send client (ok 1))
                    (receive client))
            in
            ignore (receive client);
            Unix.close client;
            answered)
      in
      let answered =
        Result.bind (connect route) (fun link ->
            let answered = Quiver_mysql.Link.query link "SELECT SLEEP(0.6)" in
            Quiver_mysql.Link.close link;
            answered)
      in
      ignore (Support.join server);
      Unix.close slow;
      Alcotest.(check bool)
        "slow query" true
        (match answered with Ok [] -> true | Ok _ | Error _ -> false))

let discovered =
  Alcotest.test_case
    "localhost's socket is the last socket in the client groups of the option \
     files, else MYSQL_UNIX_PORT, else the first packaged path that exists"
    `Quick (fun () ->
      let found ?(env = []) ?(files = []) ?(directories = []) existing =
        match
          Quiver_mysql.Local.socket
            {
              getenv = (fun name -> List.assoc_opt name env);
              read = (fun path -> List.assoc_opt path files);
              entries =
                (fun directory ->
                  Option.value ~default:[] (List.assoc_opt directory directories));
              exists = (fun path -> List.mem path existing);
            }
        with
        | Ok path -> Some (File_path.to_string path)
        | Error (`Local (Quiver_mysql.Local.No_socket _)) -> None
      in
      Alcotest.(check (list (option string)))
        "found"
        [
          Some "/srv/my.sock";
          Some "/var/lib/mysql/mysql.sock";
          Some "/run/mysqld/mysqld.sock";
          None;
          Some "/etc.sock";
          Some "/home.sock";
          Some "/srv/my.sock";
          Some "/conf.d/b.sock";
          Some "/included.sock";
          Some "/loop.sock";
          Some "/maria home.sock";
        ]
        [
          found ~env:[ ("MYSQL_UNIX_PORT", "/srv/my.sock") ] [ "/tmp/mysql.sock" ];
          found ~env:[ ("MYSQL_UNIX_PORT", "") ]
            [ "/tmp/mysql.sock"; "/var/lib/mysql/mysql.sock" ];
          found [ "/var/run/mysqld/mysqld.sock"; "/run/mysqld/mysqld.sock" ];
          found [];
          found
            ~env:[ ("MYSQL_UNIX_PORT", "/srv/my.sock") ]
            ~files:[ ("/etc/my.cnf", "[client]\nsocket = /etc.sock\n") ]
            [ "/tmp/mysql.sock" ];
          found
            ~env:[ ("HOME", "/home/me") ]
            ~files:
              [
                ("/etc/my.cnf", "[client]\nsocket = /etc.sock\n");
                ("/home/me/.my.cnf", "[mysqld]\nsocket=/server.sock\n[Client]\nloose_socket=\"/home.sock\" # mine\n");
              ]
            [];
          found
            ~env:[ ("MYSQL_UNIX_PORT", "/srv/my.sock") ]
            ~files:[ ("/etc/my.cnf", "socket = /nogroup.sock\n[mysqld]\nsocket = /server.sock\n[client]\nsocket =\n") ]
            [];
          found
            ~files:
              [
                ("/etc/mysql/my.cnf", "[client]\nsocket = /first.sock\n!includedir /conf.d\n");
                ("/conf.d/a.cnf", "[client-server]\nsocket = /conf.d/a.sock\n");
                ("/conf.d/b.cnf", "[client-mariadb]\nsocket = /conf.d/b.sock\n");
                ("/conf.d/c.txt", "[client]\nsocket = /conf.d/c.sock\n");
              ]
            ~directories:[ ("/conf.d", [ "c.txt"; "b.cnf"; "a.cnf" ]) ]
            [];
          found
            ~files:
              [
                ("/etc/my.cnf", "!include /more.cnf\n[client]\n");
                ("/more.cnf", "[client]\nsocket = /included.sock\n");
              ]
            [];
          found
            ~files:[ ("/etc/my.cnf", "[client]\nsocket = /loop.sock\n!include /etc/my.cnf\n") ]
            [];
          found
            ~env:[ ("MARIADB_HOME", "/maria"); ("MYSQL_HOME", "/mysql") ]
            ~files:
              [
                ("/maria/my.cnf", "[client]\nsocket = /maria\\shome.sock\n");
                ("/mysql/my.cnf", "[client]\nsocket = /mysql.sock\n");
              ]
            [];
        ])

let lossy_load =
  Alcotest.test_case
    "LOAD DATA LOCAL that stores other than the rows sent, or warns but of an \
     ENUM's error value, is an error with the server's warnings"
    `Quick (fun () ->
      let truncated column = Printf.sprintf "Data truncated for column '%s' at row 2" column in
      let loaded ?(said = [ truncated "a" ]) ?(enums = []) ~stored ~warned () =
        let listener, route = listening Over_tcp in
        let server =
          Support.start (fun () ->
              let client, _ = Unix.accept listener in
              send client (greeting "mysql_native_password");
              ignore (receive client);
              send client (ok 2);
              ignore (receive client);
              send client (packet 1 "\251file");
              let rec drain sequence =
                match receive client with
                | Ok "" | Error () -> sequence
                | Ok _ -> drain (sequence + 1)
              in
              let sequence = drain 3 in
              let counts = Bytes.create 2 in
              Bytes.set_uint16_le counts 0 warned;
              send client
                (packet sequence
                   ("\000" ^ String.make 1 (Char.chr stored) ^ "\000\002\000"
                  ^ Bytes.to_string counts));
              let lenenc text = String.make 1 (Char.chr (String.length text)) ^ text in
              let () =
                match receive client with
                | Ok _ ->
                    send client
                      (String.concat ""
                         ([ packet 1 "\003"; packet 2 "level"; packet 3 "code"; packet 4 "message" ]
                         @ List.mapi
                             (fun i message ->
                               packet (5 + i)
                                 (lenenc "Warning" ^ lenenc "1265" ^ lenenc message))
                             said
                         @ [ packet (5 + List.length said) "\254\000\000\002\000\000\000" ]));
                    ignore (receive client)
                | Error () -> ()
              in
              Unix.close client)
        in
        let data = ref "1\n2\n3\n" in
        let loaded =
          Result.bind
            (Quiver_mysql.Link.connect ~local_data:With_local_data route
               ~timeouts:
                 {
                   connecting = Core.Time_ns.Span.of_int_sec 5;
                   silence = Core.Time_ns.Span.of_int_sec 5;
                 }
               ~user:"root" ~password:None)
            (fun link ->
              let loaded =
                Quiver_mysql.Link.load_local link "LOAD DATA LOCAL INFILE 'f' INTO TABLE t"
                  ~rows:3
                  ~enums:
                    (List.map
                       (fun name -> Result.get_ok (Quiver_core.Ident.of_string name))
                       enums)
                  (fun buffer ~pos ~len:_ ->
                    let length = String.length !data in
                    Bytes.blit_string !data 0 buffer pos length;
                    data := "";
                    Ok length)
              in
              Quiver_mysql.Link.close link;
              loaded)
        in
        Support.join server;
        Unix.close listener;
        match loaded with
        | Ok () -> "stored"
        | Error (`Link error) ->
            Sexplib0.Sexp.to_string (Quiver_mysql.Link.sexp_of_error error)
        | Error (`Wire _ | `Transport _) -> "broken"
      in
      let lossy ?(column = "a") stored =
        Printf.sprintf
          "(Lossy_load(sent 3)(stored %d)(warnings(\"Warning 1265 Data truncated \
           for column '%s' at row 2\")))"
          stored column
      in
      Alcotest.(check (list string))
        "loaded"
        [ "stored"; lossy 2; lossy 3; lossy 4; "stored"; lossy 2; lossy ~column:"b" 3; lossy 3 ]
        [
          loaded ~stored:3 ~warned:0 ();
          loaded ~stored:2 ~warned:0 ();
          loaded ~stored:3 ~warned:1 ();
          loaded ~stored:4 ~warned:0 ();
          loaded ~enums:[ "a" ] ~stored:3 ~warned:1 ();
          loaded ~enums:[ "a" ] ~stored:2 ~warned:1 ();
          loaded ~enums:[ "a" ] ~said:[ truncated "a"; truncated "b" ] ~stored:3 ~warned:2 ();
          loaded ~enums:[ "a" ] ~stored:3 ~warned:2 ();
        ])

let result_warnings =
  Alcotest.test_case "a result's rows end with the warnings the server counted" `Quick (fun () ->
      let streamed warned =
        let listener, route = listening Over_tcp in
        let lenenc text = String.make 1 (Char.chr (String.length text)) ^ text in
        let definition =
          String.concat "" (List.map lenenc [ "def"; "db"; "t"; "t"; "v"; "v" ])
          ^ "\012\063\000\020\000\000\000\008\000\000\000\000\000"
        in
        let terminator =
          let counts = Bytes.create 2 in
          Bytes.set_uint16_le counts 0 warned;
          "\254\000\000\034\000" ^ Bytes.to_string counts
        in
        let server =
          Support.start (fun () ->
              let client, _ = Unix.accept listener in
              send client (greeting "mysql_native_password");
              ignore (receive client);
              send client (ok 2);
              ignore (receive client);
              send client
                (packet 1 "\000\001\000\000\000\001\000\000\000\000\000\000"
                ^ packet 2 definition);
              ignore (receive client);
              send client
                (String.concat ""
                   [
                     packet 1 "\001";
                     packet 2 definition;
                     packet 3 ("\000\000" ^ "\007\000\000\000\000\000\000\000");
                     packet 4 ("\000\000" ^ "\009\000\000\000\000\000\000\000");
                     packet 5 terminator;
                   ]);
              ignore (receive client);
              Unix.close client)
        in
        let streamed =
          Result.bind
            (Quiver_mysql.Link.connect ~local_data:Without_local_data route
               ~timeouts:
                 {
                   connecting = Core.Time_ns.Span.of_int_sec 5;
                   silence = Core.Time_ns.Span.of_int_sec 5;
                 }
               ~user:"root" ~password:None)
            (fun link ->
              let streamed =
                Result.bind (Quiver_mysql.Rows.open_ link "SELECT v") (fun rows ->
                    let rec drain count =
                      match Quiver_mysql.Rows.next rows ~row:(fun _ ~bitmap:_ -> Ok ()) with
                      | Ok Row -> drain (count + 1)
                      | Ok End ->
                          let warnings = Quiver_mysql.Rows.warnings rows in
                          Result.map (fun () -> (count, warnings)) (Quiver_mysql.Rows.close rows)
                      | Error _ as error -> error
                    in
                    drain 0)
              in
              Quiver_mysql.Link.close link;
              streamed)
        in
        Support.join server;
        Unix.close listener;
        match streamed with
        | Ok (rows, warnings) -> Printf.sprintf "%d rows, %d warnings" rows warnings
        | Error _ -> "broken"
      in
      Alcotest.(check (list string))
        "streamed"
        [ "2 rows, 0 warnings"; "2 rows, 1 warnings"; "2 rows, 300 warnings" ]
        (List.map streamed [ 0; 1; 300 ]))

let tests =
  [ handshake; proxysql; cleartext; sealed; short_scramble; unreachable; timeouts; lossy_load; result_warnings; discovered ]
