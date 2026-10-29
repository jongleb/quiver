open Quiver_core

let ident = Support.ident

let table n =
  Support.get_ok
    (Schema.Table.make ~generated:[]
       ~name:{ database = ident "db"; table = ident ("t" ^ string_of_int n) }
       ~fields:[ { name = ident "id"; ltype = Int64; nullability = Not_null } ]
       ~key:No_key ~estimate:{ rows = 0; bytes = 0 })

(** A sink that takes whatever reaches it. *)
module Anything = struct
  type error = Never

  let sexp_of_error Never = Sexplib0.Sexp.Atom "never"

  type t = unit
  type part = unit
  type renderer = unit
  type rendered = unit

  let format () = Output_format.Sql
  let database () _ ~ddl:_ = Ok ()
  let table () _ ~ddl:_ ~parts:_ ~order:_ = Ok ()
  let add_part () _ = Ok ()
  let objects () _ _ = Ok ()
  let part () _ ~index:_ = Ok ()
  let renderer () = ()
  let render () _ = Ok ()
  let append () () = Ok ()

  let close () answer =
    answer
      (Ok
         {
           Manifest.file = File_path.Part.of_string "part";
           rows = 0;
           bytes = 0;
           stored_bytes = 0;
           checksum = Xxh3_64 0L;
         })

  let discard () = ()
  let finish () ~source:_ ~tables:_ ~manifest:_ = Ok ()
end

module Collecting = Quiver_net.Collector.Make (Anything)

(** What a hostile peer writes, in order. *)
type action =
  | Message of string  (** Its text, on the control connection. *)
  | Batch of { stream : int; chunk : int; seq : int; rows : int; plain : bool }
      (** A well-formed batch frame of [rows] ids. *)
  | Garbled of { stream : int; chunk : int; seq : int; bytes : string }
      (** A frame whose checks pass around a payload that is no batch. *)
  | Noise of { stream : int option; bytes : string }
      (** Raw bytes, on a data connection or the control one. *)

let frame ~chunk ~seq kind codec payload ~raw =
  let buffer = Quiver_core.Byte_buffer.create () in
  Quiver_net.Frame.start buffer;
  Quiver_core.Byte_buffer.append_string buffer payload;
  Quiver_net.Frame.seal buffer kind codec ~raw ~chunk;
  let bytes = Bytes.sub buffer.bs 0 buffer.len in
  Quiver_net.Frame.number bytes ~seq;
  Bytes.to_string bytes

let message text =
  frame ~chunk:0 ~seq:0 Message Plain text ~raw:(String.length text)

let batch ~chunk ~seq ~rows ~plain =
  let batch = Batch.create [ Int64 ] in
  for row = 1 to rows do
    (let view = borrow_ batch in
     Support.add_int64 (Batch.columns view) (Int64.of_int row));
    Batch.commit_row (borrow_ batch)
  done;
  let encoded = Quiver_core.Byte_buffer.create () in
  (match Batch.seal batch with
  | Ok sealed -> Batch.encode sealed encoded
  | Error _ -> ());
  match plain with
  | true ->
      frame ~chunk ~seq Batch Plain
        (Quiver_core.Byte_buffer.contents encoded)
        ~raw:encoded.len
  | false ->
      let packed = Quiver_core.Byte_buffer.create () in
      ignore
        (Quiver_sink.Compression.zstd
           (Result.get_ok (Quiver_sink.Compression.Level.of_int 1))
           encoded ~into:packed);
      frame ~chunk ~seq Batch Zstd
        (Quiver_core.Byte_buffer.contents packed)
        ~raw:encoded.len

let controls =
  QCheck2.Gen.(
    let chunk = int_range 0 4 and n = int_range 0 3 in
    oneof
      [
        map
          (fun n ->
            Quiver_net.Message.sexp_of_control
              (Table { table = table n; ddl = ""; parts = 1 + n; order = Key_order }))
          n;
        pure
          (Quiver_net.Message.sexp_of_control
             (Database { name = ident "db"; ddl = "" }));
        map2
          (fun chunk n ->
            Quiver_net.Message.sexp_of_control
              (Begin { chunk; table = (table n).name; index = n }))
          chunk n;
        map2
          (fun chunk frames ->
            Quiver_net.Message.sexp_of_control (End { chunk; frames }))
          chunk (int_range 0 4);
        map
          (fun chunk -> Quiver_net.Message.sexp_of_control (Abort { chunk }))
          chunk;
        map
          (fun manifest ->
            Quiver_net.Message.sexp_of_control
              (Finish { source = ""; tables = []; manifest }))
          (oneof_list [ "null"; "[]"; {|{"chunks":[]}|}; "{" ]);
        pure
          (Quiver_net.Message.sexp_of_control
             (Hello
                {
                  session = "s";
                  byte_order = Quiver_net.Message.byte_order;
                  streams = 1;
                  chunks = 1;
                }));
        pure (Sexplib0.Sexp.List [ Atom "Nonsense" ]);
      ])

let actions streams =
  QCheck2.Gen.(
    let stream = int_range 0 (streams - 1)
    and chunk = int_range 0 5
    and seq = int_range 0 5 in
    list_size (int_range 0 25)
      (oneof_weighted
         [
           ( 4,
             map
               (fun sexp -> Message (Sexplib0.Sexp.to_string sexp))
               controls );
           (1, map (fun text -> Message text) (oneof_list [ "("; ")("; "" ]));
           ( 4,
             map
               (fun (((stream, chunk), (seq, rows)), plain) ->
                 Batch { stream; chunk; seq; rows; plain })
               (pair
                  (pair (pair stream chunk) (pair seq (int_range 0 30)))
                  bool) );
           ( 1,
             map
               (fun ((stream, chunk), (seq, bytes)) ->
                 Garbled { stream; chunk; seq; bytes })
               (pair (pair stream chunk)
                  (pair seq (string_size (int_range 0 40)))) );
           ( 1,
             map
               (fun (stream, bytes) -> Noise { stream; bytes })
               (pair (option stream) (string_size (int_range 1 40))) );
         ]))

let hostile_agent =
  QCheck2.Test.make ~count:100
    ~name:
      "a collector ends without raising once a hostile agent hangs up, \
       whatever it sent"
    ~print:(fun (streams, chunks, actions) ->
      Printf.sprintf "%d streams, %d chunks: %s" streams chunks
        (String.concat "; "
           (List.map
              (function
                | Message text -> text
                | Batch { stream; chunk; seq; rows; plain } ->
                    Printf.sprintf "batch s%d c%d #%d %d rows%s" stream chunk
                      seq rows
                      (match plain with true -> " plain" | false -> "")
                | Garbled { stream; chunk; seq; bytes } ->
                    Printf.sprintf "garbled s%d c%d #%d %S" stream chunk seq
                      bytes
                | Noise { stream; bytes } ->
                    Printf.sprintf "noise %s %S"
                      (Option.fold ~none:"control" ~some:(Printf.sprintf "s%d")
                         stream)
                      bytes)
              actions)))
    QCheck2.Gen.(
      let* streams = int_range 1 3 and* chunks = int_range 1 4 in
      let+ actions = actions streams in
      (streams, chunks, actions))
    (fun (streams, chunks, actions) ->
      let listening =
        Result.get_ok (Quiver_net.Channel.listen Support.loopback)
      in
      let address = Quiver_net.Channel.listening_address listening in
      let session_seconds = 2. in
      let outcome =
        Support.within session_seconds @@ fun () ->
        let collected =
          Support.start (fun () ->
              (match
                 match
                   Quiver_net.Collector.accept listening
                     Quiver_net.Protection.plaintext ~reconnect:Support.reconnect
                 with
                 | Error _ -> ()
                 | Ok session -> ignore (Collecting.run listening session () ~columns:Written)
               with
              | () -> `Ended
              | exception failure -> `Raised (Printexc.to_string failure)))
        in
        let connect () =
          let socket = Unix.socket PF_INET SOCK_STREAM 0 in
          Unix.connect socket address;
          socket
        in
        let write socket text =
          let rec go off =
            if off < String.length text then
              match
                Unix.write_substring socket text off (String.length text - off)
              with
              | count -> go (off + count)
              | exception Unix.Unix_error _ -> ()
          in
          go 0
        in
        let control = connect () in
        write control
          (message
             (Sexplib0.Sexp.to_string
                (Quiver_net.Message.sexp_of_control
                   (Hello
                      {
                        session = "s";
                        byte_order = Quiver_net.Message.byte_order;
                        streams;
                        chunks;
                      }))));
        Support.spawn (fun () ->
            let buffer = Bytes.create 4096 in
            let rec drain () =
              match Unix.read control buffer 0 (Bytes.length buffer) with
              | 0 -> ()
              | _ -> drain ()
              | exception Unix.Unix_error _ -> ()
            in
            drain ());
        let data =
          Array.init streams (fun _ ->
              let socket = connect () in
              write socket
                (message
                   (Sexplib0.Sexp.to_string
                      (Quiver_net.Message.sexp_of_attach (Data { session = "s" }))));
              socket)
        in
        List.iter
          (function
            | Message text -> write control (message text)
            | Batch { stream; chunk; seq; rows; plain } ->
                write data.(stream) (batch ~chunk ~seq ~rows ~plain)
            | Garbled { stream; chunk; seq; bytes } ->
                write data.(stream)
                  (frame ~chunk ~seq Batch Plain bytes
                     ~raw:(String.length bytes))
            | Noise { stream; bytes } ->
                write
                  (Option.fold ~none:control ~some:(Array.get data) stream)
                  bytes)
          actions;
        Array.iter
          (fun socket ->
            match Unix.shutdown socket SHUTDOWN_ALL with
            | () -> ()
            | exception Unix.Unix_error _ -> ())
          (Array.append [| control |] data);
        Support.join collected
      in
      Quiver_net.Channel.stop listening;
      match outcome with
      | Error `Timeout -> false
      | Ok `Ended -> true
      | Ok (`Raised failure) -> Alcotest.failf "the collector raised %s" failure)

let replies =
  QCheck2.Gen.(
    list_size (int_range 0 12)
      (oneof
         [
           pure (message (Sexplib0.Sexp.to_string (Quiver_net.Message.sexp_of_reply (Ready { format = Sql; columns = Written }))));
           pure (message (Sexplib0.Sexp.to_string (Quiver_net.Message.sexp_of_reply Done)));
           pure (message (Sexplib0.Sexp.to_string (Quiver_net.Message.sexp_of_reply Skipped)));
           pure (message (Sexplib0.Sexp.to_string (Quiver_net.Message.sexp_of_reply (Refused "no"))));
           map
             (fun chunk ->
               message @@ Sexplib0.Sexp.to_string
                 (Quiver_net.Message.sexp_of_reply
                    (Committed
                       {
                         chunk;
                         part =
                           {
                             file = File_path.Part.of_string "part";
                             rows = 0;
                             bytes = 0;
                             stored_bytes = 0;
                             checksum = Xxh3_64 0L;
                           };
                       })))
             (int_range 0 5);
           map
             (fun chunk ->
               message
                 (Sexplib0.Sexp.to_string
                    (Quiver_net.Message.sexp_of_reply
                       (Rejected { chunk; reason = "no" }))))
             (int_range 0 5);
           string_size (int_range 1 40);
         ]))

let hostile_collector =
  QCheck2.Test.make ~count:100
    ~name:
      "an agent ends every part and returns without raising once a hostile \
       collector hangs up, whatever it answered"
    ~print:(fun (streams, parts, replies) ->
      Printf.sprintf "%d streams, %d parts, %d replies" streams parts
        (List.length replies))
    QCheck2.Gen.(triple (int_range 1 3) (int_range 0 5) replies)
    (fun (streams, parts, replies) ->
      let listening = Unix.socket PF_INET SOCK_STREAM 0 in
      Unix.setsockopt listening SO_REUSEADDR true;
      Unix.bind listening Support.loopback;
      Unix.listen listening 16;
      let accept ~wait =
        match Quiver_core.Blocking.wait ~seconds:wait Readable listening with
        | Timed_out -> None
        | Ready -> (
            match Unix.accept listening with
            | socket, _ -> Some socket
            | exception Unix.Unix_error _ -> None)
      in
      let address = Unix.getsockname listening in
      let session_seconds = 2. in
      let stage = Atomic.make "connecting"
      and ended = Atomic.make 0
      and closed = Atomic.make 0 in
      let outcome =
        Support.within session_seconds @@ fun () ->
        Support.spawn (fun () ->
            let rec control attempts =
              match (accept ~wait:1., attempts) with
              | Some socket, _ -> Some socket
              | None, 1 -> None
              | None, _ -> control (attempts - 1)
            in
            let opened =
              Option.fold ~none:[]
                ~some:(fun control ->
                  List.iter
                    (fun reply ->
                      match
                        Unix.write_substring control reply 0
                          (String.length reply)
                      with
                      | _ -> ()
                      | exception Unix.Unix_error _ -> ())
                    replies;
                  control
                  :: List.filter_map
                       (fun _ -> accept ~wait:0.02)
                       (List.init streams Fun.id))
                (control 3)
            in
            List.iter
              (fun socket ->
                match Unix.close socket with
                | () -> ()
                | exception Unix.Unix_error _ -> ())
              (listening :: opened));
        match
          Quiver_net.Remote.create Quiver_net.Protection.plaintext
            { host = "127.0.0.1"; address }
            (Support.settings ~streams ~chunks:2 ())
        with
        | exception failure -> `Raised (Printexc.to_string failure)
        | Error _ -> `Ended
        | Ok agent -> (
            match
              Atomic.set stage "declaring the database";
              ignore (Quiver_net.Remote.database agent (ident "db") ~ddl:"");
              Atomic.set stage "declaring the table";
              ignore (Quiver_net.Remote.table agent (table 0) ~ddl:"" ~parts ~order:Key_order);
              for index = 0 to parts - 1 do
                Atomic.set stage (Printf.sprintf "opening part %d" index);
                match Quiver_net.Remote.part agent (table 0) ~index with
                | Error _ -> ()
                | Ok part ->
                    Atomic.incr closed;
                    Atomic.set stage (Printf.sprintf "closing part %d" index);
                    Quiver_net.Remote.close part (fun _ -> Atomic.incr ended)
              done;
              Atomic.set stage "finishing";
              ignore
                (Quiver_net.Remote.finish agent ~source:"" ~tables:[]
                   ~manifest:`Null);
              Atomic.set stage "waiting for the parts to end";
              let rec settled () =
                match Atomic.get ended >= Atomic.get closed with
                | true -> ()
                | false ->
                    Thread.yield ();
                    settled ()
              in
              settled ();
              Atomic.set stage "shutting down";
              Quiver_net.Remote.shutdown agent
            with
            | () -> `Ended
            | exception failure -> `Raised (Printexc.to_string failure))
      in
      match outcome with
      | Error `Timeout ->
          QCheck2.Test.fail_reportf "the agent hung while %s (%d of %d parts ended)"
            (Atomic.get stage) (Atomic.get ended) (Atomic.get closed)
      | Ok `Ended -> true
      | Ok (`Raised failure) -> Alcotest.failf "the agent raised %s" failure)

let vanishing_hop =
  Alcotest.test_case
    "a node and its agent fail, not hang, when the next hop hangs up while \
     the node waits for its answer to the finish"
    `Quick (fun () ->
      let plaintext = Quiver_net.Protection.plaintext in
      let listen () =
        Result.get_ok (Quiver_net.Channel.listen Support.loopback)
      in
      let collecting = listen () and next = listen () in
      let peer listening =
        {
          Quiver_net.Channel.host = "127.0.0.1";
          address = Quiver_net.Channel.listening_address listening;
        }
      and streams = 2
      and session_seconds = 3. in
      let outcome =
        Support.within session_seconds @@ fun () ->
        let hop =
          Support.start (fun () ->
              match Quiver_net.Channel.accept next plaintext with
              | Error _ -> ()
              | Ok control ->
                  let request () =
                    ignore
                      (Quiver_net.Channel.receive control
                         Quiver_net.Message.control_of_sexp)
                  in
                  request ();
                  ignore
                    (Quiver_net.Channel.send control
                       (Quiver_net.Message.sexp_of_reply (Ready { format = Sql; columns = Written })));
                  let data =
                    List.filter_map
                      (fun _ ->
                        Result.to_option
                          (Quiver_net.Channel.accept next plaintext))
                      (List.init streams Fun.id)
                  in
                  request ();
                  request ();
                  List.iter Quiver_net.Channel.close (control :: data))
        and node =
          Support.start (fun () ->
              match Quiver_net.Collector.accept collecting plaintext
                      ~reconnect:Support.reconnect with
              | Error _ -> `Unaccepted
              | Ok session -> (
                  match Quiver_net.Collector.forward (peer next) session with
                  | Error _ -> `Unforwarded
                  | Ok forward -> (
                      match Collecting.run ~forward collecting session () ~columns:Written with
                      | Ok () -> `Finished
                      | Error _ -> `Failed)))
        in
        let agent =
          Result.get_ok
            (Quiver_net.Remote.create plaintext (peer collecting) (Support.settings ~streams ~chunks:1 ()))
        in
        ignore (Quiver_net.Remote.database agent (ident "db") ~ddl:"");
        let finished =
          Quiver_net.Remote.finish agent ~source:"" ~tables:[] ~manifest:`Null
        in
        Quiver_net.Remote.shutdown agent;
        Support.join hop;
        (Result.is_ok finished, Support.join node)
      in
      Quiver_net.Channel.stop collecting;
      Quiver_net.Channel.stop next;
      Alcotest.(check bool)
        "the agent and the node fail in time" true
        (match outcome with Ok (false, `Failed) -> true | _ -> false))

let tests =
  vanishing_hop
  :: List.map QCheck_alcotest.to_alcotest [ hostile_agent; hostile_collector ]
