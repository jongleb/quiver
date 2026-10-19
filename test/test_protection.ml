open Base.Result.Let_syntax

(** A certificate of the fixtures in [tls/]. *)
type identity =
  | Node  (** Issued by [ca] for localhost and 127.0.0.1. *)
  | Elsewhere  (** Issued by [ca] for another host. *)
  | Lone  (** Self-signed, for localhost and 127.0.0.1. *)
  | Stranger  (** Issued by [stranger-ca] for localhost and 127.0.0.1. *)
[@@deriving enumerate]

(** The certificates a side trusts. *)
type peers = Ca | Lone_only | Stranger_ca [@@deriving enumerate]

type side = { identity : identity; peers : peers }

type scenario = {
  server : side;
  client : side;
  upload : int;  (** Bytes the client sends before it finishes. *)
  download : int;  (** Bytes the server answers with once it read them. *)
}

let identity_file = function
  | Node -> "node"
  | Elsewhere -> "elsewhere"
  | Lone -> "lone"
  | Stranger -> "stranger"

let peers_file = function
  | Ca -> "ca"
  | Lone_only -> "lone"
  | Stranger_ca -> "stranger-ca"

let scenario =
  QCheck2.Gen.(
    let side =
      let+ identity = oneof_list all_of_identity
      and+ peers = oneof_list all_of_peers in
      { identity; peers }
    in
    let size = oneof [ int_range 0 64; int_range 0 300_000 ] in
    let+ server = side
    and+ client = side
    and+ upload = size
    and+ download = size in
    { server; client; upload; download })

let print { server; client; upload; download } =
  let side { identity; peers } =
    Printf.sprintf "%s trusting %s" (identity_file identity) (peers_file peers)
  in
  Printf.sprintf "server %s, client %s, %d bytes up, %d down" (side server)
    (side client) upload download

let trusts peers identity =
  match (peers, identity) with
  | Ca, (Node | Elsewhere) | Lone_only, Lone | Stranger_ca, Stranger -> `Trusted
  | (Ca | Lone_only | Stranger_ca), (Node | Elsewhere | Lone | Stranger) ->
      `Untrusted

let exchange =
  QCheck2.Test.make ~count:80
    ~name:
      "TLS carries bytes both ways, changing keys as told, and half-closes \
       exactly when each side trusts the other's certificate and it names the \
       host dialled"
    ~print scenario (fun { server; client; upload; download } ->
      let up = String.init upload (fun n -> Char.chr (n land 255))
      and down = String.init download (fun n -> Char.chr (n * 7 land 255)) in
      let protection { identity; peers } =
        Support.protection ~node:(identity_file identity)
          ~peers:(peers_file peers)
      in
      let take_all flow =
        let buffer = Bytes.create 65536 and got = Buffer.create 1024 in
        let rec go () =
          match Quiver_net.Protection.read flow buffer 0 (Bytes.length buffer) with
          | Ok 0 -> Ok (Buffer.contents got)
          | Ok count ->
              Buffer.add_subbytes got buffer 0 count;
              go ()
          | Error _ as error -> error
        in
        go ()
      in
      let send flow text =
        let%map () =
          Quiver_net.Protection.write flow text 0 (String.length text)
        in
        Quiver_net.Protection.shutdown flow
      in
      let rekey_bytes = 4096 in
      let server_updates = Atomic.make 0 and client_updates = Atomic.make 0 in
      let listening = Unix.socket PF_INET SOCK_STREAM 0 in
      Unix.setsockopt listening SO_REUSEADDR true;
      Unix.bind listening Support.loopback;
      Unix.listen listening 4;
      let exchange_seconds = 5. in
      match
        Support.within exchange_seconds @@ fun () ->
        Support.both
          (fun () ->
            let socket, _ = Unix.accept listening in
            match Quiver_net.Protection.accepted (protection server) socket with
            | Error _ -> `Refused
            | Ok flow -> (
                Quiver_net.Protection.rekey_after flow rekey_bytes;
                match
                  let%bind got = take_all flow in
                  let%map () = send flow down in
                  got
                with
                | Ok got ->
                    Atomic.set server_updates
                      (Quiver_net.Protection.key_updates flow);
                    Quiver_net.Protection.close flow;
                    `Got got
                | Error _ ->
                    Quiver_net.Protection.close flow;
                    `Failed))
          (fun () ->
            let socket = Unix.socket PF_INET SOCK_STREAM 0 in
            Unix.connect socket (Unix.getsockname listening);
            match
              Quiver_net.Protection.connected (protection client)
                ~host:"127.0.0.1" socket
            with
            | Error (`Protection (Quiver_net.Protection.Untrusted _)) ->
                `Untrusted
            | Error (`Protection _) -> `Refused
            | Ok flow -> (
                Quiver_net.Protection.rekey_after flow rekey_bytes;
                match
                  let%bind () = send flow up in
                  take_all flow
                with
                | Ok got ->
                    Atomic.set client_updates
                      (Quiver_net.Protection.key_updates flow);
                    Quiver_net.Protection.close flow;
                    `Got got
                | Error _ ->
                    Quiver_net.Protection.close flow;
                    `Failed))
      with
      | Error `Timeout ->
          Unix.close listening;
          false
      | Ok (served, answered) -> (
          Unix.close listening;
          let client_trusts =
            match (trusts client.peers server.identity, server.identity) with
            | `Trusted, (Node | Lone | Stranger) -> `Trusted
            | `Trusted, Elsewhere | `Untrusted, _ -> `Untrusted
          in
          match (trusts server.peers client.identity, client_trusts) with
          | `Trusted, `Trusted ->
              served = `Got up && answered = `Got down
              &&
              let changed bytes updates =
                let several_writes = 2 * 65536 in
                (updates = 0 || bytes > rekey_bytes)
                && (bytes <= several_writes || updates > 0)
              in
              changed upload (Atomic.get client_updates)
              && changed download (Atomic.get server_updates)
          | `Trusted, `Untrusted | `Untrusted, `Untrusted -> (
              answered = `Untrusted
              && match served with `Got _ -> false | _ -> true)
          | `Untrusted, `Trusted -> (
              served = `Refused
              && match answered with `Got _ -> false | _ -> true)))

let mismatched =
  let run ~agent ~collector =
    let listening = Result.get_ok (Quiver_net.Channel.listen Support.loopback) in
    let mismatch_seconds = 2. in
    let outcome =
      Support.within mismatch_seconds @@ fun () ->
      Support.both
        (fun () -> Result.is_ok (Quiver_net.Collector.accept listening collector ~reconnect:Support.reconnect))
        (fun () ->
          Result.is_ok
            (Quiver_net.Remote.create agent
               {
                 host = "127.0.0.1";
                 address = Quiver_net.Channel.listening_address listening;
               }
               (Support.settings ~streams:1 ~chunks:1 ())))
    in
    Quiver_net.Channel.stop listening;
    outcome
  in
  let tls () = Support.protection ~node:"node" ~peers:"ca" in
  List.map
    (fun (name, agent, collector) ->
      Alcotest.test_case name `Quick (fun () ->
          Alcotest.(check (result (pair bool bool) reject))
            "both sides fail"
            (Ok (false, false))
            (Result.map_error ignore
               (run ~agent:(agent ()) ~collector:(collector ())))))
    [
      ( "a TLS agent and a plaintext collector both fail",
        tls,
        fun () -> Quiver_net.Protection.plaintext );
      ( "a plaintext agent and a TLS collector both fail",
        (fun () -> Quiver_net.Protection.plaintext),
        tls );
    ]

let spliced =
  QCheck2.Test.make ~count:100
    ~name:
      "a splice passes on every frame both ways, those read ahead with the \
       first message too, small and larger than a read, plain or under TLS"
    ~print:(fun (wire, up, down) ->
      let sizes frames =
        String.concat "," (List.map (fun (size, _) -> string_of_int size) frames)
      in
      Printf.sprintf "%s, up %s, down %s"
        (match wire with `Plain -> "plaintext" | `Tls -> "mutual TLS")
        (sizes up) (sizes down))
    QCheck2.Gen.(
      let frame =
        pair (oneof [ int_range 0 100; int_range 0 200_000 ]) (int_range 1 255)
      in
      let frames = list_size (int_range 0 12) frame in
      triple (oneof_list [ `Plain; `Tls ]) frames frames)
    (fun (wire, up, down) ->
      let text (size, step) =
        String.init size (fun n -> Char.chr (n * step land 255))
      in
      let up = List.map text up and down = List.map text down in
      let protection =
        match wire with
        | `Plain -> Quiver_net.Protection.plaintext
        | `Tls -> Support.protection ~node:"node" ~peers:"ca"
      in
      let listen () = Result.get_ok (Quiver_net.Channel.listen Support.loopback) in
      let relaying = listen () and receiving = listen () in
      let connect listening =
        Result.get_ok
          (Quiver_net.Channel.connect protection
             {
               host = "127.0.0.1";
               address = Quiver_net.Channel.listening_address listening;
             })
      and accept listening =
        Result.get_ok (Quiver_net.Channel.accept listening protection)
      in
      let splice_seconds = 5. in
      let outcome =
        Support.within splice_seconds @@ fun () ->
        let agent, from_agent =
          Support.both (fun () -> connect relaying) (fun () -> accept relaying)
        in
        let to_collector, collector =
          Support.both (fun () -> connect receiving) (fun () -> accept receiving)
        in
        let send channel texts =
          List.iter
            (fun text ->
              ignore (Quiver_net.Channel.send channel (Sexplib0.Sexp.Atom text)))
            texts;
          Quiver_net.Channel.finish channel
        in
        let rec all channel got =
          match Quiver_net.Channel.receive channel Sexplib0.Sexp_conv.string_of_sexp with
          | Ok text -> all channel (text :: got)
          | Error _ -> List.rev got
        in
        let exchanged channel texts () =
          Support.spawn (fun () -> send channel texts);
          all channel []
        in
        ignore
          (Quiver_net.Channel.send agent
             (Quiver_net.Message.sexp_of_attach (Data { session = "s" })));
        let (attached, spliced), (at_agent, at_collector) =
          Support.both
            (fun () ->
              let attached =
                Quiver_net.Channel.receive from_agent
                  Quiver_net.Message.attach_of_sexp
              in
              ( Result.is_ok attached,
                Result.is_ok
                  (Quiver_net.Channel.splice from_agent to_collector) ))
            (fun () ->
              Support.both (exchanged agent up) (exchanged collector down))
        in
        let result = (attached, spliced, at_collector, at_agent) in
        List.iter Quiver_net.Channel.close [ agent; from_agent; to_collector; collector ];
        result
      in
      Quiver_net.Channel.stop relaying;
      Quiver_net.Channel.stop receiving;
      match outcome with
      | Error `Timeout -> false
      | Ok (attached, spliced, at_collector, at_agent) ->
          attached && spliced
          && List.equal String.equal at_collector up
          && List.equal String.equal at_agent down)

let trusted =
  Alcotest.test_case "a client loads the CA file it names and no other" `Quick
    (fun () ->
      Alcotest.(check (list bool))
        "loaded" [ true; true; false ]
        (List.map
           (fun trust -> Result.is_ok (Quiver_openssl.Tls.client trust))
           [
             Quiver_openssl.Tls.System_roots;
             Ca_file "tls/ca.crt";
             Ca_file "tls/missing.crt";
           ]))

let stopped =
  Alcotest.test_case
    "a stopped listener's waiting accept ends, and never takes a connection \
     of the listener opened next"
    `Quick (fun () ->
      let plaintext = Quiver_net.Protection.plaintext in
      let rounds = 30 in
      let taken =
        List.init rounds (fun _ ->
            let old = Result.get_ok (Quiver_net.Channel.listen Support.loopback) in
            let waiting =
              Support.start (fun () -> Quiver_net.Channel.accept old plaintext)
            in
            Thread.delay 0.005;
            Quiver_net.Channel.stop old;
            let next = Result.get_ok (Quiver_net.Channel.listen Support.loopback) in
            let client =
              Result.get_ok
                (Quiver_net.Channel.connect plaintext
                   {
                     host = "127.0.0.1";
                     address = Quiver_net.Channel.listening_address next;
                   })
            in
            let stale = Support.join waiting in
            let accept_seconds = 1. in
            let accepted =
              Support.within accept_seconds (fun () ->
                  Quiver_net.Channel.accept next plaintext)
            in
            Quiver_net.Channel.close client;
            Quiver_net.Channel.stop next;
            Result.iter Quiver_net.Channel.close stale;
            match (stale, accepted) with
            | Error (`Channel Quiver_net.Channel.Closed), Ok (Ok channel) ->
                Quiver_net.Channel.close channel;
                `Kept
            | _ -> `Taken)
      in
      Alcotest.(check int)
        "connections taken by a stopped listener" 0
        (Base.List.count taken ~f:(fun round -> round = `Taken)))

let tests =
  stopped :: trusted :: mismatched
  @ List.map QCheck_alcotest.to_alcotest [ exchange; spliced ]
