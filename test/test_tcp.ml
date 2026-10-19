external probe : Unix.file_descr -> int array = "quiver_tcp_probe"
external probe_tos : Unix.file_descr -> int = "quiver_tcp_probe_tos"

let tests =
  [
    Alcotest.test_case
      "a tuned socket bounds unsent bytes, finds a silent peer and gives up on \
       unacknowledged data"
      `Quick (fun () ->
        let listening = Unix.socket PF_INET SOCK_STREAM 0 in
        Unix.bind listening Support.loopback;
        Unix.listen listening 1;
        let connecting = Unix.socket PF_INET SOCK_STREAM 0 in
        Unix.connect connecting (Unix.getsockname listening);
        let accepted, _ = Unix.accept listening in
        let settings =
          List.map
            (fun socket ->
              Quiver_core.Tcp.tune socket;
              Array.to_list (probe socket))
            [ connecting; accepted ]
        in
        List.iter Unix.close [ accepted; connecting; listening ];
        List.iter
          (Alcotest.(check (list int))
             "nodelay, unsent, keepalive, idle, interval, probes, unacknowledged"
             [ 1; 512 * 1024; 1; 30; 10; 6; 120 ])
          settings);
      Alcotest.test_case
      "the kernel tells a connection's round trip and send buffer limit"
      `Quick (fun () ->
        let listening = Unix.socket PF_INET SOCK_STREAM 0 in
        Unix.bind listening Support.loopback;
        Unix.listen listening 1;
        let connecting = Unix.socket PF_INET SOCK_STREAM 0 in
        Unix.connect connecting (Unix.getsockname listening);
        let accepted, _ = Unix.accept listening in
        ignore (Unix.write_substring connecting "ping" 0 4);
        ignore (Unix.read accepted (Bytes.create 4) 0 4);
        let path = Quiver_core.Tcp.path connecting in
        List.iter Unix.close [ accepted; connecting; listening ];
        match path with
        | None -> Alcotest.fail "no path on loopback"
        | Some { rtt; send_buffer } ->
            Alcotest.(check bool)
              "a round trip under a second and a buffer of at least 64 KiB" true
              (Core.Time_ns.Span.(rtt >= zero && rtt < of_sec 1.)
              && send_buffer >= 65536));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:200
         ~name:"a connection carries at most a send buffer per round trip"
         QCheck2.Gen.(
           pair
             (oneof [ return 0; int_range 1 1_000_000 ])
             (int_range 1 (1 lsl 30)))
         (fun (rtt_us, send_buffer) ->
           let ceiling =
             Quiver_core.Tcp.ceiling
               { rtt = Core.Time_ns.Span.of_int_us rtt_us; send_buffer }
           in
           match (rtt_us, ceiling) with
           | 0, None -> true
           | 0, Some _ | _, None -> false
           | _, Some rate ->
               Float.abs
                 ((rate *. Float.of_int rtt_us /. 1e6) -. Float.of_int send_buffer)
               < 1e-6 *. Float.of_int send_buffer));
      QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:20
         ~name:"a marked connection's packets carry its class"
         QCheck2.Gen.(int_range 0 63)
         (fun dscp ->
           let socket = Unix.socket PF_INET SOCK_STREAM 0 in
           Quiver_core.Tcp.mark socket
             (Option.get (Quiver_core.Tcp.Class.of_int dscp));
           let tos = probe_tos socket in
           Unix.close socket;
           tos = dscp lsl 2));
    Alcotest.test_case "a class is from 0 to 63" `Quick (fun () ->
        Alcotest.(check (list bool))
          "-1, 0, 63, 64"
          [ false; true; true; false ]
          (List.map
             (fun dscp -> Option.is_some (Quiver_core.Tcp.Class.of_int dscp))
             [ -1; 0; 63; 64 ]));
    Alcotest.test_case
      "writers held back by one pacer send at most its rate together"
      `Quick (fun () ->
        let bytes_per_second = 2_000_000 and writers = 4 and writes = 25
        and bytes = 10_000 in
        let pacer = Quiver_net.Pacer.create ~bytes_per_second in
        let started = Unix.gettimeofday () in
        ignore
          (Support.all
             (List.init writers (fun _ () ->
                  for _ = 1 to writes do
                    ignore (Quiver_net.Pacer.wait pacer ~bytes)
                  done)));
        let took = Unix.gettimeofday () -. started
        and least =
          Float.of_int ((writers * writes - 1) * bytes)
          /. Float.of_int bytes_per_second
        in
        let slack = 0.25 in
        Alcotest.(check bool)
          (Printf.sprintf "%.3f s within %.3f s and a quarter second more" took least)
          true
          (took >= least && took <= least +. slack));
  ]
