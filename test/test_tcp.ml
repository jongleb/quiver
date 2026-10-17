external probe : Unix.file_descr -> int array = "quiver_tcp_probe"

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
  ]
