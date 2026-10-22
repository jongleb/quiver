let counts ?(resent = 0) held =
  { Quiver_core.Advice.sent = 1_000_000; resent; held; connections = 4 }

let path =
  Some { Quiver_core.Tcp.rtt = Core.Time_ns.Span.of_int_ms 100; send_buffer = 4_000_000 }

let advised counts ~seconds =
  List.map
    (fun advice -> Sexplib0.Sexp.to_string (Quiver_core.Advice.sexp_of_t advice))
    (Quiver_core.Advice.of_counts counts path ~seconds ~streams:4)

let tests =
  [
    Alcotest.test_case "Linux: a limit holding connections a tenth of their time is advised"
      `Quick (fun () ->
        Alcotest.(check (list string))
          "receive window"
          [ "(Receive_buffer(Share 0.25))" ]
          (advised
             (counts (Timed { busy_us = 1000; window_full_us = 250; buffer_full_us = 99 }))
             ~seconds:1.);
        Alcotest.(check (list string))
          "send buffer" [ "(Send_buffer(Share 0.1))" ]
          (advised
             (counts (Timed { busy_us = 1000; window_full_us = 0; buffer_full_us = 100 }))
             ~seconds:1.);
        Alcotest.(check (list string))
          "receive window at a tenth" [ "(Receive_buffer(Share 0.1))" ]
          (advised
             (counts (Timed { busy_us = 1000; window_full_us = 100; buffer_full_us = 0 }))
             ~seconds:1.);
        Alcotest.(check (list string))
          "nothing held" []
          (advised
             (counts (Timed { busy_us = 1000; window_full_us = 99; buffer_full_us = 99 }))
             ~seconds:1.));
    Alcotest.test_case "bytes sent again from 0.1 % are a lossy path" `Quick (fun () ->
        let quiet = Quiver_core.Advice.Timed { busy_us = 1; window_full_us = 0; buffer_full_us = 0 } in
        Alcotest.(check (list string))
          "1000 of a million" [ "(Lossy_path 0.001)" ]
          (advised (counts ~resent:1000 quiet) ~seconds:1.);
        Alcotest.(check (list string))
          "999 of a million" [] (advised (counts ~resent:999 quiet) ~seconds:1.));
    Alcotest.test_case
      "macOS: connections near what a buffer allows each round trip are advised"
      `Quick (fun () ->
        Alcotest.(check (list string))
          "send buffer" [ "(Send_buffer(Near_ceiling 0.8))" ]
          (advised (counts (Peer_windows (4 * 8_000_000))) ~seconds:(1. /. 128.));
        Alcotest.(check (list string))
          "slower" [] (advised (counts (Peer_windows (4 * 8_000_000))) ~seconds:1.);
        Alcotest.(check (list string))
          "the peer's window is the smaller limit"
          [ "(Receive_buffer(Near_ceiling 0.8))" ]
          (advised (counts (Peer_windows (4 * 1_000_000))) ~seconds:(1. /. 32.));
        Alcotest.(check (list string))
          "a peer's window wider than the send buffer is not the limit"
          [ "(Send_buffer(Near_ceiling 1.6))" ]
          (advised (counts (Peer_windows (4 * 8_000_000))) ~seconds:(1. /. 256.)));
    Alcotest.test_case "the kernel counts what a connection sent" `Quick (fun () ->
        let listening = Unix.socket PF_INET SOCK_STREAM 0 in
        Unix.bind listening Support.loopback;
        Unix.listen listening 1;
        let connecting = Unix.socket PF_INET SOCK_STREAM 0 in
        Unix.connect connecting (Unix.getsockname listening);
        let accepted, _ = Unix.accept listening in
        let bytes = Bytes.make 100_000 'x' in
        let rec write off =
          if off < Bytes.length bytes then
            write (off + Unix.write connecting bytes off (Bytes.length bytes - off))
        in
        write 0;
        let buffer = Bytes.create 65536 in
        let rec drain left =
          if left > 0 then drain (left - Unix.read accepted buffer 0 (Bytes.length buffer))
        in
        drain (Bytes.length bytes);
        let strain = Quiver_core.Tcp.strain connecting in
        List.iter Unix.close [ accepted; connecting; listening ];
        Alcotest.(check bool)
          "at least the bytes written" true
          (Option.fold ~none:false
             ~some:(fun ({ sent; _ } : Quiver_core.Tcp.strain) -> sent >= Bytes.length bytes)
             strain));
  ]
