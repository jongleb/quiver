open Quiver_tikv

let show result =
  match result with
  | Ok text -> "ok " ^ text
  | Error (`Http1 error) -> Base.Sexp.to_string (Http1.sexp_of_error error)
  | Error (`Transport error) -> Base.Sexp.to_string (Quiver_mysql.Transport.sexp_of_error error)

let chunked =
  QCheck2.Test.make ~count:2000 ~name:"a response's body read back however it was sent: by length, in chunks or up to the end"
    ~print:(fun (body, sizes, _) -> Printf.sprintf "%S %s" body (String.concat "," (List.map string_of_int sizes)))
    QCheck2.Gen.(triple (string_size ~gen:char (int_range 0 3000)) (list_size (int_range 1 8) (int_range 1 700)) (int_range 0 2))
    (fun (body, sizes, way) ->
      let head headers = "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n" ^ headers ^ "\r\n" in
      let chunks =
        let buffer = Buffer.create (String.length body * 2) in
        let rec go pos sizes =
          if pos < String.length body then (
            let size, rest = match sizes with size :: rest -> (size, rest @ [ size ]) | [] -> (1, []) in
            let size = Int.min size (String.length body - pos) in
            Buffer.add_string buffer (Printf.sprintf "%x%s\r\n" size (if size mod 2 = 0 then ";x=y" else ""));
            Buffer.add_string buffer (String.sub body pos size);
            Buffer.add_string buffer "\r\n";
            go (pos + size) rest)
        in
        go 0 sizes;
        Buffer.contents buffer ^ "0\r\n\r\n"
      in
      let text =
        match way with
        | 0 -> head (Printf.sprintf "Content-Length: %d\r\n" (String.length body)) ^ body ^ "trailing"
        | 1 -> head "Transfer-Encoding: Chunked\r\n" ^ chunks
        | _ -> head "" ^ body
      in
      Http1.body text = Ok body)

let refused () =
  let check name expected text = Alcotest.(check string) name expected (show (Http1.body text)) in
  check "another status" {|(Refused(code 400)(body"\"bad rule\""))|} "HTTP/1.1 400 Bad Request\r\nContent-Length: 10\r\n\r\n\"bad rule\"";
  check "a redirect" {|(Refused(code 307)(body""))|} "HTTP/1.1 307 Temporary Redirect\r\nContent-Length: 0\r\n\r\n";
  check "any 2xx" "ok done" "HTTP/1.1 204 No Content\r\nContent-Length: 4\r\n\r\ndone";
  check "a body shorter than its length" {|(Malformed"a body shorter than its length")|} "HTTP/1.1 200 OK\r\nContent-Length: 9\r\n\r\nabc";
  check "a chunk cut short" {|(Malformed"a chunk cut short or of no size")|} "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nab";
  check "no end of headers" {|(Malformed"no end of the headers")|} "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n";
  check "no status line" {|(Malformed"no status line")|} "SSH-2.0\r\n\r\n"

let texts () =
  Alcotest.(check string) "a post"
    "POST /a/b HTTP/1.1\r\nHost: pd:2379\r\nAccept: application/json\r\nConnection: close\r\nContent-Type: application/json\r\nContent-Length: 7\r\n\r\n{\"x\":1}"
    (Http1.text ~host:"pd" ~port:2379 (Post { path = "/a/b"; json = {|{"x":1}|} }));
  Alcotest.(check string) "a delete to IPv6"
    "DELETE /r/q HTTP/1.1\r\nHost: [::1]:2379\r\nAccept: application/json\r\nConnection: close\r\nContent-Length: 0\r\n\r\n"
    (Http1.text ~host:"::1" ~port:2379 (Delete "/r/q"));
  Alcotest.(check (list (option (pair string int)))) "addresses"
    [ Some ("pd", 2379); Some ("::1", 2379); Some ("10.0.0.1", 80); None; None ]
    (List.map Http2.address [ "pd:2379"; "[::1]:2379"; "10.0.0.1:80"; "pd"; "pd:port" ])

let served ~replies client =
  let listener, port = Test_http2.listening () in
  let heard =
    Support.start (fun () ->
        List.map
          (fun reply ->
            let connection, _ = Unix.accept ~cloexec:true listener in
            let buffer = Buffer.create 1024 and chunk = Bytes.create 4096 in
            let whole text =
              Option.bind (Base.String.substr_index text ~pattern:"\r\n\r\n") (fun ending ->
                  let length =
                    List.fold_left
                      (fun length line ->
                        Option.value ~default:length
                          (Option.bind (Base.String.chop_prefix line ~prefix:"Content-Length: ") (fun value ->
                               int_of_string_opt (String.trim value))))
                      0
                      (String.split_on_char '\n' (String.sub text 0 ending))
                  in
                  if String.length text >= ending + 4 + length then Some text else None)
            in
            let rec read () =
              match whole (Buffer.contents buffer) with
              | Some text -> text
              | None -> (
                  match Unix.read connection chunk 0 4096 with
                  | 0 -> Buffer.contents buffer
                  | count ->
                      Buffer.add_subbytes buffer chunk 0 count;
                      read ())
            in
            let request = read () in
            ignore (Unix.write_substring connection reply 0 (String.length reply));
            Unix.close connection;
            request)
          replies)
  in
  let result = client port in
  let heard = Support.join heard in
  Unix.close listener;
  (result, heard)

let ok body = Printf.sprintf "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: %d\r\n\r\n%s" (String.length body) body
let plain : Http2.dial = { timeout = Core.Time_ns.Span.of_int_sec 5; security = Cleartext }

let requested () =
  let request : Http1.request = Post { path = "/p"; json = {|{"a":[1,2]}|} } in
  let used = ref 0 in
  let result, heard =
    served ~replies:[ ok "\"Update successfully.\"" ] (fun port ->
        used := port;
        Http1.request plain ~host:"127.0.0.1" ~port request)
  in
  Alcotest.(check string) "the answer's body" "ok \"Update successfully.\"" (show result);
  Alcotest.(check (list string)) "the request as sent" [ Http1.text ~host:"127.0.0.1" ~port:!used request ] heard;
  let result, _ =
    served ~replies:[ "HTTP/1.1 500 Internal Server Error\r\nContent-Length: 6\r\n\r\n\"oops\"" ] (fun port ->
        Http1.request plain ~host:"127.0.0.1" ~port (Delete "/x"))
  in
  Alcotest.(check string) "a refusal" {|(Refused(code 500)(body"\"oops\""))|} (show result)

let secured () =
  let context =
    match Quiver_openssl.Tls.cluster (Ca_file "tls/ca.crt") Anonymous with
    | Ok context -> context
    | Error (_ : Quiver_openssl.Tls.unusable) -> Alcotest.fail "no context"
  in
  let through ?(reply = ok "{}") alpn certificate =
    let (result, heard), chosen =
      let front = ref None in
      let outcome =
        served ~replies:[ reply ] (fun backend ->
            let process =
              Unix.open_process_args_in "python3"
                [| "python3"; "-I"; "tls_h2.py"; Int.to_string backend; "tls/" ^ certificate ^ ".crt"; "tls/" ^ certificate ^ ".key"; alpn; "-" |]
            in
            front := Some process;
            let port = int_of_string (input_line process) in
            Http1.request { timeout = Core.Time_ns.Span.of_int_sec 5; security = Secured context } ~host:"127.0.0.1" ~port
              (Post { path = "/s"; json = "{}" }))
      in
      let chosen =
        Option.fold ~none:"" ~some:(fun process ->
            let line = try input_line process with End_of_file -> "" in
            let (_ : Unix.process_status) = Unix.close_process_in process in
            line)
          !front
      in
      (outcome, chosen)
    in
    (show result, List.length heard, chosen)
  in
  let result, _, chosen = through "http/1.1" "node" in
  Alcotest.(check (pair string string)) "over TLS, offering http/1.1" ("ok {}", "http/1.1") (result, chosen);
  let result, _, _ = through ~reply:"HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n2\r\n{}\r\n0\r\n\r\n" "http/1.1" "node" in
  Alcotest.(check string) "a body in chunks, whole before the close without TLS's notice" "ok {}" result;
  let result, _, _ = through ~reply:"HTTP/1.1 200 OK\r\n\r\n{}" "http/1.1" "node" in
  Alcotest.(check bool) "a body up to the end, cut by a close without TLS's notice" true
    (Base.String.is_prefix result ~prefix:"(Tls");
  let result, _, _ = through "http/1.1" "stranger" in
  Alcotest.(check bool) "a chain to another CA" true (Base.String.is_prefix result ~prefix:"(Untrusted")

let labelled () =
  let rule = Label.deny ~name:"quiver_mirror_7" (Codec.table 5L) [ Codec.table 9L ] in
  let hex key = Hex_encode.to_hex (Codec.encode_bytes key) in
  let range id =
    let first, past = Codec.table id in
    Printf.sprintf {|{"start_key":"%s","end_key":"%s"}|} (hex first) (hex past)
  in
  let json = Label.json rule (Core.Time_ns.Span.of_int_sec 180) in
  Alcotest.(check string) "the rule"
    (Printf.sprintf
       {|{"id":"quiver_mirror_7","labels":[{"key":"schedule","value":"deny","ttl":"180000ms"}],"rule_type":"key-range","data":[%s,%s]}|}
       (range 5L) (range 9L))
    json;
  let parsed = Yojson.Safe.from_string json in
  Alcotest.(check bool) "JSON" true (match parsed with `Assoc _ -> true | _ -> false);
  let result, heard =
    served ~replies:[ ok "\"Update region label rule successfully.\""; ok "\"Delete successfully.\"" ] (fun port ->
        Result.bind (Label.keep plain ~host:"127.0.0.1" ~port rule (Hold_for (Core.Time_ns.Span.of_int_sec 180))) (fun () ->
            Label.keep plain ~host:"127.0.0.1" ~port rule Let_go))
  in
  Alcotest.(check bool) "held, then let go" true (Result.is_ok result);
  let line text = Option.value ~default:text (Option.map fst (Base.String.lsplit2 text ~on:'\r')) in
  Alcotest.(check (list string)) "a POST of the rule, then a DELETE of it by name"
    [ "POST /pd/api/v1/config/region-label/rule HTTP/1.1"; "DELETE /pd/api/v1/config/region-label/rule/quiver_mirror_7 HTTP/1.1" ]
    (List.map line heard);
  Alcotest.(check bool) "the POST carries the rule" true
    (match heard with post :: _ -> Base.String.is_suffix post ~suffix:json | [] -> false)

let tests =
  [
    QCheck_alcotest.to_alcotest chunked;
    Alcotest.test_case "refusals" `Quick refused;
    Alcotest.test_case "requests as sent, addresses" `Quick texts;
    Alcotest.test_case "a request and its answer" `Quick requested;
    Alcotest.test_case "over TLS" `Quick secured;
    Alcotest.test_case "PD's rule denying scheduling" `Quick labelled;
  ]
