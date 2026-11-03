open Quiver_tikv

let show result =
  match result with
  | Ok text -> "ok " ^ text
  | Error (`Status error) -> Base.Sexp.to_string (Status.sexp_of_error error)

let chunked =
  QCheck2.Test.make ~count:2000 ~name:"a response's body read back however it was sent: by length, in chunks or up to the end"
    ~print:(fun (body, sizes, _) -> Printf.sprintf "%S %s" body (String.concat "," (List.map string_of_int sizes)))
    QCheck2.Gen.(
      triple (string_size ~gen:char (int_range 0 3000)) (list_size (int_range 1 8) (int_range 1 700)) (int_range 0 2))
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
      Status.body text = Ok body)

let refused () =
  let check name expected text = Alcotest.(check string) name expected (show (Status.body text)) in
  check "another status" {|(Refused(code 404)(body"no table"))|} "HTTP/1.1 404 Not Found\r\nContent-Length: 8\r\n\r\nno table";
  check "a body shorter than its length" {|(Malformed"a body shorter than its length")|} "HTTP/1.1 200 OK\r\nContent-Length: 9\r\n\r\nabc";
  check "a chunk cut short" {|(Malformed"a chunk cut short or of no size")|} "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nab";
  check "no end of headers" {|(Malformed"no end of the headers")|} "HTTP/1.1 200 OK\r\nContent-Length: 2\r\n";
  check "no status line" {|(Malformed"no status line")|} "SSH-2.0\r\n\r\n"

let ids () =
  let table =
    {|{"id":110,"name":{"O":"Keyed","L":"keyed"},"cols":[{"id":1,"name":{"O":"ID","L":"id"},"offset":0,"state":5},
       {"id":3,"name":{"O":"b","L":"b"},"offset":1,"state":5}],
      "index_info":[{"id":2,"idx_name":{"O":"Kb","L":"kb"},"state":5}],"pk_is_handle":true,"is_common_handle":false,"partition":null}|}
  in
  let shown text = match Status.ids text with Ok ids -> Base.Sexp.to_string (Status.sexp_of_ids ids) | Error (`Status error) -> Base.Sexp.to_string (Status.sexp_of_error error) in
  Alcotest.(check string) "columns and indexes by their lower names, the handle" "((columns((id 1)(b 3)))(indexes((kb 2)))(handle Integer_key))" (shown table);
  Alcotest.(check string) "a common handle, no indexes"
    "((columns((a 1)))(indexes())(handle Common_key))"
    (shown {|{"cols":[{"id":1,"name":{"O":"a","L":"a"}}],"index_info":null,"pk_is_handle":false,"is_common_handle":true}|});
  Alcotest.(check string) "the hidden row number" "((columns((a 1)))(indexes())(handle Row_number))"
    (shown {|{"cols":[{"id":1,"name":{"O":"a","L":"a"}}],"pk_is_handle":false,"is_common_handle":false}|});
  Alcotest.(check string) "a column without its id" {|(Malformed"cols: an item without its name or id")|} (shown {|{"cols":[{"name":{"L":"a"}}]}|});
  Alcotest.(check string) "not JSON" "malformed" (match Status.ids "{" with Error (`Status (Malformed _)) -> "malformed" | Ok _ | Error _ -> "read");
  Alcotest.(check string) "the path escapes the names" "/schema/a%20b/t%2Fx-1_~."
    (Status.path { database = Support.ident "a b"; table = Support.ident "t/x-1_~." })

let tests = [ QCheck_alcotest.to_alcotest chunked; Alcotest.test_case "refusals" `Quick refused; Alcotest.test_case "a table's ids" `Quick ids ]
