open Quiver_tikv

let shown text = match Info.of_json text with Ok info -> Base.Sexp.to_string (Info.sexp_of_t info) | Error (`Info error) -> Base.Sexp.to_string (Info.sexp_of_error error)

let tables () =
  let check name expected text = Alcotest.(check string) name expected (shown text) in
  check "columns and indexes by their lower names, states, the handle"
    "((id 110)(columns(((name id)(id 1)(state Public)(missing Unset)(kind Unknown))((name b)(id 3)(state Changing)(missing Unset)(kind Unknown))))(indexes(((name kb)(id 2)(state Public)(scope Local))))(handle Integer_key)(layout Whole))"
    {|{"id":110,"name":{"O":"Keyed","L":"keyed"},"cols":[{"id":1,"name":{"O":"ID","L":"id"},"offset":0,"state":5,"origin_default":null},
       {"id":3,"name":{"O":"b","L":"b"},"offset":1,"state":3}],
      "index_info":[{"id":2,"idx_name":{"O":"Kb","L":"kb"},"state":5}],"pk_is_handle":true,"is_common_handle":false,"partition":null}|};
  check "a column added with a default, one added as bits, a virtual one, a stored one"
    "((id 7)(columns(((name a)(id 1)(state Public)(missing(Origin 5))(kind Unknown))((name b)(id 2)(state Public)(missing(Origin\"\\001\"))(kind Unknown))((name v)(id 3)(state Public)(missing Unset)(kind Unknown))((name s)(id 4)(state Public)(missing(Origin 1))(kind Unknown))))(indexes())(handle Common_key)(layout Whole))"
    {|{"id":7,"cols":[{"id":1,"name":{"L":"a"},"state":5,"origin_default":"5"},
       {"id":2,"name":{"L":"b"},"state":5,"origin_default":null,"origin_default_bit":"AQ=="},
       {"id":3,"name":{"L":"v"},"state":5,"origin_default":"1","generated_expr_string":"`a` + 1","generated_stored":false},
       {"id":4,"name":{"L":"s"},"state":5,"origin_default":"1","generated_expr_string":"`a` + 1","generated_stored":true}],
      "index_info":null,"pk_is_handle":false,"is_common_handle":true}|};
  check "the hidden row number" "((id 1)(columns(((name a)(id 1)(state Public)(missing Unset)(kind Unknown))))(indexes())(handle Row_number)(layout Whole))"
    {|{"id":1,"cols":[{"id":1,"name":{"L":"a"},"state":5,"generated_expr_string":""}],"pk_is_handle":false,"is_common_handle":false}|};
  check "a column without its id" {|(Malformed"cols: an item without its name or id")|} {|{"id":1,"cols":[{"name":{"L":"a"},"state":5}]}|};
  check "a column without its state" {|(Malformed"cols: an item without its state")|} {|{"id":1,"cols":[{"id":1,"name":{"L":"a"}}]}|};
  check "a default that is a number" {|(Malformed"cols: a default that is not text")|} {|{"id":1,"cols":[{"id":1,"name":{"L":"a"},"state":5,"origin_default":5}]}|};
  check "bits in all of base64" {|((id 1)(columns(((name a)(id 1)(state Public)(missing(Origin"\251\255"))(kind Unknown))))(indexes())(handle Row_number)(layout Whole))|}
    {|{"id":1,"cols":[{"id":1,"name":{"L":"a"},"state":5,"origin_default_bit":"+/8="}]}|};
  check "bits not in base64" {|(Malformed"cols: a default of bits not in base64")|}
    {|{"id":1,"cols":[{"id":1,"name":{"L":"a"},"state":5,"origin_default_bit":"A*=="}]}|};
  check "a table without its id" {|(Malformed"a table without its id")|} {|{"cols":[]}|};
  let same name expected got = Alcotest.(check string) name expected got in
  let partitioned ?(table = "") partition =
    match Info.of_json (Printf.sprintf {|{"id":90,"cols":[],"index_info":[{"id":1,"idx_name":{"L":"g"},"state":5,"is_global":false}],%s"partition":%s}|} table partition) with
    | Ok info -> Base.Sexp.to_string (Info.sexp_of_layout info.layout)
    | Error (`Info error) -> Base.Sexp.to_string (Info.sexp_of_error error)
  in
  let settled_but key =
    String.concat ","
      (List.filter_map
         (fun (field, value) -> if field = key then None else Some (Printf.sprintf {|"%s":%s|} field value))
         [ ("adding_definitions", "null"); ("dropping_definitions", "[]"); ("states", "null"); ("num", "0"); ("ddl_state", "0"); ("new_table_id", "0") ])
  in
  let settled = settled_but "" in
  same "partitions by their lower names, in TiDB's order" "(Partitioned((name p9)(id 92))(((name p0)(id 91))((name p5)(id 95))))"
    (partitioned
       (Printf.sprintf
          {|{"type":1,"expr":"`a`","enable":true,"definitions":[{"id":92,"name":{"O":"P9","L":"p9"},"less_than":["10"]},{"id":91,"name":{"O":"p0","L":"p0"}},{"id":95,"name":{"L":"p5"}}],%s}|}
          settled));
  same "partitions written under the table's id" "Whole" (partitioned (Printf.sprintf {|{"enable":false,"definitions":[{"id":92,"name":{"L":"p"}}],%s}|} settled));
  List.iter
    (fun (name, key, value) ->
      same name "Rearranging"
        (partitioned (Printf.sprintf {|{"enable":true,"definitions":[{"id":92,"name":{"L":"p"}}],%s,"%s":%s}|} (settled_but key) key value)))
    [
      ("partitions being added", "adding_definitions", {|[{"id":93,"name":{"L":"q"}}]|});
      ("partitions being dropped", "dropping_definitions", {|[{"id":92,"name":{"L":"p"}}]|});
      ("partitions being truncated", "new_partition_ids", "[94]");
      ("partitions being reorganized", "ddl_state", "4");
      ("an action on partitions", "ddl_action", "20");
      ("a new table id", "new_table_id", "99");
    ];
  same "a partition being exchanged" "Rearranging"
    (partitioned ~table:{|"exchange_partition_info":{"exchange_partition_id":92},|} (Printf.sprintf {|{"enable":true,"definitions":[{"id":92,"name":{"L":"p"}}],%s}|} settled));
  same "a table being exchanged" "Rearranging" (partitioned ~table:{|"exchange_partition_info":{"exchange_partition_id":92},|} "null");
  same "no definitions" {|(Malformed"partition: no definitions")|} (partitioned {|{"enable":true,"definitions":[]}|});
  same "a definition without its id" {|(Malformed"partition: a definition without its name or id")|} (partitioned {|{"enable":true,"definitions":[{"name":{"L":"p"}}]}|});
  check "a global index" "((id 1)(columns())(indexes(((name g)(id 3)(state Public)(scope(Global In_values)))))(handle Row_number)(layout Whole))"
    {|{"id":1,"cols":[],"index_info":[{"id":3,"idx_name":{"L":"g"},"state":5,"is_global":true}]}|};
  check "a global index naming partitions in its keys" "((id 1)(columns())(indexes(((name g)(id 3)(state Public)(scope(Global In_keys_too)))))(handle Row_number)(layout Whole))"
    {|{"id":1,"cols":[],"index_info":[{"id":3,"idx_name":{"L":"g"},"state":5,"is_global":true,"global_index_version":1}]}|};
  check "a global index of a version this does not know" "(Malformed\"index_info: a global_index_version this does not know\")"
    {|{"id":1,"cols":[],"index_info":[{"id":3,"idx_name":{"L":"g"},"state":5,"is_global":true,"global_index_version":2}]}|};
  Alcotest.(check string) "not JSON" "malformed" (match Info.of_json "{" with Error (`Info (Malformed _)) -> "malformed" | Ok _ -> "read")

let kinds () =
  let kind ?(version = 2) ?(charset = "binary") ?(elements = "null") ?(literal = "null") tp flag flen decimal =
    let text =
      Printf.sprintf
        {|{"id":1,"cols":[{"id":1,"name":{"L":"a"},"state":5,"version":%d,"type":{"Tp":%d,"Flag":%d,"Flen":%d,"Decimal":%d,"Charset":"%s","Elems":%s,"ElemsIsBinaryLit":%s}}]}|}
        version tp flag flen decimal charset elements literal
    in
    match Info.of_json text with
    | Ok { columns = [ column ]; _ } -> Base.Sexp.to_string (Info.sexp_of_kind column.kind)
    | Ok _ -> "columns"
    | Error (`Info error) -> Base.Sexp.to_string (Info.sexp_of_error error)
  in
  let check name expected shown = Alcotest.(check string) name expected shown in
  List.iter (fun tp -> check (Printf.sprintf "integer %d" tp) "(Integer Signed)" (kind tp 0 11 0)) [ 1; 2; 3; 8; 9 ];
  check "unsigned" "(Integer Unsigned)" (kind 3 (32 lor 4097) 10 0);
  check "year" "Year" (kind 13 96 4 0);
  check "float" "Float" (kind 4 0 12 (-1));
  check "double" "Double" (kind 5 0 22 (-1));
  check "decimal" "(Decimal(precision 30)(scale 10))" (kind 246 0 30 10);
  check "char" "Char" (kind ~charset:"utf8mb4" 254 0 5 0);
  check "binary" "Bytes" (kind 254 128 4 0);
  List.iter (fun tp -> check (Printf.sprintf "bytes %d" tp) "Bytes" (kind ~charset:"latin1" tp 0 10 0)) [ 15; 249; 250; 251; 252; 253 ];
  check "text of a character set it does not write" "Unknown" (kind ~charset:"gbk" 15 0 10 0);
  check "char of a character set it does not write" "Unknown" (kind ~charset:"gb18030" 254 0 10 0);
  check "date" "Date" (kind 10 128 10 0);
  check "datetime" "(Datetime(fsp 3))" (kind 12 128 23 3);
  check "timestamp" "(Timestamp(clock Utc)(fsp 6))" (kind 7 128 26 6);
  check "timestamp of TiDB before 3.0" "(Timestamp(clock Server)(fsp 0))" (kind ~version:0 7 128 19 0);
  check "time" "(Time(fsp 2))" (kind 11 128 13 2);
  check "enum" "(Enum(a b))" (kind ~charset:"utf8mb4" ~elements:{|["a","b"]|} 247 0 1 0);
  check "set" "(Set(a b))" (kind ~charset:"utf8mb4" ~elements:{|["a","b"]|} ~literal:"[false,false]" 248 0 3 0);
  check "enum of binary literals" "Unknown" (kind ~elements:{|["a","b"]|} ~literal:"[false,true]" 247 0 1 0);
  check "bit" "Bit" (kind 16 32 10 0);
  check "json" "Json" (kind 245 128 4294967295 0);
  check "vector" "Unknown" (kind 225 0 3 0);
  check "geometry" "Unknown" (kind 255 128 0 0)

let databases () =
  let shown text =
    match Info.database text with
    | Ok (name, id) -> Printf.sprintf "%s %Ld" name id
    | Error (`Info error) -> Base.Sexp.to_string (Info.sexp_of_error error)
  in
  Alcotest.(check string) "a database by its lower name" "shop 104" (shown {|{"id":104,"db_name":{"O":"Shop","L":"shop"},"charset":"utf8mb4"}|});
  Alcotest.(check string) "without a name" {|(Malformed"a database without its name or id")|} (shown {|{"id":104}|})

let meta () =
  let hex text = Hex_encode.to_hex text in
  Alcotest.(check string) "the databases' fields"
    (hex ("m" ^ "DBs\x00\x00\x00\x00\x00\xfa" ^ "\x00\x00\x00\x00\x00\x00\x00h"))
    (hex (fst Codec.databases));
  Alcotest.(check string) "past them" (hex ("m" ^ "DBs\x00\x00\x00\x00\x00\xfa" ^ "\x00\x00\x00\x00\x00\x00\x00i")) (hex (snd Codec.databases));
  Alcotest.(check string) "a table's description"
    (hex ("m" ^ "DB:104\x00\x00\xfd" ^ "\x00\x00\x00\x00\x00\x00\x00h" ^ "Table:11\xff" ^ "2\x00\x00\x00\x00\x00\x00\x00\xf8"))
    (hex (Codec.described ~database:104L ~table:112L))

let tests =
  [
    Alcotest.test_case "a table's description" `Quick tables;
    Alcotest.test_case "what columns hold" `Quick kinds;
    Alcotest.test_case "a database's description" `Quick databases;
    Alcotest.test_case "meta keys" `Quick meta;
  ]
