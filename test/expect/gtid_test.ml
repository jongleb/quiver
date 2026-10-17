let%expect_test "gtid sets" =
  let mysql_8_0 = Result.get_ok (Quiver_mysql.Server.of_version_string "8.0.46")
  and mysql_8_4 = Result.get_ok (Quiver_mysql.Server.of_version_string "8.4.11")
  and mariadb =
    Result.get_ok (Quiver_mysql.Server.of_version_string "11.8.9-MariaDB")
  in
  List.iter
    (fun (server, raw) ->
      Format.printf "%S@.  %s@." raw
        (match Quiver_mysql.Gtid.of_server_string server raw with
        | Ok gtid -> Quiver_mysql.Gtid.to_string gtid
        | Error (`Gtid error) ->
            (fun value ->
              Sexplib0.Sexp.to_string_hum
                (Quiver_mysql.Gtid.sexp_of_error value))
              error))
    [
      (mysql_8_4, "");
      (mysql_8_4, "  ");
      (mysql_8_4, "3E11FA47-71CA-11E1-9E33-C80AA9429562:1-5");
      ( mysql_8_4,
        "3e11fa47-71ca-11e1-9e33-c80aa9429562:1-5:11:20-30,\n\
         4f22fa47-71ca-11e1-9e33-c80aa9429562:7" );
      (mysql_8_4, "3e11fa47-71ca-11e1-9e33-c80aa9429562:1-3:Tag_1:5-6:other:9");
      (mysql_8_0, "3e11fa47-71ca-11e1-9e33-c80aa9429562:1-3:tag:5");
      (mysql_8_4, "3e11fa47-71ca-11e1-9e33-c80aa9429562:5-3");
      (mysql_8_4, "3e11fa47-71ca-11e1-9e33-c80aa9429562:0");
      (mysql_8_4, "3e11fa47-71ca-11e1-9e33-c80aa942956:1");
      (mysql_8_4, "3e11fa4771ca-11e1-9e33-c80aa9429562-1:1");
      (mysql_8_4, "3e11fa47-71ca-11e1-9e33-c80aa9429562:1x");
      (mysql_8_4, "3e11fa47-71ca-11e1-9e33-c80aa9429562:9tag");
      (mariadb, "");
      (mariadb, "0-1-100");
      (mariadb, "0-1-100,1-2-18446744073709551615");
      (mariadb, "0-1");
      (mariadb, "-1-1-1");
    ];
  [%expect
    {|
    ""

    "  "

    "3E11FA47-71CA-11E1-9E33-C80AA9429562:1-5"
      3e11fa47-71ca-11e1-9e33-c80aa9429562:1-5
    "3e11fa47-71ca-11e1-9e33-c80aa9429562:1-5:11:20-30,\n4f22fa47-71ca-11e1-9e33-c80aa9429562:7"
      3e11fa47-71ca-11e1-9e33-c80aa9429562:1-5:11:20-30,4f22fa47-71ca-11e1-9e33-c80aa9429562:7
    "3e11fa47-71ca-11e1-9e33-c80aa9429562:1-3:Tag_1:5-6:other:9"
      3e11fa47-71ca-11e1-9e33-c80aa9429562:1-3,3e11fa47-71ca-11e1-9e33-c80aa9429562:tag_1:5-6,3e11fa47-71ca-11e1-9e33-c80aa9429562:other:9
    "3e11fa47-71ca-11e1-9e33-c80aa9429562:1-3:tag:5"
      (Tag_unsupported (Mysql_8_0 46))
    "3e11fa47-71ca-11e1-9e33-c80aa9429562:5-3"
      (Malformed 3e11fa47-71ca-11e1-9e33-c80aa9429562:5-3)
    "3e11fa47-71ca-11e1-9e33-c80aa9429562:0"
      (Malformed 3e11fa47-71ca-11e1-9e33-c80aa9429562:0)
    "3e11fa47-71ca-11e1-9e33-c80aa942956:1"
      (Malformed 3e11fa47-71ca-11e1-9e33-c80aa942956:1)
    "3e11fa4771ca-11e1-9e33-c80aa9429562-1:1"
      (Malformed 3e11fa4771ca-11e1-9e33-c80aa9429562-1:1)
    "3e11fa47-71ca-11e1-9e33-c80aa9429562:1x"
      (Malformed 3e11fa47-71ca-11e1-9e33-c80aa9429562:1x)
    "3e11fa47-71ca-11e1-9e33-c80aa9429562:9tag"
      (Malformed 3e11fa47-71ca-11e1-9e33-c80aa9429562:9tag)
    ""

    "0-1-100"
      0-1-100
    "0-1-100,1-2-18446744073709551615"
      0-1-100,1-2-18446744073709551615
    "0-1"
      (Malformed 0-1)
    "-1-1-1"
      (Malformed -1-1-1)
    |}]
