let%expect_test "release lines" =
  List.iter
    (fun version ->
      Format.printf "%-36s %s@." version
        (match Quiver_mysql.Server.of_version_string version with
        | Ok server ->
            Sexplib0.Sexp.to_string_hum (Quiver_mysql.Server.sexp_of_t server)
        | Error (`Unsupported _) -> "unsupported"))
    [
      "8.0.46";
      "8.0.39-log";
      "8.0.36-28";
      "8.0.11";
      "8.0.10";
      "8.1.0";
      "8.4.11";
      "9.1.0";
      "8.0.11-TiDB-v7.5.0";
      "10.6.21-MariaDB";
      "10.11.19-MariaDB-ubu2204-log";
      "11.4.13-MariaDB-ubu2404-log";
      "11.8.9-MariaDB-ubu2404-log";
      "12.0.1-MariaDB";
      "garbage";
    ];
  [%expect
    {|
    8.0.46                               (Mysql_8_0 46)
    8.0.39-log                           (Mysql_8_0 39)
    8.0.36-28                            (Mysql_8_0 36)
    8.0.11                               (Mysql_8_0 11)
    8.0.10                               unsupported
    8.1.0                                unsupported
    8.4.11                               (Mysql_8_4 11)
    9.1.0                                unsupported
    8.0.11-TiDB-v7.5.0                   unsupported
    10.6.21-MariaDB                      unsupported
    10.11.19-MariaDB-ubu2204-log         (Mariadb_10_11 19)
    11.4.13-MariaDB-ubu2404-log          (Mariadb_11_4 13)
    11.8.9-MariaDB-ubu2404-log           (Mariadb_11_8 9)
    12.0.1-MariaDB                       unsupported
    garbage                              unsupported
    |}]
