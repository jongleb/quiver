let%expect_test "a TiDB position survives the manifest and names its TSO" =
  let position = Quiver_mysql.Provider.Pinned { tso = 470118915864264704L } in
  let json = Quiver_mysql.Provider.yojson_of_position position in
  print_endline (Yojson.Safe.to_string json);
  print_endline
    (Sexplib0.Sexp.to_string
       (Quiver_mysql.Provider.sexp_of_position
          (Quiver_mysql.Provider.position_of_yojson json)));
  print_string (Quiver_mysql.Provider.source position);
  [%expect {|
    ["Pinned",{"tso":470118915864264704}]
    (Pinned(tso 470118915864264704))
    [source]
    # Channel_Name = '' # It can be use to setup replication FOR CHANNEL
    # TiDB TSO (tidb_snapshot, TiCDC --start-ts) = 470118915864264704
    # quiver: read at 2026-10-30 11:55:42.866000000Z
    |}]
