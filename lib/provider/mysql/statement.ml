type t =
  | Configure_session of Server.t
  | Flush_tables_with_read_lock
  | Start_consistent_snapshot
  | Unlock_tables
  | Backup_stage_start
  | Backup_stage_block_commit
  | Backup_stage_end
  | Use of Quiver_core.Ident.t
  | Prepare_load
  | Skip_binlog
  | Create_progress_schema
  | Create_progress_table
  | Record_progress of { dump : string; target : string; file : string }
  | Forget_progress of { dump : string; target : string }
  | Begin
  | Commit
  | Rollback
  | Load_local of {
      table : Quiver_core.Ident.t;
      dialect : Quiver_core.Output_format.dialect;
      columns : (Quiver_core.Ident.t * conversion) list;
    }

and conversion = Text_value | Hex_value | Number_value [@@deriving sexp_of ~portable]

let sql = function
  | Configure_session server ->
      let day_in_seconds = string_of_int 86_400 in
      "SET SESSION time_zone = '+00:00', sql_mode = '', net_read_timeout = "
      ^ day_in_seconds ^ ", net_write_timeout = " ^ day_in_seconds
      ^ (match server with
        | Mysql_8_0 _ | Mysql_8_4 _ ->
            ", transaction_isolation = 'REPEATABLE-READ', \
             information_schema_stats_expiry = 0"
        | Mariadb_10_11 _ -> ", tx_isolation = 'REPEATABLE-READ'"
        | Mariadb_11_4 _ | Mariadb_11_8 _ ->
            ", transaction_isolation = 'REPEATABLE-READ'")
  | Flush_tables_with_read_lock -> "FLUSH TABLES WITH READ LOCK"
  | Start_consistent_snapshot -> "START TRANSACTION WITH CONSISTENT SNAPSHOT"
  | Unlock_tables -> "UNLOCK TABLES"
  | Backup_stage_start -> "BACKUP STAGE START"
  | Backup_stage_block_commit -> "BACKUP STAGE BLOCK_COMMIT"
  | Backup_stage_end -> "BACKUP STAGE END"
  | Use database -> "USE " ^ Query.quote database
  | Prepare_load ->
      "SET SESSION foreign_key_checks = 0, unique_checks = 0, time_zone = \
       '+00:00', sql_mode = 'NO_AUTO_VALUE_ON_ZERO,ALLOW_INVALID_DATES'"
  | Skip_binlog -> "SET SESSION sql_log_bin = 0"
  | Create_progress_schema -> "CREATE DATABASE IF NOT EXISTS quiver_load"
  | Create_progress_table ->
      "CREATE TABLE IF NOT EXISTS quiver_load.progress (dump CHAR(16) \
       CHARACTER SET ascii NOT NULL, target VARCHAR(64) NOT NULL, file \
       VARCHAR(512) CHARACTER SET ascii NOT NULL, PRIMARY KEY (dump, target, \
       file))"
  | Record_progress { dump; target; file } ->
      Printf.sprintf
        "INSERT INTO quiver_load.progress VALUES (X'%s', _utf8mb4 X'%s', X'%s')"
        (Hex_encode.to_hex ~case:`Lowercase dump)
        (Hex_encode.to_hex ~case:`Lowercase target)
        (Hex_encode.to_hex ~case:`Lowercase file)
  | Forget_progress { dump; target } ->
      Printf.sprintf
        "DELETE FROM quiver_load.progress WHERE dump = X'%s' AND target = \
         _utf8mb4 X'%s'"
        (Hex_encode.to_hex ~case:`Lowercase dump)
        (Hex_encode.to_hex ~case:`Lowercase target)
  | Begin -> "START TRANSACTION"
  | Commit -> "COMMIT"
  | Rollback -> "ROLLBACK"
  | Load_local { table; dialect; columns } ->
      let variable i = "@c" ^ string_of_int i in
      let value i =
        match dialect with
        | Rfc4180 -> "NULLIF(" ^ variable i ^ ", '\\\\N')"
        | Mysql_load_data -> variable i
      in
      "LOAD DATA LOCAL INFILE 'quiver' INTO TABLE " ^ Query.quote table
      ^ " CHARACTER SET utf8mb4 "
      ^ (match dialect with
        | Rfc4180 ->
            "FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' ESCAPED BY \
             '' LINES TERMINATED BY '\\r\\n' IGNORE 1 LINES"
        | Mysql_load_data ->
            "FIELDS TERMINATED BY ',' ENCLOSED BY '\"' ESCAPED BY '\\\\' LINES \
             TERMINATED BY '\\n'")
      ^ " ("
      ^ String.concat "," (List.mapi (fun i _ -> variable i) columns)
      ^ ") SET "
      ^ String.concat ", "
          (List.mapi
             (fun i (name, conversion) ->
               Query.quote name ^ " = "
               ^
               match conversion with
               | Text_value -> value i
               | Hex_value -> "UNHEX(" ^ value i ^ ")"
               | Number_value -> "CAST(" ^ value i ^ " AS UNSIGNED)")
             columns)
