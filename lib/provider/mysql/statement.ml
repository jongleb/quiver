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
      let isolation =
        match server with
        | Mysql_8_0 _ | Mysql_8_4 _ ->
            "transaction_isolation = 'REPEATABLE-READ', \
             information_schema_stats_expiry = 0"
        | Mariadb_10_11 _ -> "tx_isolation = 'REPEATABLE-READ'"
        | Mariadb_11_4 _ | Mariadb_11_8 _ ->
            "transaction_isolation = 'REPEATABLE-READ'"
      and day_in_seconds = 86_400 in
      [%string
        "SET SESSION time_zone = '+00:00', sql_mode = '', net_read_timeout = \
         %{day_in_seconds#Int}, net_write_timeout = %{day_in_seconds#Int}, \
         %{isolation}"]
  | Flush_tables_with_read_lock -> "FLUSH TABLES WITH READ LOCK"
  | Start_consistent_snapshot -> "START TRANSACTION WITH CONSISTENT SNAPSHOT"
  | Unlock_tables -> "UNLOCK TABLES"
  | Backup_stage_start -> "BACKUP STAGE START"
  | Backup_stage_block_commit -> "BACKUP STAGE BLOCK_COMMIT"
  | Backup_stage_end -> "BACKUP STAGE END"
  | Use database -> [%string "USE %{Quiver_core.Ident.backquoted database}"]
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
      [%string
        "INSERT INTO quiver_load.progress VALUES (%{Query.bytes dump}, \
         %{Query.text target}, %{Query.bytes file})"]
  | Forget_progress { dump; target } ->
      [%string
        "DELETE FROM quiver_load.progress WHERE dump = %{Query.bytes dump} AND \
         target = %{Query.text target}"]
  | Begin -> "START TRANSACTION"
  | Commit -> "COMMIT"
  | Rollback -> "ROLLBACK"
  | Load_local { table; dialect; columns } ->
      let variable i = [%string "@c%{i#Int}"] in
      let value i =
        match dialect with
        | Rfc4180 -> [%string {|NULLIF(%{variable i}, '\\N')|}]
        | Mysql_load_data -> variable i
      and format =
        match dialect with
        | Rfc4180 ->
            "FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '\"' ESCAPED BY \
             '' LINES TERMINATED BY '\\r\\n' IGNORE 1 LINES"
        | Mysql_load_data ->
            "FIELDS TERMINATED BY ',' ENCLOSED BY '\"' ESCAPED BY '\\\\' LINES \
             TERMINATED BY '\\n'"
      in
      let variables = String.concat "," (List.mapi (fun i _ -> variable i) columns)
      and assignments =
        String.concat ", "
          (List.mapi
             (fun i (name, conversion) ->
               let value =
                 match conversion with
                 | Text_value -> value i
                 | Hex_value -> [%string "UNHEX(%{value i})"]
                 | Number_value -> [%string "CAST(%{value i} AS UNSIGNED)"]
               in
               [%string "%{Quiver_core.Ident.backquoted name} = %{value}"])
             columns)
      in
      [%string
        "LOAD DATA LOCAL INFILE 'quiver' INTO TABLE %{Quiver_core.Ident.backquoted table} \
         CHARACTER SET utf8mb4 %{format} (%{variables}) SET %{assignments}"]
