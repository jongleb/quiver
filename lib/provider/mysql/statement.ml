type allocator = Auto_increment | Auto_random [@@deriving sexp_of ~portable]

type t =
  | Configure_session of Server.t
  | Flush_tables_with_read_lock
  | Start_consistent_snapshot
  | Pin_snapshot of int64
  | Unlock_tables
  | Backup_stage_start
  | Backup_stage_block_commit
  | Backup_stage_end
  | Use of Quiver_core.Ident.t
  | Prepare_load
  | Check_unique
  | Skip_binlog
  | Create_progress_schema
  | Create_progress_table
  | Record_progress of { dump : string; target : string; file : string }
  | Forget_progress of { dump : string; target : string }
  | Begin
  | Commit
  | Rollback
  | Export of Quiver_core.Schema.Name.t
  | Discard_tablespace of Quiver_core.Schema.Name.t
  | Import_tablespace of Quiver_core.Schema.Name.t
  | Rename_table of { from : Quiver_core.Schema.Name.t; into : Quiver_core.Schema.Name.t }
  | Drop_table of Quiver_core.Schema.Name.t
  | Create_database of Quiver_core.Ident.t
  | Create_like of { table : Quiver_core.Schema.Name.t; like : Quiver_core.Schema.Name.t }
  | Create_table of { table : Quiver_core.Schema.Name.t; body : string }
  | Rebase of { table : Quiver_core.Schema.Name.t; allocator : allocator; next : int64 }
  | Analyze_table of Quiver_core.Schema.Name.t
  | Drop_indexes of { table : Quiver_core.Schema.Name.t; indexes : Quiver_core.Ident.t list }
  | Scatter_regions
  | Split_table of { table : Quiver_core.Ident.t; at : Quiver_core.Key.t * Quiver_core.Key.t list }
  | Load_local of {
      table : Quiver_core.Ident.t;
      dialect : Quiver_core.Output_format.dialect;
      columns : (Quiver_core.Ident.t * conversion) list;
    }

and conversion = Text_value | Hex_value | Number_value | Enum_value
[@@deriving sexp_of ~portable]

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
        | Tidb_8_5 _ ->
            "transaction_isolation = 'REPEATABLE-READ', \
             tidb_isolation_read_engines = 'tikv,tidb', max_execution_time = 0"
      and day_in_seconds = 86_400 in
      [%string
        "SET SESSION time_zone = '+00:00', sql_mode = '', net_read_timeout = \
         %{day_in_seconds#Int}, net_write_timeout = %{day_in_seconds#Int}, \
         %{isolation}"]
  | Flush_tables_with_read_lock -> "FLUSH TABLES WITH READ LOCK"
  | Start_consistent_snapshot -> "START TRANSACTION WITH CONSISTENT SNAPSHOT"
  | Pin_snapshot tso -> [%string "SET SESSION tidb_snapshot = '%{tso#Int64}'"]
  | Unlock_tables -> "UNLOCK TABLES"
  | Backup_stage_start -> "BACKUP STAGE START"
  | Backup_stage_block_commit -> "BACKUP STAGE BLOCK_COMMIT"
  | Backup_stage_end -> "BACKUP STAGE END"
  | Use database -> [%string "USE %{Quiver_core.Ident.backquoted database}"]
  | Prepare_load ->
      "SET SESSION foreign_key_checks = 0, unique_checks = 0, time_zone = \
       '+00:00', sql_mode = 'NO_AUTO_VALUE_ON_ZERO,ALLOW_INVALID_DATES', \
       lc_messages = 'en_US', max_error_count = 65535"
  | Check_unique -> "SET SESSION unique_checks = 1"
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
  | Export name -> [%string "FLUSH TABLES %{Query.qualified name} FOR EXPORT"]
  | Discard_tablespace name -> [%string "ALTER TABLE %{Query.qualified name} DISCARD TABLESPACE"]
  | Import_tablespace name -> [%string "ALTER TABLE %{Query.qualified name} IMPORT TABLESPACE"]
  | Rename_table { from; into } -> [%string "RENAME TABLE %{Query.qualified from} TO %{Query.qualified into}"]
  | Drop_table name -> [%string "DROP TABLE IF EXISTS %{Query.qualified name}"]
  | Analyze_table name -> [%string "ANALYZE TABLE %{Query.qualified name}"]
  | Create_database name -> [%string "CREATE DATABASE IF NOT EXISTS %{Quiver_core.Ident.backquoted name}"]
  | Create_like { table; like } -> [%string "CREATE TABLE %{Query.qualified table} LIKE %{Query.qualified like}"]
  | Create_table { table; body } -> [%string "CREATE TABLE %{Query.qualified table}%{body}"]
  | Rebase { table; allocator; next } ->
      let option = match allocator with Auto_increment -> "AUTO_INCREMENT" | Auto_random -> "AUTO_RANDOM_BASE" in
      [%string "ALTER TABLE %{Query.qualified table} %{option} = %{Int64.to_string next}"]
  | Drop_indexes { table; indexes } ->
      let dropped =
        String.concat ", " (List.map (fun index -> "DROP INDEX " ^ Quiver_core.Ident.backquoted index) indexes)
      in
      [%string "ALTER TABLE %{Query.qualified table} %{dropped}"]
  | Scatter_regions -> "SET SESSION tidb_scatter_region = 'table'"
  | Split_table { table; at = first, rest } ->
      let tuple (key : Quiver_core.Key.t) =
        let first, rest = (key :> Quiver_core.Key.value * Quiver_core.Key.value list) in
        [%string {|(%{String.concat ", " (List.map Query.literal (first :: rest))})|}]
      in
      [%string {|SPLIT TABLE %{Quiver_core.Ident.backquoted table} BY %{String.concat ", " (List.map tuple (first :: rest))}|}]
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
                 | Text_value | Enum_value -> value i
                 | Hex_value -> [%string "UNHEX(%{value i})"]
                 | Number_value -> [%string "CAST(%{value i} AS UNSIGNED)"]
               in
               [%string "%{Quiver_core.Ident.backquoted name} = %{value}"])
             columns)
      in
      [%string
        "LOAD DATA LOCAL INFILE 'quiver' INTO TABLE %{Quiver_core.Ident.backquoted table} \
         CHARACTER SET utf8mb4 %{format} (%{variables}) SET %{assignments}"]
