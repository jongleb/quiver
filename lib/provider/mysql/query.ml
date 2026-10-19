type collation = {
  charset : Quiver_core.Ident.t;
  collation : Quiver_core.Ident.t;
}
[@@deriving sexp_of ~portable]

type t =
  | Tables
  | Columns
  | Unique_indexes
  | Key_bounds of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Ident.t;
    }
  | Binlog_status of Server.t
  | Gtid_executed of Server.t
  | Show_create of Quiver_core.Ddl_target.t
  | View_columns of Quiver_core.Ident.t
  | Triggers of Quiver_core.Ident.t
  | Routines of Quiver_core.Ident.t
  | Events of Quiver_core.Ident.t
  | Show_definition of {
      database : Quiver_core.Ident.t;
      name : Quiver_core.Ident.t;
      kind : Quiver_core.Definition.kind;
    }
  | Chunk of Quiver_core.Chunk.t
  | Collation of {
      table : Quiver_core.Schema.Name.t;
      column : Quiver_core.Ident.t;
    }
  | Load_columns of Quiver_core.Schema.Name.t
  | Loaded_files of { dump : string; target : string }
  | Probe of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Schema.Field.t * Quiver_core.Schema.Field.t list;
      from : Quiver_core.Key.value;
      collation : collation option;
    }
  | Estimate of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Ident.t;
      from : Quiver_core.Key.value option;
      below : Quiver_core.Key.value option;
    }
  | Ordered_keys of {
      table : Quiver_core.Schema.Name.t;
      key : Quiver_core.Schema.Field.t * Quiver_core.Schema.Field.t list;
      candidates : Quiver_core.Key.t * Quiver_core.Key.t list;
    }
[@@deriving sexp_of ~portable]


let qualified { Quiver_core.Schema.Name.database; table } =
  [%string "%{Quiver_core.Ident.backquoted database}.%{Quiver_core.Ident.backquoted table}"]

let system = "('mysql', 'sys', 'information_schema', 'performance_schema')"

let columns fields =
  String.concat ", "
    (List.map (fun { Quiver_core.Schema.Field.name; _ } -> Quiver_core.Ident.backquoted name) fields)

let bytes value = [%string "X'%{Hex_encode.to_hex ~case:`Lowercase value}'"]
let text value = [%string "_utf8mb4 %{bytes value}"]

let literal (value : Quiver_core.Key.value) =
  let quoted render =
    let out = Quiver_core.Byte_buffer.create ~cap:32 () in
    render out;
    [%string "'%{Quiver_core.Byte_buffer.contents out}'"]
  in
  match value with
  | Signed value -> Int64.to_string value
  | Unsigned value -> Printf.sprintf "%Lu" value
  | Text value -> text value
  | Binary value -> bytes value
  | Exact value -> value
  | Day value -> quoted (fun out -> Quiver_core.Temporal.day out value)
  | Instant value -> quoted (fun out -> Quiver_core.Temporal.moment out value)
  | Duration micros ->
      quoted (fun out ->
          Quiver_core.Temporal.time out Quiver_core.Ltype.Fsp.microseconds
            ~micros:(Int64_u.of_int64 micros))

let sql = function
  | Tables ->
      [%string
        "SELECT TABLE_SCHEMA, TABLE_NAME, IFNULL(TABLE_ROWS, 0), \
         IFNULL(DATA_LENGTH, 0) FROM information_schema.TABLES WHERE \
         TABLE_TYPE = 'BASE TABLE' AND TABLE_SCHEMA NOT IN %{system} ORDER BY \
         TABLE_SCHEMA, TABLE_NAME"]
  | Columns ->
      [%string
        "SELECT TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME, DATA_TYPE, COLUMN_TYPE, \
         IS_NULLABLE, NUMERIC_PRECISION, NUMERIC_SCALE, DATETIME_PRECISION, \
         EXTRA FROM information_schema.COLUMNS WHERE TABLE_SCHEMA NOT IN \
         %{system} ORDER BY TABLE_SCHEMA, TABLE_NAME, ORDINAL_POSITION"]
  | Unique_indexes ->
      [%string
        "SELECT TABLE_SCHEMA, TABLE_NAME, INDEX_NAME, COLUMN_NAME FROM \
         information_schema.STATISTICS WHERE NON_UNIQUE = 0 AND TABLE_SCHEMA \
         NOT IN %{system} ORDER BY TABLE_SCHEMA, TABLE_NAME, INDEX_NAME, \
         SEQ_IN_INDEX"]
  | Key_bounds { table; key } ->
      [%string "SELECT MIN(%{Quiver_core.Ident.backquoted key}), MAX(%{Quiver_core.Ident.backquoted key}) FROM %{qualified table}"]
  | Binlog_status server -> (
      match server with
      | Mysql_8_0 _ -> "SHOW MASTER STATUS"
      | Mysql_8_4 _ -> "SHOW BINARY LOG STATUS"
      | Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _ ->
          "SHOW BINLOG STATUS")
  | Gtid_executed server -> (
      match server with
      | Mysql_8_0 _ | Mysql_8_4 _ -> "SELECT @@GLOBAL.gtid_executed"
      | Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _ ->
          "SELECT @@GLOBAL.gtid_binlog_pos")
  | Show_create (Database database) ->
      [%string "SHOW CREATE DATABASE %{Quiver_core.Ident.backquoted database}"]
  | Show_create (Table name) -> [%string "SHOW CREATE TABLE %{qualified name}"]
  | View_columns database ->
      [%string
        "SELECT c.TABLE_NAME, c.COLUMN_NAME FROM information_schema.COLUMNS c \
         JOIN information_schema.VIEWS v ON v.TABLE_SCHEMA = c.TABLE_SCHEMA \
         AND v.TABLE_NAME = c.TABLE_NAME WHERE c.TABLE_SCHEMA = %{text \
         (database :> string)} ORDER BY c.TABLE_NAME, c.ORDINAL_POSITION"]
  | Triggers database ->
      [%string
        "SELECT TRIGGER_NAME, EVENT_OBJECT_TABLE FROM \
         information_schema.TRIGGERS WHERE TRIGGER_SCHEMA = %{text (database \
         :> string)} ORDER BY EVENT_OBJECT_TABLE, ACTION_TIMING, \
         EVENT_MANIPULATION, ACTION_ORDER"]
  | Routines database ->
      [%string
        "SELECT ROUTINE_NAME, ROUTINE_TYPE FROM information_schema.ROUTINES \
         WHERE ROUTINE_SCHEMA = %{text (database :> string)} ORDER BY \
         ROUTINE_TYPE, ROUTINE_NAME"]
  | Events database ->
      [%string
        "SELECT EVENT_NAME FROM information_schema.EVENTS WHERE EVENT_SCHEMA = \
         %{text (database :> string)} ORDER BY EVENT_NAME"]
  | Show_definition { database; name; kind } ->
      let kind =
        match kind with
        | View _ -> "VIEW"
        | Trigger _ -> "TRIGGER"
        | Procedure -> "PROCEDURE"
        | Function -> "FUNCTION"
        | Event -> "EVENT"
      in
      [%string "SHOW CREATE %{kind} %{Quiver_core.Ident.backquoted database}.%{Quiver_core.Ident.backquoted name}"]
  | Chunk chunk ->
      let table = Quiver_core.Chunk.table chunk in
      let first, rest = table.fields in
      let where =
        match chunk with
        | Whole _ -> ""
        | Range _ ->
            let rec condition (predicate : Quiver_core.Predicate.t) =
              match predicate with
              | Always -> "TRUE"
              | Compare { field; comparison; value } ->
                  let comparison =
                    match comparison with
                    | Less -> "<"
                    | Equal -> "="
                    | Greater -> ">"
                    | At_least -> ">="
                  in
                  [%string "%{Quiver_core.Ident.backquoted field.name} %{comparison} %{literal value}"]
              | All (first, rest) ->
                  [%string {|(%{String.concat " AND " (List.map condition (first :: rest))})|}]
              | Any (first, rest) ->
                  [%string {|(%{String.concat " OR " (List.map condition (first :: rest))})|}]
            in
            [%string " WHERE %{condition (Quiver_core.Predicate.of_chunk chunk)}"]
      in
      [%string "SELECT %{columns (first :: rest)} FROM %{qualified table.name}%{where}"]
  | Collation { table = { database; table }; column } ->
      [%string
        "SELECT CHARACTER_SET_NAME, COLLATION_NAME FROM \
         information_schema.COLUMNS WHERE TABLE_SCHEMA = %{text (database :> \
         string)} AND TABLE_NAME = %{text (table :> string)} AND COLUMN_NAME = \
         %{text (column :> string)}"]
  | Load_columns { database; table } ->
      [%string
        "SELECT COLUMN_NAME, DATA_TYPE FROM information_schema.COLUMNS WHERE \
         TABLE_SCHEMA = %{text (database :> string)} AND TABLE_NAME = %{text \
         (table :> string)} AND EXTRA NOT LIKE '%GENERATED%' ORDER BY \
         ORDINAL_POSITION"]
  | Loaded_files { dump; target } ->
      [%string
        "SELECT file FROM quiver_load.progress WHERE dump = %{bytes dump} AND \
         target = %{text target}"]
  | Probe { table; key = (first, _) as key; from; collation } ->
      let from =
        Option.fold ~none:(literal from)
          ~some:(fun { charset; collation } ->
            [%string
              "CONVERT(%{literal from} USING %{Quiver_core.Ident.backquoted charset}) COLLATE %{Quiver_core.Ident.backquoted \
               collation}"])
          collation
      in
      [%string
        "SELECT %{columns (fst key :: snd key)} FROM %{qualified table} WHERE \
         %{Quiver_core.Ident.backquoted first.name} >= %{from} ORDER BY %{columns (fst key :: snd \
         key)} LIMIT 1"]
  | Estimate { table; key; from; below } ->
      let where =
        match
          Base.List.filter_opt
            [
              Option.map (fun from -> [%string "%{Quiver_core.Ident.backquoted key} >= %{literal from}"]) from;
              Option.map (fun below -> [%string "%{Quiver_core.Ident.backquoted key} < %{literal below}"]) below;
            ]
        with
        | [] -> ""
        | conditions -> [%string {| WHERE %{String.concat " AND " conditions}|}]
      in
      [%string "EXPLAIN SELECT 1 FROM %{qualified table}%{where}"]
  | Ordered_keys { table; key = first, rest; candidates = candidate, others } ->
      let fields = first :: rest in
      let tuple (bound : Quiver_core.Key.t) =
        let value, values =
          (bound :> Quiver_core.Key.value * Quiver_core.Key.value list)
        in
        let literals = List.map literal (value :: values) in
        match literals with
        | [ single ] -> single
        | _ -> [%string {|(%{String.concat ", " literals})|}]
      in
      let candidates = String.concat ", " (List.map tuple (candidate :: others))
      and key =
        match fields with
        | [ { name; _ } ] -> Quiver_core.Ident.backquoted name
        | _ -> [%string "(%{columns fields})"]
      in
      [%string
        "SELECT %{columns fields} FROM %{qualified table} WHERE %{key} IN \
         (%{candidates}) ORDER BY %{columns fields}"]
