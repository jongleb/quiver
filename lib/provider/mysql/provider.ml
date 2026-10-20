(** What a zero or calendar-invalid date ([0000-00-00], [2020-02-30]) does:
    [Reject] fails the dump; [Keep] reads every [DATE], [DATETIME] and
    [TIMESTAMP] as [Unchecked_date]/[Unchecked_datetime] and writes it as
    stored. *)
type zero_dates = Reject | Keep
[@@deriving to_string ~capitalize:"kebab-case" ~portable, enumerate]

type conn_config = {
  route : Link.route;
  user : string;
  password : string option;
  timeouts : Link.timeouts;
  zero_dates : zero_dates;
}

(** How the group of sessions is opened. *)
type snapshot_mode = Ftwrl | Backup_stage | Trx_only
[@@deriving sexp_of ~portable, to_string ~capitalize:"kebab-case" ~portable, enumerate]

type error =
  | Client of Client_error.t
  | Unknown_type of {
      table : Quiver_core.Schema.Name.t;
      column : Quiver_core.Ident.t;
      column_type : string;
    }
  | Schema_changed of Quiver_core.Schema.Name.t
  | Invalid_date of {
      table : Quiver_core.Schema.Name.t;
      column : Quiver_core.Ident.t;
    }
  | Non_positive_sessions of int
  | Gtid of Gtid.error
  | Malformed_binlog_file of string
  | Unmatched_filter of Quiver_core.Filter.rule
  | Unsupported_snapshot_mode of { mode : snapshot_mode; server : Server.t }
  | Unparsable_key of {
      table : Quiver_core.Schema.Name.t;
      column : Quiver_core.Ident.t;
      text : string;
    }
[@@deriving sexp_of ~portable]

type binlog_file = { base : string; index : int } [@@deriving sexp_of ~portable, yojson]

type binlog = { file : binlog_file; offset : int64; gtid : Gtid.t }
[@@deriving sexp_of ~portable, yojson]

type lock = Global_read_lock | Backup_stage_block_commit
[@@deriving sexp_of ~portable, yojson]

type position =
  | Locked of {
      lock : lock;
      binlog : binlog option;
      lock_held : Core.Time_ns.Span.t;
    }
  | Unlocked
[@@deriving sexp_of ~portable]

let yojson_of_position = function
  | Locked { lock; binlog; lock_held } ->
      `List
        [
          `String "Locked";
          `Assoc
            [
              ("lock", yojson_of_lock lock);
              ("binlog", yojson_of_option yojson_of_binlog binlog);
              ("lock_held", Quiver_core.Codec.yojson_of_span lock_held);
            ];
        ]
  | Unlocked -> `List [ `String "Unlocked" ]

let position_of_yojson = function
  | `List [ `String "Locked"; fields ] ->
      Locked
        {
          lock = Quiver_core.Codec.field "lock" lock_of_yojson fields;
          binlog =
            Quiver_core.Codec.field "binlog"
              (option_of_yojson binlog_of_yojson)
              fields;
          lock_held =
            Quiver_core.Codec.field "lock_held" Quiver_core.Codec.span_of_yojson
              fields;
        }
  | `List [ `String "Unlocked" ] -> Unlocked
  | json ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "Provider.position" json

type session = {
  conn : Conn.t;
  mutable database : Quiver_core.Ident.t option;
      (** The default database, set for [SHOW CREATE] of objects. *)
}

type _ supports =
  | With_ftwrl : Quiver_core.Consistency.snapshot supports
  | With_backup_stage : Quiver_core.Consistency.snapshot supports
  | Without_lock : Quiver_core.Consistency.best_effort supports

open Base.Result.Let_syntax

let client_failure result = Result.map_error (fun error -> Client error) result
let provider result = Result.map_error (fun error -> `Provider error) result
let client result = provider (client_failure result)

let unexpected query = Error (`Provider (Client (Unexpected_result query)))

let key_parser (ltype : Quiver_core.Ltype.t) =
  match ltype with
  | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 ->
      Some
        (fun text ->
          Option.map
            (fun v -> Quiver_core.Key.Signed v)
            (Int64.of_string_opt text))
  | Uint64 ->
      Some
        (fun text ->
          Option.map
            (fun v -> Quiver_core.Key.Unsigned v)
            (Int64.of_string_opt ("0u" ^ text)))
  | String -> Some (fun text -> Some (Quiver_core.Key.Text text))
  | Bytes -> Some (fun text -> Some (Quiver_core.Key.Binary text))
  | Decimal _ -> Some (fun text -> Some (Quiver_core.Key.Exact text))
  | Date ->
      Some
        (fun text ->
          Option.map
            (fun day -> Quiver_core.Key.Day day)
            (Quiver_core.Temporal.parse_date text))
  | Datetime _ | Timestamp _ ->
      Some
        (fun text ->
          Option.map
            (fun moment -> Quiver_core.Key.Instant moment)
            (Quiver_core.Temporal.parse_datetime text))
  | Time _ ->
      Some
        (fun text ->
          Option.map
            (fun micros -> Quiver_core.Key.Duration micros)
            (Quiver_core.Temporal.parse_time text))
  | Bool | Year | Unchecked_date | Unchecked_datetime _ | Float32 | Float64
  | Json | Geometry | Enum _ | Set _ ->
      None

let close { conn; _ } = Conn.close conn
let hang_up session = Quiver_core.Guarded.hold session (fun session -> close session)

type connections = {
  zero_dates : zero_dates;
  server : Server.t;
  control : session Await.Capsule.Await.With_mutex.t;
      (** Takes the lock while the sessions open their snapshots. *)
  sessions : session Await.Capsule.Await.With_mutex.t iarray;
  idle : session Await.Capsule.Await.With_mutex.t Await.Await.Mpmc_queue.t;
      (** Every connection no query holds, for introspection and planning. *)
}

let connect config (wanted : Quiver_core.Provider_intf.sessions) =
  let opened () =
    Quiver_core.Guarded.create (fun () ->
        let { route; user; password; timeouts; _ } = config in
        let%bind conn =
          client_failure (Conn.connect route ~timeouts ~user ~password)
        in
        match Conn.execute conn (Configure_session (Link.server (Conn.link conn))) with
        | Ok () -> Ok { conn; database = None }
        | Error error ->
            Conn.close conn;
            Error (Client error))
  in
  let most = match wanted with Exactly n | At_most n -> n in
  if most < 1 then Error (`Provider (Non_positive_sessions most))
  else
    let sessions, control =
      Quiver_core.Concurrent.both
        (fun () -> Quiver_core.Concurrent.each most (fun (_ : int) -> opened ()))
        opened
    in
    match
      Base.Result.combine control (Base.Result.all sessions)
        ~ok:(fun control sessions -> (control, sessions))
        ~err:(fun error _ -> error)
    with
    | Error error ->
        List.iter (Result.iter hang_up) (control :: sessions);
        Error (`Provider error)
    | Ok (control, sessions) -> (
        let server =
          Quiver_core.Guarded.hold control (fun { conn; _ } ->
              Link.server (Conn.link conn))
        in
        let suggested () =
          Quiver_core.Guarded.hold control (fun { conn; _ } ->
              Result.map
                (fun rows ->
                  match (server, rows) with
                  | Mysql_8_4 _, [ [| Some threads |] ] ->
                      Option.map (fun threads -> 2 * threads)
                        (int_of_string_opt threads)
                  | ( (Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _),
                      [ [| Some threads |] ] ) ->
                      int_of_string_opt threads
                  | _ -> None)
                (client_failure (Conn.rows conn (Read_threads server))))
        in
        let kept =
          match (wanted, server) with
          | Exactly _, _ | At_most _, Mysql_8_0 _ -> Ok most
          | At_most _, (Mysql_8_4 _ | Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _)
            ->
              Result.map
                (fun suggestion ->
                  Option.fold ~none:most ~some:(Int.min most)
                    (Base.Option.filter suggestion ~f:(fun threads -> threads > 0)))
                (suggested ())
        in
        match kept with
        | Error error ->
            List.iter hang_up (control :: sessions);
            Error (`Provider error)
        | Ok kept ->
            let #(sessions, surplus) = Base.List.split_n sessions kept in
            List.iter hang_up surplus;
            let idle = Await.Await.Mpmc_queue.create () in
            List.iter
              (fun (connection : session Await.Capsule.Await.With_mutex.t) ->
                Await.Await.Mpmc_queue.push idle connection)
              (control :: sessions);
            Ok
              {
                zero_dates = config.zero_dates;
                server;
                control;
                sessions = Iarray.of_list sessions;
                idle;
              })

let opened { sessions; _ } = Iarray.length sessions

let disconnect { control; sessions; _ } =
  hang_up control;
  Iarray.iter hang_up sessions

let fetch connections query =
  let held =
    Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await connections.idle
  in
  let rows =
    Quiver_core.Guarded.hold held (fun { conn; _ } ->
        Result.map (List.map Array.to_list) (client_failure (Conn.rows conn query)))
  in
  Await.Await.Mpmc_queue.push connections.idle held;
  rows

let supports : type lvl.
    snapshot_mode ->
    lvl Quiver_core.Consistency.t ->
    (lvl supports, [> `Consistency of Quiver_core.Consistency.error ]) result =
 fun mode level ->
  match (mode, level) with
  | Ftwrl, Snapshot -> Ok With_ftwrl
  | Backup_stage, Snapshot -> Ok With_backup_stage
  | Trx_only, Best_effort -> Ok Without_lock
  | (Ftwrl | Backup_stage), Best_effort ->
      Error (`Consistency (Unsupported (Any Best_effort)))
  | Trx_only, Snapshot -> Error (`Consistency (Unsupported (Any Snapshot)))

let introspect connections (filter : Quiver_core.Filter.t) =
  let tables, (columns, indexes) =
    Quiver_core.Concurrent.both
      (fun () -> fetch connections Tables)
      (fun () ->
        Quiver_core.Concurrent.both
          (fun () -> fetch connections Columns)
          (fun () -> fetch connections Unique_indexes))
  in
  let%bind tables = provider tables
  and columns = provider columns
  and indexes = provider indexes in
  let name database table =
    let%bind database = Quiver_core.Ident.of_string database in
    let%bind table = Quiver_core.Ident.of_string table in
    Ok { Quiver_core.Schema.Name.database; table }
  in
  let number =
    Option.fold ~none:0 ~some:(fun n ->
        Option.value ~default:0 (int_of_string_opt n))
  in
  let%bind fields =
    Base.List.fold_result columns ~init:[] ~f:(fun fields row ->
        match row with
        | [
            Some database;
            Some table;
            Some column;
            Some data_type;
            Some column_type;
            Some nullable;
            precision;
            scale;
            fsp;
            extra;
          ] -> (
            let%bind table = name database table in
            let%bind column = Quiver_core.Ident.of_string column in
            let words = String.split_on_char ' ' column_type in
            let members () =
              let quoted =
                Angstrom.(
                  char '\''
                  *> (many
                        (string "''" *> return "'"
                        <|> (char '\\' *> any_char >>| String.make 1)
                        <|> take_while1 (function
                          | '\'' | '\\' -> false
                          | _ -> true))
                     >>| String.concat "")
                  <* char '\'')
              in
              Result.map_error
                (fun _ ->
                  `Provider (Unknown_type { table; column; column_type }))
                Angstrom.(
                  parse_string ~consume:All
                    (skip_while (fun c -> c <> '(')
                     *> char '('
                     *> sep_by (char ',') quoted
                    <* char ')' <* end_of_input)
                    column_type)
            in
            let integer (signed : Quiver_core.Ltype.t) unsigned =
              if Base.List.mem words "unsigned" ~equal:String.equal then Ok unsigned else Ok signed
            in
            let fsp () = Quiver_core.Ltype.Fsp.of_int (number fsp) in
            let%bind ltype, splitting =
              match String.lowercase_ascii data_type with
              | "bit" -> Ok (Quiver_core.Ltype.Uint64, `Unsplittable)
              | other ->
                  let%map ltype =
                    match other with
                    | "tinyint" -> integer Int8 Uint8
                    | "smallint" -> integer Int16 Uint16
                    | "mediumint" | "int" -> integer Int32 Uint32
                    | "bigint" -> integer Int64 Uint64
                    | "decimal" ->
                        let%map decimal =
                          Quiver_core.Ltype.Decimal.make
                            ~precision:(number precision)
                            ~scale:(number scale)
                        in
                        Quiver_core.Ltype.Decimal decimal
                    | "float" -> Ok Float32
                    | "double" -> Ok Float64
                    | "char" | "varchar" | "tinytext" | "text"
                    | "mediumtext" | "longtext" ->
                        Ok String
                    | "binary" | "varbinary" | "tinyblob" | "blob"
                    | "mediumblob" | "longblob" ->
                        Ok Bytes
                    | "date" -> (
                        match connections.zero_dates with
                        | Reject -> Ok Date
                        | Keep -> Ok Unchecked_date)
                    | "time" ->
                        let%map fsp = fsp () in
                        Quiver_core.Ltype.Time fsp
                    | "datetime" -> (
                        let%map fsp = fsp () in
                        match connections.zero_dates with
                        | Keep -> Quiver_core.Ltype.Unchecked_datetime fsp
                        | Reject -> Quiver_core.Ltype.Datetime fsp)
                    | "timestamp" -> (
                        let%map fsp = fsp () in
                        match connections.zero_dates with
                        | Keep -> Quiver_core.Ltype.Unchecked_datetime fsp
                        | Reject -> Quiver_core.Ltype.Timestamp fsp)
                    | "year" -> Ok Year
                    | "json" -> Ok Json
                    | "geometry" | "point" | "linestring" | "polygon"
                    | "multipoint" | "multilinestring" | "multipolygon"
                    | "geometrycollection" | "geomcollection" ->
                        Ok Geometry
                    | "enum" ->
                        let%map members = members () in
                        Quiver_core.Ltype.Enum members
                    | "set" ->
                        let%map members = members () in
                        Quiver_core.Ltype.Set members
                    | _ ->
                        Error
                          (`Provider
                             (Unknown_type { table; column; column_type }))
                  in
                  (ltype, `Splittable)
            in
            let generated =
              Base.List.mem
                (String.split_on_char ' ' (Option.value ~default:"" extra))
                "GENERATED" ~equal:String.equal
            in
            if generated then Ok fields
            else
              let nullability : Quiver_core.Schema.Field.nullability =
                match nullable with "YES" -> Nullable | _ -> Not_null
              in
              Ok
                (( table,
                   ({ Quiver_core.Schema.Field.name = column; ltype; nullability }, splitting) )
                :: fields))
        | _ -> unexpected Columns)
  in
  let fields_by_table =
    Base.Hashtbl.create (module Quiver_core.Schema.Name)
  in
  List.iter
    (fun (table, field) ->
      Base.Hashtbl.add_multi fields_by_table ~key:table ~data:field)
    fields;
  let%bind indexes =
    Base.Result.all
      (List.map
         (function
           | [ Some database; Some table; Some index; column ] ->
               let%bind table = name database table in
               let%bind index = Quiver_core.Ident.of_string index in
               let%bind column =
                 Option.fold ~none:(Ok None)
                   ~some:(fun column ->
                     Result.map Option.some
                       (Quiver_core.Ident.of_string column))
                   column
               in
               Ok (table, index, column)
           | _ -> unexpected Unique_indexes)
         indexes)
  in
  let indexes_by_table =
    Base.Hashtbl.create (module Quiver_core.Schema.Name)
  in
  List.iter
    (fun (table, index, column) ->
      Base.Hashtbl.add_multi indexes_by_table ~key:table
        ~data:(index, column))
    (List.rev indexes);
  let module Idents = Set.Make (Quiver_core.Ident) in
  let module Names = Set.Make (Quiver_core.Schema.Name) in
  let databases, names =
    match filter with
    | Everything -> (Idents.empty, Names.empty)
    | Only (first, rest) ->
        List.fold_left
          (fun (databases, names) -> function
            | Quiver_core.Filter.Database database ->
                (Idents.add database databases, names)
            | Table name -> (databases, Names.add name names))
          (Idents.empty, Names.empty)
          (first :: rest)
  in
  let admitted (name : Quiver_core.Schema.Name.t) =
    match filter with
    | Everything -> true
    | Only _ -> Idents.mem name.database databases || Names.mem name names
  in
  let%bind tables =
    Base.Result.all
      (List.map
         (function
           | [ Some database; Some table; Some rows; Some bytes ] ->
               let%map name = name database table in
               (name, number (Some rows), number (Some bytes))
           | _ -> unexpected Tables)
         tables)
  in
  let%bind () =
    match filter with
    | Everything -> Ok ()
    | Only (first, rest) ->
        let present_databases =
          Idents.of_list
            (List.map
               (fun ((name : Quiver_core.Schema.Name.t), _, _) ->
                 name.database)
               tables)
        and present_names =
          Names.of_list (List.map (fun (name, _, _) -> name) tables)
        in
        Option.fold ~none:(Ok ())
          ~some:(fun rule -> Error (`Provider (Unmatched_filter rule)))
          (List.find_opt
             (function
               | Quiver_core.Filter.Database database ->
                   not (Idents.mem database present_databases)
               | Table name -> not (Names.mem name present_names))
             (first :: rest))
  in
  let%bind tables =
    Base.Result.all
      (List.map
         (fun (name, rows, bytes) ->
           let columns = Base.Hashtbl.find_multi fields_by_table name in
           let fields = List.map fst columns in
           let keyable =
             Idents.of_list
               (List.filter_map
                  (fun ( ({ name; nullability; ltype } :
                           Quiver_core.Schema.Field.t),
                         literals ) ->
                    match (nullability, literals, key_parser ltype) with
                    | Not_null, `Splittable, Some _ -> Some name
                    | Nullable, _, _ | _, `Unsplittable, _ | _, _, None ->
                        None)
                  columns)
           in
           let usable columns =
             List.for_all
               (Base.Option.exists ~f:(fun column -> Idents.mem column keyable))
               columns
           in
           let candidates =
             List.filter_map
               (fun group ->
                 match group with
                 | (index, _) :: _ -> (
                     let columns = List.map snd group in
                     match columns with
                     | Some first :: rest when usable columns ->
                         Some (index, (first, Base.List.filter_opt rest))
                     | _ -> None)
                 | [] -> None)
               (Base.List.group
                  (Base.Hashtbl.find_multi indexes_by_table name)
                  ~break:(fun (a, _) (b, _) ->
                    not (Quiver_core.Ident.equal a b)))
           in
           let key =
             Option.value
               ~default:
                 (Option.fold ~none:Quiver_core.Schema.Table.No_key
                    ~some:(fun (index, fields) ->
                      Quiver_core.Schema.Table.Unique { index; fields })
                    (Base.List.hd candidates))
               (let open Base.Option.Let_syntax in
                let%map fields =
                  List.find_map
                    (fun ((index : Quiver_core.Ident.t), fields) ->
                      match (index :> string) with
                      | "PRIMARY" -> Some fields
                      | _ -> None)
                    candidates
                in
                Quiver_core.Schema.Table.Primary fields)
           in
           Quiver_core.Schema.Table.make ~name ~fields ~key
             ~estimate:{ rows; bytes })
         (List.filter (fun (name, _, _) -> admitted name) tables))
  in
  Quiver_core.Schema.make tables

(** Why planning a table fails. *)
type planning =
  [ `Provider of error
  | `Chunk of Quiver_core.Chunk.error
  | `Key of Quiver_core.Key.error
  | `Ident of Quiver_core.Ident.error ]

let plan connections (schema : Quiver_core.Schema.t)
    ({ chunk_bytes } : Quiver_core.Plan_opts.t) =
  let connections_count = Iarray.length connections.sessions + 1
  and planning_tables = Portable.Atomic.make 0
  and largest_first =
    List.stable_sort
      (fun (_, (a : Quiver_core.Schema.Table.t)) (_, b) ->
        Int.compare b.estimate.bytes a.estimate.bytes)
      (List.mapi (fun position table -> (position, table)) schema.tables)
  in
  let%map chunks =
    Result.map_error (function #planning as error -> error)
    @@ Base.Result.all
      (List.map snd
         (List.sort
            (fun (a, _) (b, _) -> Int.compare a b)
            (List.combine
               (List.map fst largest_first)
               (Quiver_core.Concurrent.spread ~workers:connections_count
                  (Iarray.of_list (List.map snd largest_first))
                  (fun (table : Quiver_core.Schema.Table.t)
                     : (Quiver_core.Chunk.t list, planning) result ->
               let workers =
                 Int.max 1
                   (connections_count
                   / (Portable.Atomic.fetch_and_add planning_tables 1 + 1))
               and planned chunks =
                 Portable.Atomic.decr planning_tables;
                 chunks
               in
               planned
               @@
               let whole = Ok [ Quiver_core.Chunk.whole table ] in
               let count =
                 Int.max 1
                   ((table.estimate.bytes + chunk_bytes - 1) / chunk_bytes)
               in
               let value (field : Quiver_core.Schema.Field.t) text =
                 let unparsable =
                   match field.ltype with
                   | Date | Datetime _ | Timestamp _ ->
                       Invalid_date { table = table.name; column = field.name }
                   | Bool | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64
                   | Float32 | Float64 | Decimal _ | String | Bytes | Year | Unchecked_date
                   | Unchecked_datetime _ | Time _ | Json | Geometry | Enum _ | Set _ ->
                       Unparsable_key { table = table.name; column = field.name; text }
                 in
                 Option.to_result ~none:(`Provider unparsable)
                   (Option.bind (key_parser field.ltype) (fun parse -> parse text))
               in
               let tuple fields query row =
                 match Base.List.zip fields row with
                 | Unequal_lengths -> unexpected query
                 | Ok pairs ->
                     Base.Result.all
                       (List.map
                          (fun (field, cell) ->
                            Option.fold ~none:(unexpected query)
                              ~some:(value field) cell)
                          pairs)
               in
               match table.key with
               | No_key -> whole
               | _ when count <= 1 -> whole
               | Primary ((first, rest) as key)
               | Unique { fields = (first, rest) as key; _ } -> (
                   match
                     Base.Option.all
                       (List.map
                          (fun (field : Quiver_core.Schema.Field.t) ->
                            Option.map
                              (fun (_ : string -> Quiver_core.Key.value option) ->
                                field)
                              (key_parser field.ltype))
                          (first :: rest))
                   with
                   | None | Some [] -> whole
                   | Some (leading :: _ as parsed) -> (
                       let query =
                         Query.Key_bounds
                           { table = table.name; key = first.name }
                       in
                       let%bind rows = provider (fetch connections query) in
                       match rows with
                       | [ [ None; None ] ] -> whole
                       | [ [ Some min; Some max ] ] -> (
                           let%bind low = value leading min
                           and high = value leading max in
                           let bisected (wrap @ portable) ~min ~max =
                             let rows_column =
                               match connections.server with
                               | Mysql_8_0 _ | Mysql_8_4 _ -> 9
                               | Mariadb_10_11 _ | Mariadb_11_4 _
                               | Mariadb_11_8 _ ->
                                   8
                             in
                             let weigh from below : (int, planning) result =
                               let query =
                                 Query.Estimate
                                   {
                                     table = table.name;
                                     key = first.name;
                                     from = Option.map wrap from;
                                     below = Option.map wrap below;
                                   }
                               in
                               let%bind rows = provider (fetch connections query) in
                               match rows with
                               | [] -> Ok 0
                               | [ row ] -> (
                                   match List.nth_opt row rows_column with
                                   | None -> unexpected query
                                   | Some None -> Ok 0
                                   | Some (Some text) ->
                                       Option.fold ~none:(unexpected query)
                                         ~some:Result.ok
                                         (int_of_string_opt text))
                               | _ -> unexpected query
                             in
                             let middle lo hi =
                               Ok
                                 (Quiver_core.Bisect.integer_middle ~min ~max lo
                                    hi)
                             in
                             let%bind total = weigh None None in
                             let%bind bounds =
                               let spare_cuts = 16 in
                               Quiver_core.Bisect.boundaries ~workers ~weigh
                                 ~middle
                                 ~target:
                                   (Int.max 1 ((total + count - 1) / count))
                                 ~limit:((4 * count) + spare_cuts)
                             in
                             Quiver_core.Chunk.of_bounds table
                               (List.map (fun bound -> [ wrap bound ]) bounds)
                           in
                           match (rest, low, high) with
                           | [], Signed min, Signed max ->
                               bisected
                                 (fun bound -> Quiver_core.Key.Signed bound)
                                 ~min ~max
                           | [], Unsigned min, Unsigned max ->
                               bisected
                                 (fun bound -> Quiver_core.Key.Unsigned bound)
                                 ~min ~max
                           | _ -> (
                               let%bind collation =
                                 match first.ltype with
                                 | String -> (
                                     let query =
                                       Query.Collation
                                         {
                                           table = table.name;
                                           column = first.name;
                                         }
                                     in
                                     let%bind rows = provider (fetch connections query) in
                                     match rows with
                                     | [ [ Some charset; Some collation ] ] ->
                                         let%bind charset =
                                           Quiver_core.Ident.of_string charset
                                         and collation =
                                           Quiver_core.Ident.of_string collation
                                         in
                                         Ok (Some { Query.charset; collation })
                                     | _ -> unexpected query)
                                 | _ -> Ok None
                               in
                               let%bind found =
                                 Base.Result.all
                                   (Quiver_core.Concurrent.spread ~workers
                                      (Iarray.of_list
                                         (Quiver_core.Probe.between low high
                                            ~count))
                                      (fun from
                                         : ( Quiver_core.Key.value list option,
                                             planning )
                                           result ->
                                        let query =
                                          Query.Probe
                                            {
                                              table = table.name;
                                              key;
                                              from;
                                              collation;
                                            }
                                        in
                                        let%bind rows = provider (fetch connections query) in
                                        match rows with
                                        | [] -> Ok None
                                        | [ row ] ->
                                            let%map found =
                                              tuple parsed query row
                                            in
                                            Some found
                                        | _ -> unexpected query))
                               in
                               match
                                 Base.List.remove_consecutive_duplicates
                                   ~equal:
                                     (List.equal Quiver_core.Key.equal_value)
                                   (Base.List.filter_opt found)
                               with
                               | [] -> whole
                               | candidate :: others ->
                                   let%bind candidate =
                                     Quiver_core.Key.make key candidate
                                   and others =
                                     Base.Result.all
                                       (List.map (Quiver_core.Key.make key)
                                          others)
                                   in
                                   let query =
                                     Query.Ordered_keys
                                       {
                                         table = table.name;
                                         key;
                                         candidates = (candidate, others);
                                       }
                                   in
                                   let%bind rows = provider (fetch connections query) in
                                   let%bind bounds =
                                     Base.Result.all
                                       (List.map (tuple parsed query) rows)
                                   in
                                   Quiver_core.Chunk.of_bounds table bounds))
                       | _ -> unexpected query)))))))
  in
  List.concat chunks

let snapshot (type lvl) (supports : lvl supports) connections =
  let { server; control; sessions; _ } = connections in
  let execute statement =
    provider
      (Quiver_core.Guarded.hold control (fun { conn; _ } ->
           client_failure (Conn.execute conn statement)))
  in
  let read_position () =
    let%bind status, gtids =
      provider
        (Quiver_core.Guarded.hold control (fun { conn; _ } ->
             let rows query =
               Result.map (List.map Array.to_list)
                 (client_failure (Conn.rows conn query))
             in
             let%bind status = rows (Binlog_status server) in
             let%map gtids = rows (Gtid_executed server) in
             (status, gtids)))
    in
    match (status, gtids) with
    | [], _ -> Ok None
    | (file :: offset :: _) :: _, [ [ gtid ] ] -> (
        let%bind gtid =
          Result.map_error
            (fun (`Gtid error) -> `Provider (Gtid error))
            (Gtid.of_server_string server (Option.value ~default:"" gtid))
        in
        match (file, Option.bind offset Int64.of_string_opt) with
        | Some file, Some offset ->
            Option.to_result
              ~none:(`Provider (Malformed_binlog_file file))
              (let open Base.Option.Let_syntax in
               let%bind base, index = Base.String.rsplit2 file ~on:'.' in
               let%map index = int_of_string_opt index in
               Some { file = { base; index }; offset; gtid })
        | _ -> unexpected (Binlog_status server))
    | _ -> unexpected (Binlog_status server)
  in
  let started () =
    Base.Result.all_unit
      (Quiver_core.Concurrent.spread ~workers:(Iarray.length sessions) sessions
         (fun session ->
           Quiver_core.Guarded.hold session (fun { conn; _ } ->
               client_failure (Conn.execute conn Start_consistent_snapshot))))
  in
  let opened =
    let%bind lock =
      match (supports, server) with
      | With_ftwrl, _ ->
          Ok
            (Some
               ( Global_read_lock,
                 [ Statement.Flush_tables_with_read_lock ],
                 Statement.Unlock_tables ))
      | With_backup_stage, (Mariadb_10_11 _ | Mariadb_11_4 _ | Mariadb_11_8 _)
        ->
          Ok
            (Some
               ( Backup_stage_block_commit,
                 [ Statement.Backup_stage_start; Backup_stage_block_commit ],
                 Statement.Backup_stage_end ))
      | With_backup_stage, (Mysql_8_0 _ | Mysql_8_4 _) ->
          Error
            (`Provider (Unsupported_snapshot_mode { mode = Backup_stage; server }))
      | Without_lock, _ -> Ok None
    in
    match lock with
    | None ->
        let%map () = provider (started ()) in
        Unlocked
    | Some (lock, acquire, release) ->
        let%bind () =
          Base.List.fold_result acquire ~init:() ~f:(fun () statement ->
              execute statement)
        in
        let acquired = Quiver_core.Stopwatch.start () in
        let started, binlog = Quiver_core.Concurrent.both started read_position in
        let released = execute release in
        let lock_held = Quiver_core.Stopwatch.elapsed acquired in
        let%bind () = provider started in
        let%bind binlog = binlog in
        let%map () = released in
        Locked { lock; binlog; lock_held }
  in
  hang_up control;
  match opened with
  | Ok position -> Ok (Iarray.to_array sessions, position)
  | Error error ->
      Iarray.iter hang_up sessions;
      Error error

let ddl { conn; _ } (target : Quiver_core.Ddl_target.t) =
  let query = Query.Show_create target in
  let%bind rows = client (Conn.rows conn query) in
  match rows with
  | [ [| _; Some ddl |] ] | [ [| _; Some ddl; _; _ |] ] -> Ok ddl
  | _ -> unexpected query

let objects { conn; _ } database =
  let names query decode =
    let%bind rows = client (Conn.rows conn query) in
    Base.Result.all
      (List.map
         (fun row ->
           Option.value ~default:(unexpected query) (decode row))
         rows)
  in
  let%bind columns =
    names (View_columns database) (function
      | [| Some view; Some column |] ->
          Some
            (let%bind view = Quiver_core.Ident.of_string view in
             let%map column = Quiver_core.Ident.of_string column in
             (view, column))
      | _ -> None)
  and triggers =
    names (Triggers database) (function
      | [| Some name; Some table |] ->
          Some
            (let%bind name = Quiver_core.Ident.of_string name in
             let%map table = Quiver_core.Ident.of_string table in
             (name, Quiver_core.Definition.Trigger { table }))
      | _ -> None)
  and routines =
    names (Routines database) (function
      | [| Some name; Some "PROCEDURE" |] ->
          Some
            (let%map name = Quiver_core.Ident.of_string name in
             (name, Quiver_core.Definition.Procedure))
      | [| Some name; Some "FUNCTION" |] ->
          Some
            (let%map name = Quiver_core.Ident.of_string name in
             (name, Quiver_core.Definition.Function))
      | _ -> None)
  and events =
    names (Events database) (function
      | [| Some name |] ->
          Some
            (let%map name = Quiver_core.Ident.of_string name in
             (name, Quiver_core.Definition.Event))
      | _ -> None)
  in
  let views =
    List.filter_map
      (function
        | (view, first) :: rest ->
            Some
              ( view,
                Quiver_core.Definition.View
                  { columns = (first, List.map snd rest) } )
        | [] -> None)
      (Base.List.group columns ~break:(fun (a, _) (b, _) ->
           not (Quiver_core.Ident.equal a b)))
  in
  Ok (views @ triggers @ routines @ events)

let definition session database (name, (kind : Quiver_core.Definition.kind)) =
  let%bind () =
    match session.database with
    | Some current when Quiver_core.Ident.equal current database -> Ok ()
    | Some _ | None ->
        let%map () = client (Conn.execute session.conn (Use database)) in
        session.database <- Some database
  in
  let query = Query.Show_definition { database; name; kind } in
  let%bind rows = client (Conn.rows session.conn query) in
  let context charset collation sql_mode time_zone =
    let%bind charset = Quiver_core.Ident.of_string charset in
    let%map collation = Quiver_core.Ident.of_string collation in
    { Quiver_core.Definition.charset; collation; sql_mode; time_zone }
  in
  let%map context, text =
    match (kind, rows) with
    | View _, [ [| _; Some text; Some charset; Some collation |] ] ->
        let%map context = context charset collation None None in
        (context, text)
    | ( Trigger _,
        [
          [|
            _; sql_mode; Some text; Some charset; Some collation; _; _;
          |];
        ] )
    | ( (Procedure | Function),
        [ [| _; sql_mode; Some text; Some charset; Some collation; _ |] ]
      ) ->
        let%map context = context charset collation sql_mode None in
        (context, text)
    | ( Event,
        [
          [|
            _;
            sql_mode;
            time_zone;
            Some text;
            Some charset;
            Some collation;
            _;
          |];
        ] ) ->
        let%map context = context charset collation sql_mode time_zone in
        (context, text)
    | (View _ | Trigger _ | Procedure | Function | Event), _ ->
        unexpected query
  in
  { Quiver_core.Definition.database; name; kind; context; text }

(** How one result column turns into a batch value. *)
type decoder =
  | Small of Rows.width * Rows.sign
  | Big
  | Bits
  | Single
  | Double
  | Day
  | Instant
  | Unchecked_day
  | Unchecked_instant
  | Duration
  | Text

let key_span { conn; _ } chunk =
  match Quiver_core.Chunk.integer_key chunk with
  | None -> Ok None
  | Some (field : Quiver_core.Schema.Field.t) -> (
      let query = Query.Key_span { chunk; key = field.name } in
      let%bind rows = client (Conn.rows conn query) in
      match (rows, key_parser field.ltype) with
      | [ [| None; None |] ], _ -> Ok None
      | [ [| Some lo; Some hi |] ], Some parse -> (
          match (parse lo, parse hi) with
          | Some lo, Some hi -> Ok (Some (lo, hi))
          | _ -> unexpected query)
      | _ -> unexpected query)

let read_chunk { conn; _ } (chunk : Quiver_core.Chunk.t) writer =
  let table = Quiver_core.Chunk.table chunk in
  let first, rest = table.fields in
  let fields = Array.of_list (first :: rest) in
  let query = Query.Chunk chunk in
  let failed = function
    | (`Link _ | `Wire _ | `Transport _) as failure ->
        `Provider (Client (Client_error.of_failure (Fetch query) failure))
    | `Written error -> error
    | (`Provider _ | `Column _) as other -> other
  in
  let%bind rows =
    Result.map_error failed (Rows.open_ (Conn.link conn) (Query.sql query))
  in
  let result =
    let changed = Error (`Provider (Schema_changed table.name)) in
    let%bind decoders =
      if Rows.width rows <> Array.length fields then changed
      else
        let%map decoders =
          Base.Result.all
            (List.map
               (fun (j, { Quiver_core.Schema.Field.ltype; _ }) ->
                 match (ltype, Rows.kind rows j) with
                 | ( (Int8 | Int16 | Int32 | Uint8 | Uint16 | Uint32 | Year),
                     Integer { width = (W1 | W2 | W4) as width; sign } ) ->
                     Ok (Small (width, sign))
                 | (Int64 | Uint64), Integer { width = W8; _ } -> Ok Big
                 | Uint64, Bits -> Ok Bits
                 | Float32, Float32 -> Ok Single
                 | Float64, Float64 -> Ok Double
                 | Date, Date -> Ok Day
                 | (Datetime _ | Timestamp _), Datetime -> Ok Instant
                 | Unchecked_date, Date -> Ok Unchecked_day
                 | Unchecked_datetime _, Datetime -> Ok Unchecked_instant
                 | Time _, Time -> Ok Duration
                 | ( ( Decimal _ | String | Bytes | Json | Geometry | Enum _
                     | Set _ ),
                     Text ) ->
                     Ok Text
                 | _ -> changed)
               (List.mapi (fun j field -> (j, field)) (Array.to_list fields)))
        in
        Array.of_list decoders
    in
    let[@inline never] [@zero_alloc assume error] refuse j error =
      match error with
      | `Invalid_date ->
          Error
            (`Provider
               (Invalid_date { table = table.name; column = fields.(j).name }))
      | `Column error -> Error (`Column error)
    in
    let[@inline] dated j result =
      match result with Ok () -> Ok () | Error error -> refuse j error
    in
    let[@zero_alloc] cell reader bitmap j (Quiver_core.Column.Any column) =
      match Rows.presence reader ~bitmap j with
      | Null ->
          Quiver_core.Column.add_null column;
          Ok ()
      | Present -> (
          match (decoders.(j), Quiver_core.Column.repr column) with
          | Small (width, sign), Int ->
              Rows.add_integer reader width sign column
          | Big, Int64 -> Rows.add_int64 reader column
          | Bits, Int64 -> Rows.add_bits reader column
          | Single, Float -> Rows.add_float32 reader column
          | Double, Float -> Rows.add_float64 reader column
          | Day, Int -> dated j (Rows.add_date reader column)
          | Instant, Int64 -> dated j (Rows.add_datetime reader column)
          | Unchecked_day, Int ->
              dated j (Rows.add_unchecked_date reader column)
          | Unchecked_instant, Int64 ->
              dated j (Rows.add_unchecked_datetime reader column)
          | Duration, Int64 -> Rows.add_time reader column
          | Text, Text -> Rows.add_text reader column
          | ( ( Small _ | Big | Bits | Single | Double | Day | Instant
              | Unchecked_day | Unchecked_instant | Duration | Text ),
              _ ) ->
              changed)
    in
    let width = Array.length fields in
    let[@zero_alloc] rec cells reader bitmap
        (columns : Quiver_core.Column.any Iarray.t @ local) j =
      if j = width then Ok ()
      else
        match cell reader bitmap j (Iarray.get columns j) with
        | Ok () -> cells reader bitmap columns (j + 1)
        | Error _ as error -> error
    in
    Quiver_core.Batch_writer.fill writer (fun columns ->
        Result.map_error failed
          (Rows.next rows ~row:(fun reader ~bitmap ->
               cells reader bitmap columns 0)))
  in
  match (result, Rows.close rows) with
  | Ok (), Ok () -> Ok ()
  | Error error, _ -> Error error
  | Ok (), Error error -> Error (failed error)

let source position =
  "[source]\n\
   # Channel_Name = '' # It can be use to setup replication FOR CHANNEL\n"
  ^
  match position with
  | Unlocked -> "# quiver: sessions opened without a lock, no common position\n"
  | Locked { lock; binlog; lock_held } ->
      Option.fold ~none:""
        ~some:(fun { file = { base; index }; offset; gtid } ->
          let variable =
            match gtid with Gtid.Mysql _ -> "executed_gtid_set" | Gtid.Mariadb _ -> "gtid_slave_pos"
          in
          Printf.sprintf
            "# %s = \"%s\"\n\
             # SOURCE_LOG_FILE = \"%s.%06d\"\n\
             # SOURCE_LOG_POS = %Ld\n"
            variable (Gtid.to_string gtid) base index offset)
        binlog
      ^ Format.asprintf "# quiver: %s held for %a\n"
          (Sexplib0.Sexp.to_string (sexp_of_lock lock))
          Core.Time_ns.Span.pp lock_held
