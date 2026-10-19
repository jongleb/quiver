open Base.Result.Let_syntax

type t = {
  dir : File_path.t;
  compression : Compression.t;
  buffers : Quiver_core.Buffer_pool.t;
  started : Core.Time_ns.t;
  views : Quiver_core.Schema.Name.t list Portable.Atomic.t;
      (** Views written so far, listed in [metadata] for myloader. *)
}

let create ~dir compression =
  match Core_unix.mkdir_p (File_path.to_string dir) with
  | () ->
      Ok
        {
          dir;
          compression;
          buffers = Quiver_core.Buffer_pool.create ();
          started = Core.Time_ns.now ();
          views = Portable.Atomic.make [];
        }
  | exception Unix.Unix_error (error, _, _) ->
      Error
        (`Output (Output.Io { file = dir; message = Unix.error_message error }))

let compression t = t.compression
let buffers t = t.buffers

let file_name (name : Quiver_core.Ident.t) =
  Base.String.concat_map
    (name :> string)
    ~f:(function
      | ('A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '-') as c ->
          String.make 1 c
      | c -> Printf.sprintf "%%%02X" (Char.code c))

type script = { name : string; statements : string list }

let restore_sql_mode = "'NO_AUTO_VALUE_ON_ZERO,ALLOW_INVALID_DATES'"

let header =
  [%string
    "/*!40101 SET NAMES utf8mb4*/;\n\
     /*!40014 SET FOREIGN_KEY_CHECKS=0*/;\n\
     /*!40101 SET SQL_MODE=%{restore_sql_mode}*/;\n\
     /*!40103 SET TIME_ZONE='+00:00'*/;\n"]

let schema t name text =
  Output.save
    File_path.Operators.(
      t.dir /?. ~.[%string "%{name}.sql%{Compression.extension t.compression}"])
    t.compression text

let database t database ~ddl =
  schema t
    [%string "%{file_name database}-schema-create"]
    [%string "%{header}%{ddl};\n"]

let table t ({ name = { database; table }; _ } : Quiver_core.Schema.Table.t)
    ~ddl =
  schema t
    [%string "%{file_name database}.%{file_name table}-schema"]
    [%string "%{header}%{ddl};\n"]

let scripts database definitions =
  let literal text = [%string {|'%{Base.String.substr_replace_all text ~pattern:"'" ~with_:"''"}'|}] in
  let wrapped
      ({ context = { charset; collation; sql_mode; time_zone }; text; name; _ } :
        Quiver_core.Definition.t) before =
    let%map text =
      Base.Result.of_option
        (Myloader.statement (Myloader.escapes sql_mode) text)
        ~error:(`Unsplittable { Quiver_core.Schema.Name.database; table = name })
    in
    [
      "SET @PREV_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT";
      "SET @PREV_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS";
      "SET @PREV_COLLATION_CONNECTION=@@COLLATION_CONNECTION";
      "SET character_set_client = " ^ (charset :> string);
      "SET character_set_results = " ^ (charset :> string);
      "SET collation_connection = " ^ (collation :> string);
    ]
    @ Option.fold ~none:[]
        ~some:(fun mode ->
          [ "SET @PREV_SQL_MODE=@@SQL_MODE"; "SET SQL_MODE=" ^ literal mode ])
        sql_mode
    @ Option.fold ~none:[]
        ~some:(fun zone ->
          [ "SET @PREV_TIME_ZONE=@@TIME_ZONE"; "SET TIME_ZONE=" ^ literal zone ])
        time_zone
    @ before @ [ text ]
    @ Option.fold ~none:[]
        ~some:(fun _ -> [ "SET TIME_ZONE=@PREV_TIME_ZONE" ])
        time_zone
    @ Option.fold ~none:[]
        ~some:(fun _ -> [ "SET SQL_MODE=@PREV_SQL_MODE" ])
        sql_mode
    @ [
        "SET character_set_client = @PREV_CHARACTER_SET_CLIENT";
        "SET character_set_results = @PREV_CHARACTER_SET_RESULTS";
        "SET collation_connection = @PREV_COLLATION_CONNECTION";
      ]
  in
  let file name statements =
    match statements with [] -> [] | _ -> [ { name; statements } ]
  in
  let views =
    List.filter_map
      (fun ({ name; kind; _ } as definition : Quiver_core.Definition.t) ->
        match kind with
        | View { columns } -> Some (definition, name, columns)
        | Trigger _ | Procedure | Function | Event -> None)
      definitions
  in
  let base name = [%string "%{file_name database}.%{file_name name}"] in
  let stubs =
    List.concat_map
      (fun (_, name, (first, rest)) ->
        file
          (base name ^ "-schema")
          (let columns =
             String.concat ",\n"
               (List.map (fun column -> Quiver_core.Ident.backquoted column ^ " int") (first :: rest))
           in
           [
             [%string
               "CREATE TABLE IF NOT EXISTS %{Quiver_core.Ident.backquoted name}(\n%{columns}\n) \
                ENGINE=MEMORY"];
           ]))
      views
  in
  let%bind views =
    Base.Result.all
      (List.map
         (fun (definition, name, _) ->
           let%map view =
             wrapped definition
               [
                 "DROP TABLE IF EXISTS " ^ Quiver_core.Ident.backquoted name;
                 "DROP VIEW IF EXISTS " ^ Quiver_core.Ident.backquoted name;
               ]
           in
           file (base name ^ "-schema-view") view)
         views)
  in
  let dropped keyword ({ name; _ } as definition : Quiver_core.Definition.t) =
    wrapped definition [ "DROP " ^ keyword ^ " IF EXISTS " ^ Quiver_core.Ident.backquoted name ]
  in
  let%bind triggers =
    Base.Result.all
      (List.map (dropped "TRIGGER")
         (List.filter
            (fun (definition : Quiver_core.Definition.t) ->
              match definition.kind with
              | Trigger _ -> true
              | View _ | Procedure | Function | Event -> false)
            definitions))
  in
  let%map post =
    Base.Result.all
      (List.map
         (fun (definition : Quiver_core.Definition.t) ->
           match definition.kind with
           | Procedure ->
               Result.map Option.some (dropped "PROCEDURE" definition)
           | Function -> Result.map Option.some (dropped "FUNCTION" definition)
           | Event -> Result.map Option.some (dropped "EVENT" definition)
           | View _ | Trigger _ -> Ok None)
         definitions)
  in
  stubs @ List.concat views
  @ file (file_name database ^ "-schema-triggers") (List.concat triggers)
  @ file
      (file_name database ^ "-schema-post")
      (List.concat (List.filter_map Fun.id post))

let objects t database definitions =
  let%bind scripts = scripts database definitions in
  List.iter
    (fun ({ name; kind; _ } : Quiver_core.Definition.t) ->
      match kind with
      | View _ ->
          Portable.Atomic.update t.views ~pure_f:(fun views ->
              { Quiver_core.Schema.Name.database; table = name } :: views)
      | Trigger _ | Procedure | Function | Event -> ())
    definitions;
  Base.List.fold_result scripts ~init:() ~f:(fun () { name; statements } ->
      schema t name
        (header
        ^ String.concat ""
            (List.map (fun statement -> statement ^ ";\n") statements)))

let part t ({ name = { database; table }; _ } : Quiver_core.Schema.Table.t)
    ~index ~extension =
  Output.create
    File_path.Operators.(
      t.dir
      /?. ~.(Printf.sprintf "%s.%s.%05d.%s%s" (file_name database)
               (file_name table) index extension
               (Compression.extension t.compression)))
    t.compression

let finish t ~source ~tables ~manifest =
  let stamp moment = Core.Time_ns.to_sec_string moment ~zone:Core.Time_float.Zone.utc in
  let metadata =
    String.concat ""
      ([
         "# Started dump at: " ^ stamp t.started ^ "\n";
         "[config]\nquote-character = BACKTICK\n\n";
         "[myloader_session_variables]\nSQL_MODE=" ^ restore_sql_mode
         ^ " /*!40101\n\n";
         source;
         "\n";
       ]
      @ List.rev_map
          (fun { Quiver_core.Schema.Name.database; table } ->
            Printf.sprintf
              "[%s.%s]\nreal_table_name=%s\nrows = 0\nis_view = 1\n\n"
              (Quiver_core.Ident.backquoted database) (Quiver_core.Ident.backquoted table)
              (table :> string))
          (Portable.Atomic.get t.views)
      @ List.map
          (fun ({ Quiver_core.Schema.Name.database; table }, rows) ->
            Printf.sprintf "[%s.%s]\nreal_table_name=%s\nrows = %d\n\n"
              (Quiver_core.Ident.backquoted database) (Quiver_core.Ident.backquoted table)
              (table :> string)
              rows)
          tables
      @ [ "# Finished dump at: " ^ stamp (Core.Time_ns.now ()) ^ "\n" ])
  in
  let%bind () =
    Output.save File_path.Operators.(t.dir /?. ~."metadata") Plain metadata
  in
  Output.save
    File_path.Operators.(t.dir /?. ~."quiver.json")
    Plain
    (Yojson.Safe.pretty_to_string manifest ^ "\n")
