open Base.Result.Let_syntax

type cause =
  [ `Link of Quiver_mysql.Link.error
  | `Wire of Quiver_mysql.Wire.error
  | `Transport of Quiver_mysql.Transport.error
  | `File of Quiver_innodb.File.error
  | `Tablespace of Quiver_innodb.Tablespace.error
  | `Check of Quiver_innodb.Check.error
  | `Warned of string list ]
[@@deriving sexp_of ~portable]

type unsuitable =
  | Name
  | Definition
  | Template of Quiver_innodb.File.error
  | Cfg of Quiver_innodb.Cfg.error
  | Layout of Quiver_innodb.Layout.error
  | Space of Quiver_innodb.Tablespace.error
  | Masked_key
[@@deriving sexp_of ~portable]

type builder = Quiver | Server [@@deriving sexp_of ~portable]

type table = {
  service : Quiver_core.Schema.Name.t;  (** The name it is built and imported under. *)
  into : Quiver_core.Schema.Name.t;
  layout : Quiver_innodb.Layout.t;
  secondary : Quiver_innodb.Layout.t list;  (** The secondary indexes built with it. *)
  left : Deferred_keys.key list;  (** Keys the server adds after the import. *)
  space : Quiver_innodb.Tablespace.t;
  file : Quiver_innodb.File.t;
  scratch : Quiver_innodb.File.t;  (** Sorted secondary records while the file is built. *)
  cfg : string;  (** The server's [.cfg] of the empty table. *)
  building : string;  (** The file being built. *)
  sorting : string;  (** The scratch file's path. *)
  ibd : string;  (** Where the server keeps the table's tablespace. *)
  cfg_path : string;
}

type prepared = Prepared of table | Unsuitable of unsuitable

let plain name =
  String.length name > 0
  && String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_') name

let execute link statement = Quiver_mysql.Link.run_statement link statement

let prepare link ~datadir ~collations ~(into : Quiver_core.Schema.Name.t) (table : Quiver_core.Schema.Table.t)
    ~(definition : Deferred_keys.t) ~builder ~sorting =
  let database = (into.database :> string) in
  if not (plain database && plain (table.name.table :> string)) then Ok (Unsuitable Name)
  else
    let digest =
      Quiver_sink.Xxh3.digest
        (Bytes.of_string (database ^ "." ^ (into.table :> string)))
        ~pos:0
        ~len:(String.length database + 1 + String.length (into.table :> string))
    in
    let%bind service_table =
      Result.map_error
        (fun (`Ident _) -> `File { Quiver_innodb.File.call = "name"; code = 22 })
        (Quiver_core.Ident.of_string (Printf.sprintf "q_%016Lx" digest))
    in
    let service = { into with table = service_table } in
    let separator = if String.ends_with ~suffix:"/" datadir then "" else "/" in
    let directory = datadir ^ separator ^ database ^ "/" in
    let base = directory ^ (service_table :> string) in
    let ibd = base ^ ".ibd" and cfg_path = base ^ ".cfg" and building = base ^ ".quiver" in
    let sorting =
      Option.fold sorting ~none:(base ^ ".sort") ~some:(fun directory ->
          Filename.concat directory (Printf.sprintf "%s.%s.sort" database (service_table :> string)))
    in
    let built, server = match builder with Quiver -> (definition.keys, []) | Server -> ([], definition.keys) in
    let prefix = "CREATE TABLE " ^ Quiver_core.Ident.backquoted table.name.table in
    match Base.String.chop_prefix (Deferred_keys.with_keys definition built) ~prefix with
    | None -> Ok (Unsuitable Definition)
    | Some rest ->
        let%bind () = execute link (Drop_table service) in
        let (_ : (unit, _) result) = Quiver_innodb.File.remove building in
        let%bind () = Quiver_mysql.Link.execute link ("CREATE TABLE " ^ Quiver_mysql.Query.qualified service ^ rest) in
        let drop unsuitable =
          let%map () = execute link (Drop_table service) in
          Unsuitable unsuitable
        in
        let dumped =
          List.map
            (fun ({ name; ltype; _ } : Quiver_core.Schema.Field.t) -> ((name :> string), ltype))
            (Quiver_core.Schema.Table.columns table)
        in
        let rec shaped attempt left =
          let%bind () = execute link (Export service) in
          let template = Quiver_innodb.File.contents ibd and cfg = Quiver_innodb.File.contents cfg_path in
          let%bind () = execute link Unlock_tables in
          match (template, cfg) with
          | Error (`File error), _ | _, Error (`File error) -> drop (Template error)
          | Ok template, Ok cfg -> (
              match Quiver_innodb.Cfg.parse cfg with
              | Error (`Cfg error) -> drop (Cfg error)
              | Ok parsed -> (
                  match Quiver_innodb.Layout.of_cfg parsed ~collations dumped with
                  | Error (`Layout error) -> drop (Layout error)
                  | Ok layout -> (
                      let laid =
                        List.map
                          (fun (index : Quiver_innodb.Cfg.Index.t) ->
                            (index.name, Quiver_innodb.Layout.secondary layout parsed index))
                          parsed.secondary
                      in
                      let refused = List.filter_map (fun (name, laid) -> match laid with Error (`Layout error) -> Some (name, error) | Ok _ -> None) laid in
                      match (refused, attempt) with
                      | [], (`First | `Again) -> (
                          let secondary = List.filter_map (fun (_, laid) -> Result.to_option laid) laid in
                          let%bind file = Quiver_innodb.File.create building in
                          let%bind scratch = Quiver_innodb.File.create sorting in
                          match Quiver_innodb.Tablespace.create ~template layout ~secondary file ~scratch with
                          | Error (`Tablespace error) ->
                              Quiver_innodb.File.close file;
                              Quiver_innodb.File.close scratch;
                              let (_ : (unit, _) result) = Quiver_innodb.File.remove building in
                              let (_ : (unit, _) result) = Quiver_innodb.File.remove sorting in
                              drop (Space error)
                          | Ok space ->
                              Ok
                                (Prepared
                                   {
                                     service;
                                     into;
                                     layout;
                                     secondary;
                                     left;
                                     space;
                                     file;
                                     scratch;
                                     cfg;
                                     building;
                                     sorting;
                                     ibd;
                                     cfg_path;
                                   }))
                      | (_, error) :: _, `Again -> drop (Layout error)
                      | (_, error) :: _, `First -> (
                          let first_refused group =
                            List.find_index
                              (fun (key : Deferred_keys.key) -> key.group = group && List.mem_assoc key.name refused)
                              definition.keys
                          in
                          let left =
                            List.filteri
                              (fun index (key : Deferred_keys.key) ->
                                Option.fold (first_refused key.group) ~none:false ~some:(fun first -> index >= first))
                              definition.keys
                          in
                          match
                            Base.Result.all (List.map (fun (key : Deferred_keys.key) -> Quiver_core.Ident.of_string key.name) left)
                          with
                          | Error (`Ident _) -> drop (Layout error)
                          | Ok indexes -> (
                              match execute link (Drop_indexes { table = service; indexes }) with
                              | Error _ -> drop (Layout error)
                              | Ok () -> shaped `Again left)))))
        in
        shaped `First server

let space t = t.space

let abandon link t =
  Quiver_innodb.File.close t.file;
  Quiver_innodb.File.close t.scratch;
  let (_ : (unit, _) result) = execute link (Drop_table t.service) in
  List.iter
    (fun path ->
      let (_ : (unit, _) result) = Quiver_innodb.File.remove path in
      ())
    [ t.building; t.sorting; t.ibd; t.cfg_path ]

type checked = { table : table; rows : int }

let check link t finished ~threads =
  let checked =
    let%bind pages = Quiver_innodb.Tablespace.finish t.space finished ~threads in
    let read n =
      let page = Bytes.make Quiver_innodb.Page.size '\000' in
      let (_ : (unit, _) result) = Quiver_innodb.File.read t.file n page in
      page
    in
    let%map checked = Quiver_innodb.Check.tablespace t.layout ~secondary:t.secondary ~pages ~threads ~read in
    { table = t; rows = checked.rows }
  in
  Result.iter_error (fun _ -> abandon link t) checked;
  checked

let import link { table = t; rows } =
  let imported =
    Quiver_innodb.File.close t.file;
    Quiver_innodb.File.close t.scratch;
    let (_ : (unit, _) result) = Quiver_innodb.File.remove t.sorting in
    let%bind () = execute link (Discard_tablespace t.service) in
    let%bind () = Quiver_innodb.File.rename t.building t.ibd in
    let%bind () = Quiver_innodb.File.put t.cfg_path t.cfg in
    let%bind () = Quiver_innodb.File.adopt t.ibd in
    let%bind () = Quiver_innodb.File.adopt t.cfg_path in
    let%bind () = execute link (Import_tablespace t.service) in
    let%bind warnings = Quiver_mysql.Link.run_query link Warnings in
    let%bind () =
      match warnings with
      | [] -> Ok ()
      | said ->
          Error (`Warned (List.map (fun row -> String.concat " " (List.filter_map Fun.id (Array.to_list row))) said))
    in
    let (_ : (unit, _) result) = Quiver_innodb.File.remove t.cfg_path in
    let%map () = execute link (Rename_table { from = t.service; into = t.into }) in
    rows
  in
  Result.iter_error (fun _ -> abandon link t) imported;
  imported

let left t = t.left
