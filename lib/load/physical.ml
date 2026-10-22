open Base.Result.Let_syntax

type cause =
  [ `Link of Quiver_mysql.Link.error
  | `Wire of Quiver_mysql.Wire.error
  | `Transport of Quiver_mysql.Transport.error
  | `File of Quiver_innodb.File.error
  | `Tablespace of Quiver_innodb.Tablespace.error
  | `Check of Quiver_innodb.Check.error ]
[@@deriving sexp_of ~portable]

type unsuitable =
  | Name
  | Definition
  | Template of Quiver_innodb.File.error
  | Cfg of Quiver_innodb.Cfg.error
  | Layout of Quiver_innodb.Layout.error
  | Space of Quiver_innodb.Tablespace.error
[@@deriving sexp_of ~portable]

type table = {
  service : Quiver_core.Schema.Name.t;  (** The name it is built and imported under. *)
  into : Quiver_core.Schema.Name.t;
  layout : Quiver_innodb.Layout.t;
  space : Quiver_innodb.Tablespace.t;
  file : Quiver_innodb.File.t;
  cfg : string;  (** The server's [.cfg] of the empty table. *)
  building : string;  (** The file being built. *)
  ibd : string;  (** Where the server keeps the table's tablespace. *)
  cfg_path : string;
}

type prepared = Prepared of table | Unsuitable of unsuitable

let plain name =
  String.length name > 0
  && String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '_') name

let execute link statement = Quiver_mysql.Link.run_statement link statement

let prepare link ~datadir ~collations ~(into : Quiver_core.Schema.Name.t) (table : Quiver_core.Schema.Table.t) ~create =
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
    let prefix = "CREATE TABLE " ^ Quiver_core.Ident.backquoted table.name.table in
    match Base.String.chop_prefix create ~prefix with
    | None -> Ok (Unsuitable Definition)
    | Some rest -> (
        let%bind () = execute link (Drop_table service) in
        let (_ : (unit, _) result) = Quiver_innodb.File.remove building in
        let%bind () = Quiver_mysql.Link.execute link ("CREATE TABLE " ^ Quiver_mysql.Query.qualified service ^ rest) in
        let%bind () = execute link (Export service) in
        let template = Quiver_innodb.File.contents ibd and cfg = Quiver_innodb.File.contents cfg_path in
        let%bind () = execute link Unlock_tables in
        let drop unsuitable =
          let%map () = execute link (Drop_table service) in
          Unsuitable unsuitable
        in
        match (template, cfg) with
        | Error (`File error), _ | _, Error (`File error) -> drop (Template error)
        | Ok template, Ok cfg -> (
            let dumped =
              List.map
                (fun ({ name; ltype; _ } : Quiver_core.Schema.Field.t) -> ((name :> string), ltype))
                (Quiver_core.Schema.Table.columns table)
            in
            match Quiver_innodb.Cfg.parse cfg with
            | Error (`Cfg error) -> drop (Cfg error)
            | Ok parsed -> (
                match Quiver_innodb.Layout.of_cfg parsed ~collations dumped with
                | Error (`Layout error) -> drop (Layout error)
                | Ok layout -> (
                    let%bind file = Quiver_innodb.File.create building in
                    match Quiver_innodb.Tablespace.create ~template layout file with
                    | Error (`Tablespace error) ->
                        Quiver_innodb.File.close file;
                        let (_ : (unit, _) result) = Quiver_innodb.File.remove building in
                        drop (Space error)
                    | Ok space -> Ok (Prepared { service; into; layout; space; file; cfg; building; ibd; cfg_path })))))

let space t = t.space

let abandon link t =
  Quiver_innodb.File.close t.file;
  let (_ : (unit, _) result) = execute link (Drop_table t.service) in
  List.iter
    (fun path ->
      let (_ : (unit, _) result) = Quiver_innodb.File.remove path in
      ())
    [ t.building; t.ibd; t.cfg_path ]

let import link t finished =
  let built =
    let%bind pages = Quiver_innodb.Tablespace.finish t.space finished in
    let read n =
      let page = Bytes.make Quiver_innodb.Page.size '\000' in
      let (_ : (unit, _) result) = Quiver_innodb.File.read t.file n page in
      page
    in
    let%bind checked = Quiver_innodb.Check.tablespace t.layout ~pages ~read in
    Quiver_innodb.File.close t.file;
    let%bind () = execute link (Discard_tablespace t.service) in
    let%bind () = Quiver_innodb.File.rename t.building t.ibd in
    let%bind () = Quiver_innodb.File.put t.cfg_path t.cfg in
    let%bind () = Quiver_innodb.File.adopt t.ibd in
    let%bind () = Quiver_innodb.File.adopt t.cfg_path in
    let%bind () = execute link (Import_tablespace t.service) in
    let (_ : (unit, _) result) = Quiver_innodb.File.remove t.cfg_path in
    let%map () = execute link (Rename_table { from = t.service; into = t.into }) in
    checked.rows
  in
  Result.iter_error (fun _ -> abandon link t) built;
  built
