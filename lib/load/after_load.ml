type error = Malformed of string | Unused of string [@@deriving sexp_of]

type rule = { pattern : Quiver_core.Pattern.table; written : string; statement : string }

let sexp_of_rule { written; _ } = Sexplib0.Sexp.Atom written

let rule written =
  match Quiver_core.Pattern.assignment written with
  | None -> Error (`After_load (Malformed written))
  | Some (named, statement) -> (
      match String.trim statement with
      | "" -> Error (`After_load (Malformed written))
      | statement ->
          Result.map
            (fun pattern -> { pattern; written; statement })
            (Quiver_core.Pattern.table named))

let statements rules tables ~into =
  let placed (into : Quiver_core.Schema.Name.t) statement =
    let table =
      Quiver_core.Ident.backquoted into.database ^ "." ^ Quiver_core.Ident.backquoted into.table
    in
    Base.String.substr_replace_all statement ~pattern:"{table}" ~with_:table
  in
  let named rule name =
    match Quiver_core.Pattern.fits_table rule.pattern name with Fits -> Some name | Misses -> None
  in
  match List.find_opt (fun rule -> List.for_all (fun name -> Option.is_none (named rule name)) tables) rules with
  | Some { written; _ } -> Error (`After_load (Unused written))
  | None ->
      Ok
        (List.filter_map
           (fun name ->
             let target = into name in
             match List.filter_map (fun rule -> Option.map (fun _ -> placed target rule.statement) (named rule name)) rules with
             | [] -> None
             | first :: rest -> Some (target, (first, rest)))
           tables)
