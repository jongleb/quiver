type rule = Database of Ident.t | Table of Schema.Name.t
[@@deriving sexp_of, equal]

type t = Everything | Only of rule * rule list
type rows = All_rows | No_rows | Rows_of of rule * rule list
type objects = With_objects | Without_objects
type error = Unmatched_rows of rule [@@deriving sexp_of]

type split = {
  with_rows : Schema.Table.t list;
  schema_only : Schema.Table.t list;
}

let split rows tables =
  let covers rule ({ name; _ } : Schema.Table.t) =
    match rule with
    | Database database -> Ident.equal database name.database
    | Table table -> Schema.Name.equal table name
  in
  match rows with
  | All_rows -> Ok { with_rows = tables; schema_only = [] }
  | No_rows -> Ok { with_rows = []; schema_only = tables }
  | Rows_of (first, rest) ->
      let rules = first :: rest in
      Option.fold
        ~none:
          (let with_rows, schema_only =
             List.partition
               (fun table -> List.exists (fun rule -> covers rule table) rules)
               tables
           in
           Ok { with_rows; schema_only })
        ~some:(fun rule -> Error (`Filter (Unmatched_rows rule)))
        (List.find_opt
           (fun rule -> not (List.exists (covers rule) tables))
           rules)
