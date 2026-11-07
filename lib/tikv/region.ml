open Base.Result.Let_syntax

type replica = { peer : Kv.peer; address : string } [@@deriving sexp_of ~portable]

type t = { context : Kv.region; start : string; stop : Kv.bound; address : string; replicas : replica list }
[@@deriving sexp_of ~portable]

type error =
  | Malformed of string option list
  | No_table of Quiver_core.Schema.Name.t
[@@deriving sexp_of ~portable]

let find regions key =
  List.find_opt
    (fun region ->
      String.compare region.start key <= 0
      && match region.stop with Endless -> true | Before stop -> String.compare key stop < 0)
    regions

let table_id link name =
  let%bind ids = Quiver_mysql.Link.run_query link (Table_id name) in
  match ids with
  | [ [| Some id |] ] -> Option.to_result ~none:(`Region (Malformed [ Some id ])) (Int64.of_string_opt id)
  | _ -> Error (`Region (No_table name))
