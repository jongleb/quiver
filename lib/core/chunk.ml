open Base.Result.Let_syntax

type error = Unsplittable_key of Schema.Name.t [@@deriving sexp_of ~portable, equal ~portable]

module Range = struct
  type t = Below of Key.t | Between of Key.t * Key.t | From of Key.t
  [@@deriving equal ~portable, yojson_of]

  let pp out = function
    | Below bound -> Format.fprintf out "below %a" Key.pp bound
    | Between (lo, hi) ->
        Format.fprintf out "from %a below %a" Key.pp lo Key.pp hi
    | From bound -> Format.fprintf out "from %a" Key.pp bound

  let sexp_of_t range = Sexplib0.Sexp.Atom (Format.asprintf "%a" pp range)
end

type t =
  | Whole of Schema.Table.t
  | Range of {
      table : Schema.Table.t;
      key : Schema.Field.t * Schema.Field.t list;
      range : Range.t;
    }

let pp out = function
  | Whole { name; _ } -> Format.fprintf out "%a, whole" Schema.Name.pp name
  | Range { table = { name; _ }; key = first, rest; range } ->
      let key =
        match rest with
        | [] -> (first.name :> string)
        | _ ->
            "("
            ^ String.concat ", "
                (List.map
                   (fun ({ name; _ } : Schema.Field.t) -> (name :> string))
                   (first :: rest))
            ^ ")"
      in
      Format.fprintf out "%a, %s %a" Schema.Name.pp name key Range.pp range

let sexp_of_t chunk = Sexplib0.Sexp.Atom (Format.asprintf "%a" pp chunk)
let table = function Whole table | Range { table; _ } -> table
let whole table = Whole table

let of_bounds (table : Schema.Table.t) values =
  match table.key with
  | No_key -> Error (`Chunk (Unsplittable_key table.name))
  | Primary key | Unique { fields = key; _ } -> (
      let%map bounds = Base.Result.all (List.map (Key.make key) values) in
      let range range = Range { table; key; range } in
      let rec between = function
        | lo :: (hi :: _ as rest) -> range (Between (lo, hi)) :: between rest
        | [ last ] -> [ range (From last) ]
        | [] -> []
      in
      match bounds with
      | [] -> [ Whole table ]
      | first :: _ as all -> range (Below first) :: between all)
