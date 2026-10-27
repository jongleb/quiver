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

type order = Signed_order | Unsigned_order

let ordered = function
  | Range { key = _, []; range = Between (lo, hi); _ } -> (
      match
        ((lo :> Key.value * Key.value list), (hi :> Key.value * Key.value list))
      with
      | (Signed lo, []), (Signed hi, []) -> Some (Signed_order, lo, hi)
      | (Unsigned lo, []), (Unsigned hi, []) -> Some (Unsigned_order, lo, hi)
      | _ -> None)
  | Whole _ | Range _ -> None

let integer_key chunk =
  let integer (field : Schema.Field.t) =
    match field.ltype with
    | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64 -> Some field
    | _ -> None
  in
  match chunk with
  | Range { key = field, []; _ }
  | Whole
      {
        key = Primary (field, []) | Unique { fields = field, []; _ } | Indexed { field; _ };
        _;
      } ->
      integer field
  | Range _ | Whole _ -> None

let closed chunk (lo : Key.value) (hi : Key.value) =
  let above =
    match hi with
    | Signed hi when not (Int64.equal hi Int64.max_int) -> Some (Key.Signed (Int64.succ hi))
    | Unsigned hi when not (Int64.equal hi (-1L)) -> Some (Key.Unsigned (Int64.succ hi))
    | _ -> None
  in
  let between table field (low : Key.value) (high : Key.value) =
    let make () =
      Result.to_option
        (let%map low = Key.make (field, []) [ low ]
         and high = Key.make (field, []) [ high ] in
         Range { table; key = (field, []); range = Between (low, high) })
    in
    match (low, high) with
    | Signed l, Signed h when Int64.compare l h < 0 -> make ()
    | Unsigned l, Unsigned h when Int64.unsigned_compare l h < 0 -> make ()
    | _ -> None
  in
  match (chunk, integer_key chunk) with
  | Range { range = Between _; _ }, Some _ -> Some chunk
  | Range { table; range = Below below; _ }, Some field ->
      between table field lo (fst (below :> Key.value * Key.value list))
  | Range { table; range = From from; _ }, Some field ->
      Option.bind above (fun above ->
          between table field (fst (from :> Key.value * Key.value list)) above)
  | Whole table, Some field ->
      Option.bind above (fun above -> between table field lo above)
  | (Range _ | Whole _), None -> None

let bounds chunk = Option.map (fun (_, lo, hi) -> (lo, hi)) (ordered chunk)

let divide chunk at =
  match (chunk, ordered chunk) with
  | Range ({ range = Between (lo, hi); _ } as range), Some (order, low, high) ->
      let compare, value =
        match order with
        | Signed_order -> (Int64.compare, Key.Signed at)
        | Unsigned_order -> (Int64.unsigned_compare, Key.Unsigned at)
      in
      if compare low at < 0 && compare at high < 0 then
        Option.map
          (fun at ->
            ( Range { range with range = Between (lo, at) },
              Range { range with range = Between (at, hi) } ))
          (Result.to_option (Key.make range.key [ value ]))
      else None
  | (Whole _ | Range _), _ -> None

let of_bounds (table : Schema.Table.t) values =
  let key : (Schema.Field.t * Schema.Field.t list) option =
    match table.key with
    | No_key -> None
    | Primary key | Unique { fields = key; _ } -> Some key
    | Indexed { field; _ } -> Some (field, [])
  in
  match key with
  | None -> Error (`Chunk (Unsplittable_key table.name))
  | Some key -> (
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
