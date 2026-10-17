type comparison = Less | Equal | Greater | At_least [@@deriving sexp_of]

type t =
  | Always
  | Compare of {
      field : Schema.Field.t;
      comparison : comparison;
      value : Key.value;
    }
  | All of t * t list
  | Any of t * t list
[@@deriving sexp_of]

let of_chunk (chunk : Chunk.t) =
  match chunk with
  | Whole _ -> Always
  | Range { key = first, rest; range; _ } -> (
      let lexicographic ~strict ~last (bound : Key.t) =
        let value, values = (bound :> Key.value * Key.value list) in
        let fields = first :: rest and values = value :: values in
        let pairs = List.combine fields values in
        let compare (field, value) comparison =
          Compare { field; comparison; value }
        in
        let terms =
          List.mapi
            (fun i pair ->
              let equal =
                List.map
                  (fun pair -> compare pair Equal)
                  (List.filteri (fun j _ -> j < i) pairs)
              in
              let decisive =
                compare pair
                  (if i = List.length pairs - 1 then last else strict)
              in
              match equal with
              | [] -> decisive
              | first :: others -> All (first, others @ [ decisive ]))
            pairs
        in
        match terms with
        | [ only ] -> only
        | first :: others -> Any (first, others)
        | [] -> Always
      in
      match range with
      | Below bound -> lexicographic ~strict:Less ~last:Less bound
      | From bound -> lexicographic ~strict:Greater ~last:At_least bound
      | Between (lo, hi) ->
          All
            ( lexicographic ~strict:Greater ~last:At_least lo,
              [ lexicographic ~strict:Less ~last:Less hi ] ))
