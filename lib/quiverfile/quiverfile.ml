type error = Unreadable of string | Not_settings of string | Unusable of string | Scoped of string
[@@deriving sexp_of]
type value = Text of string | Texts of string list [@@deriving sexp_of]
type t = { common : (string * value) list; sections : (string * (string * value) list) list }

let of_string ?file ~commands ~scoped text =
  let fail error = Error (`Quiverfile error) in
  let scalar : YAMLx.value -> string option = function
    | Bool (_, b) -> Some (Bool.to_string b)
    | Int (_, i) -> Some (Int64.to_string i)
    | Float (_, f) -> Some (Printf.sprintf "%.17g" f)
    | String (_, s) -> Some s
    | Null _ | Seq _ | Map _ -> None
  in
  let option name (v : YAMLx.value) =
    match (v, scalar v) with
    | Null _, _ -> Ok None
    | _, Some text -> Ok (Some (name, Text text))
    | Seq (_, items), None ->
        Option.fold (Base.Option.all (List.map scalar items)) ~none:(fail (Unusable name)) ~some:(fun texts ->
            Ok (Some (name, Texts texts)))
    | (Bool _ | Int _ | Float _ | String _ | Map _), None -> fail (Unusable name)
  in
  let options where pairs =
    List.fold_left
      (fun acc ((_, key, v) : YAMLx.loc * YAMLx.value * YAMLx.value) ->
        match (acc, key) with
        | Error _, _ -> acc
        | Ok acc, String (_, name) -> (
            match option name v with
            | Ok None -> Ok acc
            | Ok (Some setting) -> Ok (setting :: acc)
            | Error _ as failed -> failed)
        | Ok _, _ -> fail (Not_settings where))
      (Ok []) pairs
    |> Result.map List.rev
  in
  match YAMLx.Values.of_yaml ?file ~strict_keys:true ~reject_ambiguous:true text with
  | Error message -> fail (Unreadable message)
  | Ok [] -> Ok { common = []; sections = [] }
  | Ok [ Null _ ] -> Ok { common = []; sections = [] }
  | Ok [ Map (_, pairs) ] ->
      let section ((_, key, _) : YAMLx.loc * YAMLx.value * YAMLx.value) =
        match key with String (_, name) -> List.mem name commands | _ -> false
      in
      let own, shared = List.partition section pairs in
      Result.bind
        (Result.bind (options "the top" shared) (fun common ->
             Option.fold
               (List.find_opt (fun (name, _) -> List.mem name scoped) common)
               ~none:(Ok common)
               ~some:(fun (name, _) -> fail (Scoped name))))
        (fun common ->
          List.fold_left
            (fun acc ((_, key, v) : YAMLx.loc * YAMLx.value * YAMLx.value) ->
              match (acc, key, v) with
              | Error _, _, _ -> acc
              | Ok acc, String (_, name), Map (_, pairs) ->
                  Result.map (fun settings -> (name, settings) :: acc) (options name pairs)
              | Ok acc, String (_, name), Null _ -> Ok ((name, []) :: acc)
              | Ok _, String (_, name), _ -> fail (Not_settings name)
              | Ok _, _, _ -> fail (Not_settings "the top"))
            (Ok []) own
          |> Result.map (fun sections -> { common; sections = List.rev sections }))
  | Ok [ _ ] -> fail (Not_settings "the top")
  | Ok (_ :: _ :: _) -> fail (Unreadable "more than one document")

let settings t ~command =
  let own = Option.value (List.assoc_opt command t.sections) ~default:[] in
  own @ List.filter (fun (name, _) -> not (List.mem_assoc name own)) t.common
