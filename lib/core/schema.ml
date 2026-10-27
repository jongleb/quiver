open Base.Result.Let_syntax

module Name = struct
  type t = { database : Ident.t; table : Ident.t }
  [@@deriving equal ~portable, compare ~portable, hash ~portable, yojson]

  let pp out { database; table } =
    Format.fprintf out "%s.%s" (database :> string) (table :> string)

  let sexp_of_t { database; table } =
    Sexplib0.Sexp.List [ Atom (database :> string); Atom (table :> string) ]

  let t_of_sexp = function
    | Sexplib0.Sexp.List [ database; table ] ->
        { database = Ident.t_of_sexp database; table = Ident.t_of_sexp table }
    | sexp -> Sexplib0.Sexp_conv.of_sexp_error "a database and a table" sexp
end

module Field = struct
  type nullability = Nullable | Not_null [@@deriving sexp ~portable, equal ~portable, yojson]

  type t = { name : Ident.t; ltype : Ltype.t; nullability : nullability }
  [@@deriving sexp ~portable, equal ~portable, yojson]
end

type error =
  | No_fields of Name.t
  | Duplicate_field of { table : Name.t; field : Ident.t }
  | Unknown_key_field of { table : Name.t; field : Ident.t }
  | Nullable_key_field of { table : Name.t; field : Ident.t }
  | Duplicate_table of Name.t
[@@deriving sexp_of ~portable]

module Table = struct
  type 'f key =
    | Primary of ('f * 'f list)
    | Unique of { index : Ident.t; fields : 'f * 'f list }
    | Indexed of { index : Ident.t; field : 'f }
    | No_key
  [@@deriving sexp ~portable, yojson]

  type estimate = { rows : int; bytes : int } [@@deriving sexp ~portable, yojson]

  type t = {
    name : Name.t;
    fields : Field.t * Field.t list;
    generated : Field.t list; [@sexp.list] [@yojson.default []]
    key : Field.t key;
    estimate : estimate;
  }
  [@@deriving sexp ~portable, yojson]

  let columns { fields = first, rest; generated; _ } = (first :: rest) @ generated

  let make ~name ~fields ~generated ~key ~estimate =
    let resolve (first, rest) =
      let one field =
        Option.fold
          ~none:(Error (Unknown_key_field { table = name; field }))
          ~some:(fun ({ Field.nullability; _ } as found) ->
            match nullability with
            | Field.Not_null -> Ok found
            | Field.Nullable ->
                Error (Nullable_key_field { table = name; field }))
          (List.find_opt
             (fun { Field.name; _ } -> Ident.equal name field)
             (fields @ generated))
      in
      let%map first = one first
      and rest = Base.Result.all (List.map one rest) in
      (first, rest)
    in
    let duplicate =
      Base.List.find_a_dup ~compare:Ident.compare
        (List.map (fun { Field.name; _ } -> name) (fields @ generated))
    in
    match (fields, duplicate) with
    | [], _ -> Error (`Schema (No_fields name))
    | _, Some field -> Error (`Schema (Duplicate_field { table = name; field }))
    | first :: rest, None ->
        Result.map_error
          (fun error -> `Schema error)
          (let%map key =
             match key with
             | No_key -> Ok No_key
             | Primary fields ->
                 let%map fields = resolve fields in
                 Primary fields
             | Unique { index; fields } ->
                 let%map fields = resolve fields in
                 Unique { index; fields }
             | Indexed { index; field } ->
                 let%map field, _ = resolve (field, []) in
                 Indexed { index; field }
           in
           { name; fields = (first, rest); generated; key; estimate })

  let remade { name; fields = first, rest; generated; key; estimate } =
    let names (first, rest) =
      (first.Field.name, List.map (fun { Field.name; _ } -> name) rest)
    in
    let key =
      match key with
      | No_key -> No_key
      | Primary fields -> Primary (names fields)
      | Unique { index; fields } -> Unique { index; fields = names fields }
      | Indexed { index; field } -> Indexed { index; field = field.Field.name }
    in
    make ~name ~fields:(first :: rest) ~generated ~key ~estimate

  let t_of_sexp sexp =
    Codec.checked_of_sexp (fun (`Schema e) -> sexp_of_error e) (remade (t_of_sexp sexp)) sexp

  let t_of_yojson json =
    Codec.checked_of_yojson (fun (`Schema e) -> sexp_of_error e) (remade (t_of_yojson json)) json
end

type t = { tables : Table.t list } [@@deriving sexp_of ~portable, yojson_of]

let make tables =
  Option.fold
    ~none:(Ok { tables })
    ~some:(fun name -> Error (`Schema (Duplicate_table name)))
    (Base.List.find_a_dup ~compare:Name.compare
       (List.map (fun { Table.name; _ } -> name) tables))
