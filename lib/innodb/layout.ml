open Base.Result.Let_syntax

type error =
  | Missing_column of string
  | Unsupported of { column : string; mtype : int; prtype : int }
  | Mismatch of { column : string; ltype : Quiver_core.Ltype.t }
  | Prefix_key of string
  | Row_format of { table_flags : int; space_flags : int }
  | Key_type of string
  | Descending_key of string
  | Charset of { column : string; collation : int }
[@@deriving sexp_of ~portable]

type sign = Signed | Unsigned [@@deriving sexp_of ~portable]

type number = Plain | Year | Days | Member of string list | Members of string list
[@@deriving sexp_of ~portable]

type clock = Microseconds | Fields [@@deriving sexp_of ~portable]

type charset = Utf8mb4 | Utf8mb3 | Latin1 [@@deriving sexp_of ~portable]

type storage =
  | Integer of { bytes : int; sign : sign; number : number }
  | Float
  | Double
  | Decimal of { precision : int; scale : int }
  | Datetime of { fsp : int; clock : clock }
  | Timestamp of { fsp : int; clock : clock }
  | Time of int
  | Binary of int
  | Char of { least : int; big : bool; charset : charset }
  | Text of { big : bool; charset : charset }
  | Variable of { big : bool }
  | Json
  | Row_id
  | Trx_id
  | Roll_ptr
[@@deriving sexp_of ~portable]

type length = Fixed of int | Varying of { big : bool } [@@deriving sexp_of ~portable]

type nullability = Not_null | Null_bit of int [@@deriving sexp_of ~portable]
type part = Whole | Prefix of int [@@deriving sexp_of ~portable]

type field = { source : int; storage : storage; nullability : nullability; part : part }
[@@deriving sexp_of ~portable]

type order = Bytes_order | Source_order | Fields_order [@@deriving sexp_of ~portable]
type distinct = Key | Leading of int [@@deriving sexp_of ~portable]

type t = {
  fields : field list;
  key : int;
  order : order;
  distinct : distinct;
  null_bytes : int;
  index : int;
  root : int;
}
[@@deriving sexp_of ~portable]

let decimal_bytes ~precision ~scale =
  let digits n = ((n / 9) * 4) + Int.min 4 (((n mod 9) + 1) / 2) in
  digits (precision - scale) + digits scale

let length storage =
  match storage with
  | Integer { bytes; _ } | Binary bytes -> Fixed bytes
  | Float -> Fixed 4
  | Double -> Fixed 8
  | Decimal { precision; scale } -> Fixed (decimal_bytes ~precision ~scale)
  | Datetime { fsp; _ } -> Fixed (5 + ((fsp + 1) / 2))
  | Timestamp { fsp; _ } -> Fixed (4 + ((fsp + 1) / 2))
  | Time fsp -> Fixed (3 + ((fsp + 1) / 2))
  | Row_id | Trx_id -> Fixed 6
  | Roll_ptr -> Fixed 7
  | Char { least; charset = Latin1; _ } -> Fixed least
  | Char { big; _ } | Text { big; _ } | Variable { big } -> Varying { big }
  | Json -> Varying { big = true }

let of_cfg (cfg : Cfg.t) ~collations dumped =
  let%bind () =
    match (cfg.table_flags, cfg.space_flags) with
    | 33, 0x4021 -> Ok ()
    | table_flags, space_flags -> Error (`Layout (Row_format { table_flags; space_flags }))
  in
  let storage (field : Cfg.Index.field) =
    let%bind (column : Cfg.Column.t) =
      Option.to_result ~none:(`Layout (Missing_column field.column))
        (List.find_opt (fun (column : Cfg.Column.t) -> column.name = field.column) cfg.columns)
    in
    let nullable = column.prtype land 256 = 0 in
    let found =
      List.find_mapi
        (fun i (name, ltype) -> if name = column.name then Some (i, ltype) else None)
        dumped
    in
    let%bind () = if field.prefix = 0 then Ok () else Error (`Layout (Prefix_key column.name)) in
    match (column.name, column.mtype, found) with
    | "DB_ROW_ID", 8, _ -> Ok (-1, Row_id, nullable)
    | "DB_TRX_ID", 8, _ -> Ok (-1, Trx_id, nullable)
    | "DB_ROLL_PTR", 8, _ -> Ok (-1, Roll_ptr, nullable)
    | _, _, None -> Error (`Layout (Missing_column column.name))
    | _, mtype, Some (i, ltype) -> (
        let sign = if column.prtype land 512 <> 0 then Unsigned else Signed in
        let collation = column.prtype lsr 16 in
        let mysql = column.prtype land 0xFF in
        let fraction fsp = (fsp + 1) / 2 in
        let text storage =
          Option.fold (List.assoc_opt collation collations)
            ~none:(Error (`Layout (Charset { column = column.name; collation })))
            ~some:(fun charset -> Ok (i, storage charset, nullable))
        in
        let integer number = Ok (i, Integer { bytes = column.len; sign; number }, nullable) in
        let mismatch = Error (`Layout (Mismatch { column = column.name; ltype })) in
        match (mtype, (ltype : Quiver_core.Ltype.t), sign) with
        | 6, (Int8 | Int16 | Int32 | Int64), Signed | 6, (Uint8 | Uint16 | Uint32 | Uint64), Unsigned
        | 6, Unchecked_date, _ ->
            integer Plain
        | 6, Year, _ -> integer Year
        | 6, Date, _ -> integer Days
        | 6, Enum members, _ -> integer (Member members)
        | 6, Set members, _ -> integer (Members members)
        | 3, Uint64, _ when mysql = 16 -> integer Plain
        | 3, Bytes, _ when collation = 63 -> Ok (i, Binary column.len, nullable)
        | 3, Decimal { precision; scale }, _ when mysql = 246 && column.len = decimal_bytes ~precision ~scale ->
            Ok (i, Decimal { precision; scale }, nullable)
        | 3, (Datetime fsp | Unchecked_datetime fsp), _
          when mysql = 12 && column.len = 5 + fraction (fsp :> int) ->
            let clock = match ltype with Unchecked_datetime _ -> Fields | _ -> Microseconds in
            Ok (i, Datetime { fsp = (fsp :> int); clock }, nullable)
        | 3, (Timestamp fsp | Unchecked_datetime fsp), _ when mysql = 7 && column.len = 4 + fraction (fsp :> int) ->
            let clock = match ltype with Unchecked_datetime _ -> Fields | _ -> Microseconds in
            Ok (i, Timestamp { fsp = (fsp :> int); clock }, nullable)
        | 3, Time fsp, _ when mysql = 11 && column.len = 3 + fraction (fsp :> int) ->
            Ok (i, Time (fsp :> int), nullable)
        | 9, Float32, _ -> Ok (i, Float, nullable)
        | 10, Float64, _ -> Ok (i, Double, nullable)
        | (1 | 12 | 5), String, _ -> text (fun charset -> Text { big = column.len > 255 || mtype = 5; charset })
        | (2 | 13), String, _ ->
            text (fun charset ->
                let widest = match charset with Utf8mb4 -> 4 | Utf8mb3 -> 3 | Latin1 -> 1 in
                Char { least = column.len / widest; big = column.len > 255; charset })
        | (4 | 5), Bytes, _ when collation = 63 -> Ok (i, Variable { big = column.len > 255 || mtype = 5 }, nullable)
        | 14, Geometry, _ -> Ok (i, Variable { big = true }, nullable)
        | 5, Json, _ when mysql = 245 -> Ok (i, Json, nullable)
        | (1 | 2 | 3 | 4 | 5 | 6 | 9 | 10 | 12 | 13 | 14), _, _ -> mismatch
        | _ -> Error (`Layout (Unsupported { column = column.name; mtype; prtype = column.prtype })))
  in
  let%bind stored = Base.Result.all (List.map storage cfg.clustered.fields) in
  let%bind order =
    Base.List.fold_result (Base.List.take (List.combine cfg.clustered.fields stored) cfg.clustered.n_uniq)
      ~init:Bytes_order ~f:(fun order ((field : Cfg.Index.field), (_, storage, _)) ->
        match (storage, field.order) with
        | _, Descending -> Error (`Layout (Descending_key field.column))
        | (Integer _ | Decimal _ | Datetime _ | Timestamp _ | Time _ | Binary _ | Row_id), Ascending -> Ok order
        | (Float | Double | Char _ | Text _ | Variable _), Ascending -> Ok Source_order
        | (Json | Trx_id | Roll_ptr), Ascending -> Error (`Layout (Key_type field.column)))
  in
  let fields, _ =
    List.fold_left
      (fun (fields, nulls) (source, storage, nullable) ->
        if nullable then ({ source; storage; nullability = Null_bit nulls; part = Whole } :: fields, nulls + 1)
        else ({ source; storage; nullability = Not_null; part = Whole } :: fields, nulls))
      ([], 0) stored
  in
  Ok
    {
      fields = List.rev fields;
      key = cfg.clustered.n_uniq;
      order;
      distinct = Key;
      null_bytes = (cfg.clustered.n_nullable + 7) / 8;
      index = cfg.clustered.id;
      root = cfg.clustered.root;
    }

let secondary clustered (cfg : Cfg.t) (index : Cfg.Index.t) =
  let columns = List.combine (List.map (fun (field : Cfg.Index.field) -> field.column) cfg.clustered.fields) clustered.fields in
  let%bind stored =
    Base.Result.all
      (List.map
         (fun (field : Cfg.Index.field) ->
           let%bind (column : field) =
             Option.to_result ~none:(`Layout (Missing_column field.column)) (List.assoc_opt field.column columns)
           in
           let%bind part =
             match (field.prefix, column.storage) with
             | 0, _ -> Ok Whole
             | prefix, Variable _ -> Ok (Prefix prefix)
             | _, _ -> Error (`Layout (Prefix_key field.column))
           in
           match (field.order, column.storage) with
           | Descending, _ -> Error (`Layout (Descending_key field.column))
           | Ascending, (Integer _ | Decimal _ | Datetime _ | Timestamp _ | Time _ | Binary _ | Variable _) ->
               Ok (column, part)
           | Ascending, (Float | Double | Char _ | Text _ | Json | Row_id | Trx_id | Roll_ptr) ->
               Error (`Layout (Key_type field.column)))
         index.fields)
  in
  let fields, nulls =
    List.fold_left
      (fun (fields, nulls) ((column : field), part) ->
        match column.nullability with
        | Null_bit _ -> ({ column with nullability = Null_bit nulls; part } :: fields, nulls + 1)
        | Not_null -> ({ column with part } :: fields, nulls))
      ([], 0) stored
  in
  let n = List.length fields in
  Ok
    {
      fields = List.rev fields;
      key = n;
      order = Fields_order;
      distinct = (if index.n_uniq < n then Leading index.n_uniq else Key);
      null_bytes = (nulls + 7) / 8;
      index = index.id;
      root = index.root;
    }
