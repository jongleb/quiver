open Base.Result.Let_syntax

type error =
  | Truncated of int
  | Unsupported_version of int
  | Page_size of int
  | Row_versions
  | No_clustered_index
[@@deriving sexp_of ~portable]

module Column = struct
  type t = { name : string; mtype : int; prtype : int; len : int; mbminmaxlen : int }
  [@@deriving sexp_of ~portable]
end

module Index = struct
  type order = Ascending | Descending [@@deriving sexp_of ~portable]
  type field = { column : string; prefix : int; fixed : int; order : order } [@@deriving sexp_of ~portable]

  type t = {
    id : int;
    root : int;
    name : string;
    n_uniq : int;
    n_nullable : int;
    fields : field list;
  }
  [@@deriving sexp_of ~portable]
end

type t = {
  table : string;
  autoinc : int;
  table_flags : int;
  space_flags : int;
  columns : Column.t list;
  clustered : Index.t;
  secondary : Index.t list;
}
[@@deriving sexp_of ~portable]

let parse text =
  let need at n =
    if at + n > String.length text then Error (`Cfg (Truncated at)) else Ok ()
  in
  let u8 at =
    let%map () = need at 1 in
    (Char.code (String.get text at), at + 1)
  in
  let u32 at =
    let%map () = need at 4 in
    (Int32.to_int (String.get_int32_be text at) land 0xFFFF_FFFF, at + 4)
  in
  let u64 at =
    let%map () = need at 8 in
    (Int64.to_int (String.get_int64_be text at), at + 8)
  in
  let name at =
    let%bind length, at = u32 at in
    let%map () = need at length in
    (String.sub text at (Int.max 0 (length - 1)), at + length)
  in
  let rec many n at read acc =
    match n with
    | 0 -> Ok (List.rev acc, at)
    | _ ->
        let%bind item, at = read at in
        many (n - 1) at read (item :: acc)
  in
  let%bind version, at = u32 0 in
  let%bind () = match version with 7 -> Ok () | other -> Error (`Cfg (Unsupported_version other)) in
  let%bind _host, at = name at in
  let%bind table, at = name at in
  let%bind autoinc, at = u64 at in
  let%bind page_size, at = u32 at in
  let%bind () = match page_size with 16384 -> Ok () | other -> Error (`Cfg (Page_size other)) in
  let%bind table_flags, at = u32 at in
  let%bind n_columns, at = u32 at in
  let%bind _instant_nullable, at = u32 at in
  let%bind counts, at = many 5 at u32 [] in
  let%bind () =
    match counts with
    | [ _; _; _; 0; 0 ] -> Ok ()
    | _ -> Error (`Cfg Row_versions)
  in
  let%bind space_flags, at = u32 at in
  let%bind _compression, at = u8 at in
  let column at =
    let%bind numbers, at = many 7 at u32 [] in
    let%bind name, at = name at in
    let%bind added, at = u8 at in
    let%bind dropped, at = u8 at in
    let%bind _position, at = u32 at in
    let%bind default, at = u8 at in
    match (numbers, added, dropped, default) with
    | [ prtype; mtype; len; mbminmaxlen; _; _; _ ], 0xFF, 0xFF, 0 ->
        Ok ({ Column.name; mtype; prtype; len; mbminmaxlen }, at)
    | _ -> Error (`Cfg Row_versions)
  in
  let%bind columns, at = many n_columns at column [] in
  let field at =
    let%bind prefix, at = u32 at in
    let%bind fixed, at = u32 at in
    let%bind ascending, at = u32 at in
    let%map column, at = name at in
    let order : Index.order = if ascending = 0 then Descending else Ascending in
    ({ Index.column; prefix; fixed; order }, at)
  in
  let index at =
    let%bind id, at = u64 at in
    let%bind _space, at = u32 at in
    let%bind root, at = u32 at in
    let%bind kind, at = u32 at in
    let%bind _trx_id_offset, at = u32 at in
    let%bind _user_fields, at = u32 at in
    let%bind n_uniq, at = u32 at in
    let%bind n_nullable, at = u32 at in
    let%bind n_fields, at = u32 at in
    let%bind name, at = name at in
    let%map fields, at = many n_fields at field [] in
    ((kind, { Index.id; root; name; n_uniq; n_nullable; fields }), at)
  in
  let%bind n_indexes, at = u32 at in
  let%bind indexes, _ = many n_indexes at index [] in
  let tables = List.filter (fun (_, (index : Index.t)) -> index.name <> "CLUST_IND_SDI") indexes in
  match tables with
  | (kind, clustered) :: secondary when kind land 1 = 1 ->
      Ok
        {
          table;
          autoinc;
          table_flags;
          space_flags;
          columns;
          clustered;
          secondary = List.map snd secondary;
        }
  | _ -> Error (`Cfg No_clustered_index)
