type error = Ragged of { column : int; length : int; rows : int } | Malformed
[@@deriving sexp_of, equal]

type t = { mutable columns : Column.any Iarray.t; mutable rows : int }

let create ltypes =
  { columns = Iarray.of_list (List.map Column.create ltypes); rows = 0 }

let columns t = t.columns
let rows t = t.rows

let byte_size t =
  Iarray.fold_left
    (fun size (Column.Any column) -> size + Column.byte_size column)
    0 t.columns

let[@inline] commit_row t = t.rows <- t.rows + 1

type sealed = t

let seal (t : t @ unique) : (sealed, _) result @ unique =
  let ragged (t @ local) =
    let rows = t.rows in
    Base.Iarray.find_mapi t.columns ~f:(fun column (Column.Any c) ->
        match Column.length c with
        | length when length = rows -> None
        | length -> Some (column, length))
  in
  match ragged (borrow_ t) with
  | None -> Ok t
  | Some (column, length) ->
      Error (`Batch (Ragged { column; length; rows = rows (borrow_ t) }))

let unseal t = t
let sealed_columns t = t.columns
let sealed_rows t = t.rows

let reshape t ltypes =
  let columns = t.columns in
  t.rows <- 0;
  Iarray.iter (fun (Column.Any column) -> Column.clear column) columns;
  if
    not
      (List.equal Ltype.equal
         (List.map
            (fun (Column.Any column) -> Column.ltype column)
            (Iarray.to_list columns))
         ltypes)
  then
    t.columns <-
      Iarray.of_list
        (List.mapi
           (fun j ltype ->
             if j < Iarray.length columns then
               Column.recycle (Iarray.get columns j) ltype
             else Column.create ltype)
           ltypes)

let encode t (out : Byte_buffer.t @ local) =
  Byte_buffer.add_int32_le out t.rows;
  Iarray.iter (fun (Column.Any column) -> Column.encode column out) t.columns [@nontail]

let decode (t @ local) source ~stop = exclave_
  let header = 4 in
  let rows = if stop >= header then Int32.to_int (Bytes.get_int32_le source 0) else -1 in
  let next =
    Iarray.fold_left
      (fun off (Column.Any column) ->
        Option.bind off (fun off ->
            Column.decode column source ~off ~stop ~rows))
      (Some header) t.columns
  in
  match next with
  | Some off when off = stop ->
      t.rows <- rows;
      Ok t
  | Some _ | None ->
      t.rows <- 0;
      Iarray.iter (fun (Column.Any column) -> Column.clear column) t.columns;
      Error (`Batch Malformed)
