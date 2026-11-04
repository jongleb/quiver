open Base.Result.Let_syntax

type error =
  | Changing of string
  | Handles_differ of { source : Info.handle; target : Info.handle }
  | Columns_differ of { source : string list; target : string list }
  | Indexes_differ of { source : string list; target : string list }
  | Partitions_differ of { source : string list; target : string list }
  | Rearranging
  | Global_index of string
  | Old_row
  | Missing of string
  | Malformed of string
[@@deriving sexp_of ~portable]

(** Each source column id's id in the copy, by the source id. *)
type columns = Kept | Moved of int option iarray

(** What a row written before a column was added lacks in the copy. *)
type need =
  | Filled of { source : int; into : int; cell : string }  (** The column's default then, as a cell. *)
  | Unfilled of { source : int; name : string }  (** A default this does not write. *)

type t = {
  tables : (int64 * int64) iarray;  (** Each source id as its keys hold it, by that, with the copy's. *)
  indexes : (int64 * int64) list; columns : columns; needed : need list }

let tables t = List.map (fun (held, into) -> (Int64.logxor held Int64.min_int, into)) (Iarray.to_list t.tables)

let make ~(source : Info.t) ~(target : Info.t) =
  let changing =
    Base.Option.first_some
      (List.find_map (fun (column : Info.column) -> match column.state with Changing -> Some column.name | Public -> None) (source.columns @ target.columns))
      (List.find_map (fun (index : Info.index) -> match index.state with Changing -> Some index.name | Public -> None) (source.indexes @ target.indexes))
  in
  let columns (table : Info.t) = List.sort String.compare (List.map (fun (column : Info.column) -> column.name) table.columns)
  and indexes (table : Info.t) = List.sort String.compare (List.map (fun (index : Info.index) -> index.name) table.indexes)
  and partitions (table : Info.t) =
    match table.layout with
    | Partitioned (first, rest) -> List.sort (fun (a : Info.partition) b -> String.compare a.name b.name) (first :: rest)
    | Whole | Rearranging -> []
  in
  let names table = List.map (fun (partition : Info.partition) -> partition.name) (partitions table) in
  let global =
    List.find_map (fun (index : Info.index) -> match index.scope with Global -> Some index.name | Local -> None) (source.indexes @ target.indexes)
  in
  match (changing, global) with
  | Some name, _ -> Error (`Renumber (Changing name))
  | None, _ when source.layout = Rearranging || target.layout = Rearranging -> Error (`Renumber Rearranging)
  | None, Some name -> Error (`Renumber (Global_index name))
  | None, None when names source <> names target -> Error (`Renumber (Partitions_differ { source = names source; target = names target }))
  | None, None when source.handle <> target.handle -> Error (`Renumber (Handles_differ { source = source.handle; target = target.handle }))
  | None, None when columns source <> columns target -> Error (`Renumber (Columns_differ { source = columns source; target = columns target }))
  | None, None when indexes source <> indexes target -> Error (`Renumber (Indexes_differ { source = indexes source; target = indexes target }))
  | None, None ->
      let pairs =
        List.filter_map
          (fun (column : Info.column) ->
            Option.map (fun (other : Info.column) -> (column, other)) (List.find_opt (fun (other : Info.column) -> other.name = column.name) target.columns))
          source.columns
      in
      let moved =
        if List.for_all (fun ((column : Info.column), (other : Info.column)) -> column.id = other.id) pairs then Kept
        else
          let most = List.fold_left (fun most ((column : Info.column), _) -> Int.max most column.id) 0 pairs in
          Moved
            (Iarray.of_list
               (List.init (most + 1) (fun id ->
                    List.find_map (fun ((column : Info.column), (other : Info.column)) -> if column.id = id then Some other.id else None) pairs)))
      in
      let needed =
        List.filter_map
          (fun ((column : Info.column), (other : Info.column)) ->
            match (column.missing, other.missing) with
            | Origin default, Unset ->
                Some
                  (Option.fold
                     ~none:(Unfilled { source = column.id; name = column.name })
                     ~some:(fun cell -> Filled { source = column.id; into = other.id; cell })
                     (Cell.of_default column.kind default))
            | Origin _, Origin _ | Unset, _ -> None)
          pairs
      in
      let indexes =
        List.filter_map
          (fun (index : Info.index) ->
            Option.map (fun (other : Info.index) -> (index.id, other.id)) (List.find_opt (fun (other : Info.index) -> other.name = index.name) target.indexes))
          source.indexes
      in
      let tables =
        match partitions source with
        | [] -> [ (source.id, target.id) ]
        | own -> List.map2 (fun (from : Info.partition) (into : Info.partition) -> (from.id, into.id)) own (partitions target)
      in
      let held = List.map (fun (source, into) -> (Int64.logxor source Int64.min_int, into)) tables in
      Ok { tables = Iarray.of_list (List.sort (fun (a, _) (b, _) -> Int64.compare a b) held); indexes; columns = moved; needed }

(** What a key of the table is of. *)
type kind = Row | Index | Other

let kind key =
  let length = String.length key in
  if length >= 19 && key.[9] = '_' && key.[10] = 'i' then Index else if length >= 11 && key.[9] = '_' && key.[10] = 'r' then Row else Other

let key t key =
  let rec search tables key low high =
    if low >= high then -1
    else
      let middle = (low + high) / 2 in
      match Int64.compare (fst (Iarray.get tables middle)) (String.get_int64_be key 1) with
      | 0 -> middle
      | n when n < 0 -> search tables key (middle + 1) high
      | _ -> search tables key low middle
  in
  let found = if String.length key < 9 then -1 else search t.tables key 0 (Iarray.length t.tables) in
  if found < 0 then None
  else
    let into = snd (Iarray.get t.tables found) in
    match kind key with
    | Index ->
        let id = Int64.logxor (String.get_int64_be key 11) Int64.min_int in
        Option.map
          (fun target ->
            let bytes = Bytes.of_string key in
            Bytes.set_int64_be bytes 1 (Int64.logxor into Int64.min_int);
            Bytes.set_int64_be bytes 11 (Int64.logxor target Int64.min_int);
            Bytes.unsafe_to_string bytes)
          (List.find_map (fun (source, target) -> if Int64.equal source id then Some target else None) t.indexes)
    | Row | Other -> Some (Codec.retabled ~into key)

(** A row of TiDB's second format: how wide an id is, how many columns hold a value and how many
    are NULL, where its end offsets and its data start. *)
type row = { width : int; not_null : int; null : int; offsets : int; data : int }

let malformed value = Error (`Renumber (Malformed (Hex_encode.to_hex value)))

let id value row i = if row.width = 1 then Char.code value.[6 + i] else Int32.to_int (String.get_int32_le value (6 + (4 * i))) land 0xFFFF_FFFF

let stop value row i =
  if row.width = 1 then String.get_uint16_le value (row.offsets + (2 * i)) else Int32.to_int (String.get_int32_le value (row.offsets + (4 * i))) land 0xFFFF_FFFF

let row value =
  let length = String.length value in
  if length >= 1 && (value.[0] = '\x08' || value.[0] = '\x00') then Error (`Renumber Old_row)
  else if length < 6 || value.[0] <> '\x80' then malformed value
  else
    let width = if Char.code value.[1] land 1 = 0 then 1 else 4 in
    let offset_width = if width = 1 then 2 else 4 in
    let not_null = String.get_uint16_le value 2 and null = String.get_uint16_le value 4 in
    let offsets = 6 + ((not_null + null) * width) in
    let data = offsets + (not_null * offset_width) in
    if data > length then malformed value
    else
      let row = { width; not_null; null; offsets; data } in
      let size = if not_null = 0 then 0 else stop value row (not_null - 1) in
      if data + size > length then malformed value else Ok row

let moved map id = if id < Iarray.length map then Iarray.get map id else None

let rewritten target value row added =
  let rec gather i acc =
    if i >= row.not_null then Ok acc
    else
      let start = if i = 0 then 0 else stop value row (i - 1) and finish = stop value row i in
      if start > finish then malformed value
      else gather (i + 1) (Option.fold ~none:acc ~some:(fun moved -> (moved, value, row.data + start, finish - start) :: acc) (target (id value row i)))
  in
  let%bind kept = gather 0 (List.map (fun (into, cell) -> (into, cell, 0, String.length cell)) added) in
  let kept = List.sort (fun (a, _, _, _) (b, _, _, _) -> Int.compare a b) kept
  and nulls = List.sort Int.compare (List.filter_map (fun i -> target (id value row (row.not_null + i))) (List.init row.null Fun.id)) in
  let size = List.fold_left (fun size (_, _, _, length) -> size + length) 0 kept
  and most = List.fold_left Int.max (List.fold_left (fun most (id, _, _, _) -> Int.max most id) 0 kept) nulls in
  let width = if most > 255 || size > 65535 then 4 else 1 in
  let offset_width = if width = 1 then 2 else 4 in
  let count = List.length kept and nulls_count = List.length nulls in
  let offsets = 6 + ((count + nulls_count) * width) in
  let data = offsets + (count * offset_width) in
  let bytes = Bytes.create (data + size) in
  let put pos width n = if width = 1 then Bytes.set_uint8 bytes pos n else if width = 2 then Bytes.set_uint16_le bytes pos n else Bytes.set_int32_le bytes pos (Int32.of_int n) in
  Bytes.set bytes 0 '\x80';
  Bytes.set_uint8 bytes 1 (if width = 1 then 0 else 1);
  Bytes.set_uint16_le bytes 2 count;
  Bytes.set_uint16_le bytes 4 nulls_count;
  List.iteri (fun i (id, _, _, _) -> put (6 + (i * width)) width id) kept;
  List.iteri (fun i id -> put (6 + ((count + i) * width)) width id) nulls;
  let (_ : int * int) =
    List.fold_left
      (fun (i, written) (_, source, from, length) ->
        Bytes.blit_string source from bytes (data + written) length;
        put (offsets + (i * offset_width)) offset_width (written + length);
        (i + 1, written + length))
      (0, 0) kept
  in
  Ok (Bytes.unsafe_to_string bytes)

let value t ~key value =
  let holds row wanted =
    let rec search low high =
      low < high
      &&
      let middle = (low + high) / 2 in
      let found = id value row middle in
      found = wanted || if found < wanted then search (middle + 1) high else search low middle
    in
    search 0 row.not_null || search row.not_null (row.not_null + row.null)
  in
  let restored map value =
    let length = String.length value in
    if length <= 9 then Ok value
    else
      let tail = Char.code value.[0] in
      let layout, first = if tail <= 1 && value.[1] = '\x7d' then (`Version_one, 3) else (`Version_zero, 1) in
      let past = length - tail in
      let rec options pos =
        if pos >= past then pos
        else
          match value.[pos] with
          | '\x7f' when pos + 3 <= past -> options (pos + 3 + String.get_uint16_be value (pos + 1))
          | '\x7e' -> options (pos + 9)
          | _ -> pos
      in
      let start = options first in
      if start >= past || value.[start] <> '\x80' then Ok value
      else
        let data = String.sub value start (past - start) in
        let%bind row = row data in
        let%bind moved = rewritten (moved map) data row [] in
        match layout with
        | `Version_one -> Ok (String.sub value 0 start ^ moved ^ String.sub value past tail)
        | `Version_zero ->
            let padding = Int.max 0 (10 - past) in
            let handle = tail - padding in
            let body = start + String.length moved in
            let padded = Int.max 0 (10 - body) in
            Ok
              (String.make 1 (Char.chr (padded + handle))
              ^ String.sub value 1 (start - 1)
              ^ moved ^ String.make padded '\x00'
              ^ String.sub value (length - handle) handle)
  in
  let filled row =
    let lacking = List.filter (function Filled { source; _ } | Unfilled { source; _ } -> not (holds row source)) t.needed in
    Option.fold
      ~none:(Ok (List.filter_map (function Filled { into; cell; _ } -> Some (into, cell) | Unfilled _ -> None) lacking))
      ~some:(fun name -> Error (`Renumber (Missing name)))
      (List.find_map (function Unfilled { name; _ } -> Some name | Filled _ -> None) lacking)
  in
  match (kind key, t.columns) with
  | Row, Kept -> (
      match t.needed with
      | [] -> Ok value
      | _ :: _ -> (
          let%bind row = row value in
          match%bind filled row with [] -> Ok value | _ :: _ as added -> rewritten Option.some value row added))
  | Row, Moved map ->
      let%bind row = row value in
      let%bind added = filled row in
      rewritten (moved map) value row added
  | Index, Moved map -> restored map value
  | Index, Kept | Other, _ -> Ok value
