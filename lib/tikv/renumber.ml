open Base.Result.Let_syntax

type error =
  | Changing of string
  | Handles_differ of { source : Info.handle; target : Info.handle }
  | Columns_differ of { source : string list; target : string list }
  | Indexes_differ of { source : string list; target : string list }
  | Partitions_differ of { source : string list; target : string list }
  | Rearranging
  | Global_index of string
  | Missing of string
  | Malformed of string
[@@deriving sexp_of ~portable]

(** Each source column id's id in the copy, by the source id. *)
type columns = Kept | Moved of int option iarray

(** What a row written before a column was added lacks in the copy. *)
type need =
  | Filled of { source : int; into : int; name : string; cell : string; datum : string option }
      (** The column's default then, in a row of either format; [None] when this does not write it in the first. *)
  | Unfilled of { source : int; name : string }  (** A default this does not write. *)

(** What keys under an id of the source are of. *)
type holder =
  | Physical  (** The table's or a partition's: rows and local indexes. *)
  | Logical  (** A partitioned table's own: its global indexes. *)

type t = {
  tables : (int64 * int64 * holder) iarray;  (** Each source id as its keys hold it, by that, with the copy's. *)
  partitions : (int64 * int64) iarray;  (** Each source partition's id as keys and values hold it, by that, with the copy's so held. *)
  indexes : (int64 * int64 * Info.scope) list;
  columns : columns;
  needed : need list;
  ranks : (int * int) list;  (** Each column id of the copy with its place among the copy's columns. *)
}

let tables t = List.map (fun (held, into, _) -> (Int64.logxor held Int64.min_int, into)) (Iarray.to_list t.tables)

let ranges t =
  List.concat_map
    (fun (held, into, holder) ->
      let source = Int64.logxor held Int64.min_int in
      match holder with
      | Physical -> [ (source, into, Codec.table source) ]
      | Logical ->
          let index id = fst (Codec.table source) ^ "_i" ^ String.init 8 (fun i -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical (Int64.logxor id Int64.min_int) (8 * (7 - i))) 0xFFL))) in
          List.filter_map
            (fun (id, _, (scope : Info.scope)) -> match scope with Global _ -> Some (source, into, (index id, index (Int64.succ id))) | Local -> None)
            (List.sort (fun (a, _, _) (b, _, _) -> Int64.compare a b) t.indexes))
    (Iarray.to_list t.tables)

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
    List.find_map
      (fun (index : Info.index) ->
        match (source.layout, index.scope) with
        | Whole, Global _ -> Some index.name
        | _, scope ->
            Option.bind
              (List.find_opt (fun (other : Info.index) -> other.name = index.name) target.indexes)
              (fun (other : Info.index) -> if other.scope = scope then None else Some index.name))
      source.indexes
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
                     ~some:(fun cell -> Filled { source = column.id; into = other.id; name = column.name; cell; datum = Datum.of_cell column.kind cell })
                     (Cell.of_default column.kind default))
            | Origin _, Origin _ | Unset, _ -> None)
          pairs
      in
      let indexes =
        List.filter_map
          (fun (index : Info.index) ->
            Option.map
              (fun (other : Info.index) -> (index.id, other.id, index.scope))
              (List.find_opt (fun (other : Info.index) -> other.name = index.name) target.indexes))
          source.indexes
      in
      let held id = Int64.logxor id Int64.min_int in
      let paired = List.map2 (fun (from : Info.partition) (into : Info.partition) -> (from.id, into.id)) (partitions source) (partitions target) in
      let tables =
        match paired with
        | [] -> [ (source.id, target.id, Physical) ]
        | _ :: _ ->
            List.map (fun (from, into) -> (from, into, Physical)) paired
            @ if List.exists (fun (_, _, scope) -> scope <> Info.Local) indexes then [ (source.id, target.id, Logical) ] else []
      in
      Ok
        {
          tables = Iarray.of_list (List.sort (fun (a, _, _) (b, _, _) -> Int64.compare a b) (List.map (fun (from, into, holder) -> (held from, into, holder)) tables));
          partitions = Iarray.of_list (List.sort (fun (a, _) (b, _) -> Int64.compare a b) (List.map (fun (from, into) -> (held from, held into)) paired));
          indexes;
          columns = moved;
          needed;
          ranks = List.mapi (fun rank (column : Info.column) -> (column.id, rank)) target.columns;
        }

(** What a key of the table is of. *)
type kind = Row | Index | Other

let kind key =
  let length = String.length key in
  if length >= 19 && key.[9] = '_' && key.[10] = 'i' then Index else if length >= 11 && key.[9] = '_' && key.[10] = 'r' then Row else Other

let malformed value = Error (`Renumber (Malformed (Hex_encode.to_hex value)))

let partition t held =
  let rec search low high =
    if low >= high then None
    else
      let middle = (low + high) / 2 in
      let from, into = Iarray.get t.partitions middle in
      match Int64.compare from held with 0 -> Some into | n when n < 0 -> search (middle + 1) high | _ -> search low middle
  in
  search 0 (Iarray.length t.partitions)

let key t key =
  let rec search tables key low high =
    if low >= high then -1
    else
      let middle = (low + high) / 2 in
      let held, _, _ = Iarray.get tables middle in
      match Int64.compare held (String.get_int64_be key 1) with
      | 0 -> middle
      | n when n < 0 -> search tables key (middle + 1) high
      | _ -> search tables key low middle
  in
  let found = if String.length key < 9 then -1 else search t.tables key 0 (Iarray.length t.tables) in
  if found < 0 then Ok None
  else
    let _, into, holder = Iarray.get t.tables found in
    match (kind key, holder) with
    | Index, _ -> (
        let id = Int64.logxor (String.get_int64_be key 11) Int64.min_int in
        let moved target =
          let bytes = Bytes.of_string key in
          Bytes.set_int64_be bytes 1 (Int64.logxor into Int64.min_int);
          Bytes.set_int64_be bytes 11 (Int64.logxor target Int64.min_int);
          bytes
        in
        match (List.find_opt (fun (source, _, _) -> Int64.equal source id) t.indexes, holder) with
        | Some (_, target, Local), Physical | Some (_, target, Global In_values), Logical -> Ok (Some (Bytes.unsafe_to_string (moved target)))
        | Some (_, target, Global In_keys_too), Logical ->
            let length = String.length key in
            let rec named pos =
              if pos = length then Ok (Bytes.unsafe_to_string (moved target))
              else if key.[pos] = '\x7e' then
                if pos + 18 = length && key.[pos + 9] = '\x03' then
                  Option.fold ~none:(malformed key)
                    ~some:(fun into ->
                      let bytes = moved target in
                      Bytes.set_int64_be bytes (pos + 1) into;
                      Ok (Bytes.unsafe_to_string bytes))
                    (partition t (String.get_int64_be key (pos + 1)))
                else malformed key
              else Option.fold ~none:(malformed key) ~some:(fun size -> named (pos + size)) (Datum.length key pos)
            in
            Result.map Option.some (named 19)
        | (None | Some (_, _, Global _)), Physical | (None | Some (_, _, Local)), Logical -> Ok None)
    | (Row | Other), Physical -> Ok (Some (Codec.retabled ~into key))
    | (Row | Other), Logical -> Ok None

(** A row of TiDB's second format: how wide an id is, how many columns hold a value and how many
    are NULL, where its end offsets and its data start. *)
type row = { width : int; not_null : int; null : int; offsets : int; data : int }

let id value row i = if row.width = 1 then Char.code value.[6 + i] else Int32.to_int (String.get_int32_le value (6 + (4 * i))) land 0xFFFF_FFFF

let stop value row i =
  if row.width = 1 then String.get_uint16_le value (row.offsets + (2 * i)) else Int32.to_int (String.get_int32_le value (row.offsets + (4 * i))) land 0xFFFF_FFFF

let row value =
  let length = String.length value in
  if length < 6 || value.[0] <> '\x80' then malformed value
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
  let entry scope columns value =
    let length = String.length value in
    if length <= 9 then match scope with Info.Local -> Ok value | Global _ -> malformed value
    else
      let tail = Char.code value.[0] in
      let layout, first = if tail <= 1 && value.[1] = '\x7d' then (`Version_one, 3) else (`Version_zero, 1) in
      let past = length - tail in
      let rec options pos named =
        if pos >= past then (pos, named)
        else
          match value.[pos] with
          | '\x7f' when pos + 3 <= past -> options (pos + 3 + String.get_uint16_be value (pos + 1)) named
          | '\x7e' -> options (pos + 9) (Some pos)
          | _ -> (pos, named)
      in
      let start, named = options first None in
      let%bind value =
        match (scope, named) with
        | Local, _ -> Ok value
        | Global _, Some pos when pos + 9 <= past ->
            Option.fold ~none:(malformed value)
              ~some:(fun into ->
                let bytes = Bytes.of_string value in
                Bytes.set_int64_be bytes (pos + 1) into;
                Ok (Bytes.unsafe_to_string bytes))
              (partition t (String.get_int64_be value (pos + 1)))
        | Global _, _ -> malformed value
      in
      match columns with
      | Kept -> Ok value
      | Moved _ when start >= past || value.[start] <> '\x80' -> Ok value
      | Moved map ->
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
  let filled held form =
    Base.List.fold_result (List.rev t.needed) ~init:[] ~f:(fun added need ->
        match need with
        | (Filled { source; _ } | Unfilled { source; _ }) when held source -> Ok added
        | Filled { into; name; cell; datum; _ } ->
            Option.fold ~none:(Error (`Renumber (Missing name))) ~some:(fun value -> Ok ((into, value) :: added)) (form cell datum)
        | Unfilled { name; _ } -> Error (`Renumber (Missing name)))
  in
  let first target =
    let length = String.length value in
    let rec pairs pos acc =
      if pos >= length then Ok (List.rev acc)
      else
        match Option.bind (Datum.id value pos) (fun (id, at) -> Option.map (fun size -> (id, at, size)) (Datum.length value at)) with
        | Some (id, at, size) -> pairs (at + size) ((id, at, size) :: acc)
        | None -> malformed value
    in
    let%bind held = if String.equal value "\x00" then Ok [] else pairs 0 [] in
    let%bind added = filled (fun source -> List.exists (fun (id, _, _) -> id = source) held) (fun _ datum -> datum) in
    let kept = List.filter_map (fun (id, at, size) -> Option.map (fun into -> (into, String.sub value at size)) (target id)) held @ added in
    let rank id = Option.value ~default:Int.max_int (List.assoc_opt id t.ranks) in
    match (t.columns, added, List.stable_sort (fun (a, _) (b, _) -> Int.compare (rank a) (rank b)) kept) with
    | Kept, [], _ -> Ok value
    | _, _, [] -> Ok "\x00"
    | _, _, kept -> Ok (String.concat "" (List.concat_map (fun (into, datum) -> [ Datum.of_id into; datum ]) kept))
  in
  let second target =
    let%bind row = row value in
    let%bind added = filled (holds row) (fun cell _ -> Some cell) in
    match (t.columns, added) with Kept, [] -> Ok value | (Kept | Moved _), _ -> rewritten target value row added
  in
  let format = if String.length value > 0 && value.[0] = '\x80' then second else first in
  match (kind key, t.columns) with
  | Row, Kept -> ( match t.needed with [] -> Ok value | _ :: _ -> format Option.some)
  | Row, Moved map -> format (moved map)
  | Index, columns ->
      let id = Int64.logxor (String.get_int64_be key 11) Int64.min_int in
      entry
        (Option.fold ~none:Info.Local ~some:(fun (_, _, scope) -> scope) (List.find_opt (fun (source, _, _) -> Int64.equal source id) t.indexes))
        columns value
  | Other, _ -> Ok value
