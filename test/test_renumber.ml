open Sexplib0.Sexp_conv
open Quiver_tikv

(* A column's place in a row. *)
type cell = Absent | Null | Held of string [@@deriving sexp_of]

(* What a source row puts after its data. *)
type trailer = Plain | Checksum of int | Forced_large [@@deriving sexp_of]

(* What the source reads for a row written without the column. *)
type origin = Unset | Writable of string | Unwritable [@@deriving sexp_of]

type column = { name : string; source : int; target : int; origin : origin } [@@deriving sexp_of]

type case = {
  columns : column list;
  dropped : int list;  (* Ids of columns the source no longer has, still in old rows. *)
  rows : ((cell list * (int * string) list) * trailer) list;
}
[@@deriving sexp_of]

let encode ~large cells =
  let held = List.sort compare (List.filter_map (fun (id, cell) -> match cell with Held bytes -> Some (id, bytes) | Null | Absent -> None) cells)
  and nulls = List.sort compare (List.filter_map (fun (id, cell) -> match cell with Null -> Some id | Held _ | Absent -> None) cells) in
  let size = List.fold_left (fun size (_, bytes) -> size + String.length bytes) 0 held in
  let most = List.fold_left max (List.fold_left (fun most (id, _) -> max most id) 0 held) nulls in
  let large = large || most > 255 || size > 65535 in
  let buffer = Buffer.create 64 in
  let id n = if large then Buffer.add_int32_le buffer (Int32.of_int n) else Buffer.add_uint8 buffer n in
  Buffer.add_char buffer '\x80';
  Buffer.add_uint8 buffer (if large then 1 else 0);
  Buffer.add_uint16_le buffer (List.length held);
  Buffer.add_uint16_le buffer (List.length nulls);
  List.iter (fun (n, _) -> id n) held;
  List.iter id nulls;
  let _ =
    List.fold_left
      (fun stop (_, bytes) ->
        let stop = stop + String.length bytes in
        if large then Buffer.add_int32_le buffer (Int32.of_int stop) else Buffer.add_uint16_le buffer stop;
        stop)
      0 held
  in
  List.iter (fun (_, bytes) -> Buffer.add_string buffer bytes) held;
  Buffer.contents buffer

let with_trailer row = function
  | Plain | Forced_large -> row
  | Checksum extra ->
      let flagged = Bytes.of_string row in
      Bytes.set_uint8 flagged 1 (Bytes.get_uint8 flagged 1 lor 2);
      Bytes.to_string flagged ^ String.make (if extra = 0 then 5 else 9) '\x07'

let decode row =
  let failed what = Error what in
  if String.length row < 6 || row.[0] <> '\x80' then failed "no version"
  else
    let flags = Char.code row.[1] in
    let large = flags land 1 = 1 in
    if flags land 2 <> 0 then failed "a checksum flag"
    else
      let held = String.get_uint16_le row 2 and nulls = String.get_uint16_le row 4 in
      let width = if large then 4 else 1 in
      let id i = if large then Int32.to_int (String.get_int32_le row (6 + (4 * i))) else Char.code row.[6 + i] in
      let offsets = 6 + ((held + nulls) * width) in
      let stop i = if large then Int32.to_int (String.get_int32_le row (offsets + (4 * i))) else String.get_uint16_le row (offsets + (2 * i)) in
      let data = offsets + (held * if large then 4 else 2) in
      let ascending ids = List.for_all2 (fun a b -> a < b) (List.filteri (fun i _ -> i < List.length ids - 1) ids) (List.tl ids) in
      let held_ids = List.init held id and null_ids = List.init nulls (fun i -> id (held + i)) in
      let stops = List.init held stop in
      if data > String.length row then failed "short"
      else if (held > 1 && not (ascending held_ids)) || (nulls > 1 && not (ascending null_ids)) then failed "ids out of order"
      else if data + (if held = 0 then 0 else List.nth stops (held - 1)) <> String.length row then failed "data not to the end"
      else
        let starts = List.filteri (fun i _ -> i < held) (0 :: stops) in
        if List.exists2 (fun start stop -> start > stop) starts stops then failed "offsets backwards"
        else
          Ok
            (List.sort compare
               (List.map2 (fun n (start, stop) -> (n, Some (String.sub row (data + start) (stop - start)))) held_ids (List.combine starts stops)
               @ List.map (fun n -> (n, None)) null_ids))

let info ~id ~ids ~origins (columns : column list) : Info.t =
  let described column : Info.missing * Info.kind =
    match origins column with
    | Unset -> (Unset, Bytes)
    | Writable text -> (Origin text, Bytes)
    | Unwritable -> (Origin "{}", Json)
  in
  {
    id;
    columns =
      List.map
        (fun (column : column) ->
          let missing, kind = described column in
          { Info.name = column.name; id = ids column; state = Public; missing; kind })
        columns;
    indexes = [];
    handle = Row_number;
    layout = Whole;
  }

let gen =
  QCheck2.Gen.(
    let* count = int_range 1 8 in
    let* pool = oneof_weighted [ (3, return 30); (1, return 300) ] in
    let* shuffled = shuffle_list (List.init pool (fun i -> i + 1)) in
    let* kept = oneof_weighted [ (1, return true); (3, return false) ] in
    let* dropped_count = int_range 0 2 in
    let* base = oneof_weighted [ (4, return 1); (1, return 254) ] in
    let sources = if kept then List.init count (fun i -> i + 1) else List.filteri (fun i _ -> i < count) shuffled in
    let dropped = List.filteri (fun i _ -> i >= count && i < count + dropped_count) shuffled in
    let dropped = if kept then List.map (fun id -> id + count) dropped else dropped in
    let* origins =
      list_size (return count)
        (oneof_weighted [ (5, return Unset); (2, map (fun text -> Writable text) (string_size (int_range 0 9))); (1, return Unwritable) ])
    in
    let columns =
      List.mapi (fun i (source, origin) -> { name = Printf.sprintf "c%d" i; source; target = (if kept then source else base + i); origin }) (List.combine sources origins)
    in
    let bytes = oneof_weighted [ (20, string_size (int_range 0 12)); (1, string_size (return 66_000)) ] in
    let cell = oneof_weighted [ (2, return Absent); (2, return Null); (5, map (fun bytes -> Held bytes) bytes) ] in
    let row =
      pair
        (pair (list_size (return count) cell) (flatten_list (List.map (fun id -> map (fun bytes -> (id, bytes)) (string_size (int_range 0 5))) dropped)))
        (oneof [ return Plain; map (fun extra -> Checksum extra) (int_range 0 1); return Forced_large ])
    in
    let* rows = list_size (int_range 1 6) row in
    return { columns; dropped; rows })

let source_row case ((cells, dropped), trailer) =
  let large = match trailer with Forced_large -> true | Plain | Checksum _ -> false in
  with_trailer (encode ~large (List.map2 (fun column cell -> (column.source, cell)) case.columns cells @ List.map (fun (id, bytes) -> (id, Held bytes)) dropped)) trailer

let rows =
  QCheck2.Test.make ~count:3000
    ~name:
      "a row reads under the copy's ids as the source's under its own, with the defaults it was written before, \
       without dropped columns when ids move, without a checksum"
    ~print:(fun case -> Base.Sexp.to_string_hum (sexp_of_case case))
    gen
    (fun case ->
      let source = info ~id:75L ~ids:(fun column -> column.source) ~origins:(fun column -> column.origin) case.columns
      and target = info ~id:300L ~ids:(fun column -> column.target) ~origins:(fun _ -> Unset) case.columns in
      match Renumber.make ~source ~target with
      | Error _ -> false
      | Ok plan ->
          List.for_all
            (fun ((((cells, dropped), _) as row) : (cell list * (int * string) list) * trailer) ->
              let value = source_row case row in
              let key = fst (Codec.records 75L) ^ "\x80\x00\x00\x00\x00\x00\x00\x01" in
              let cells = List.combine case.columns cells in
              let lacking = List.filter_map (fun (column, cell) -> match (column.origin, cell) with Unwritable, Absent -> Some column.name | _ -> None) cells
              and filled = List.exists (fun (column, cell) -> match (column.origin, cell) with Writable _, Absent -> true | _ -> false) cells in
              let kept = List.for_all (fun column -> column.source = column.target) case.columns in
              match (Renumber.value plan ~key value, lacking) with
              | Error (`Renumber (Renumber.Missing name)), _ :: _ -> List.mem name lacking
              | Error _, _ -> false
              | Ok _, _ :: _ -> false
              | Ok moved, [] when kept && not filled -> moved == value
              | Ok moved, [] -> (
                  let expected =
                    List.sort compare
                      (List.filter_map
                         (fun (column, cell) ->
                           match (cell, column.origin) with
                           | Absent, Writable text -> Some (column.target, Some text)
                           | Absent, (Unset | Unwritable) -> None
                           | Null, _ -> Some (column.target, None)
                           | Held bytes, _ -> Some (column.target, Some bytes))
                         cells
                      @ if kept then List.map (fun (id, bytes) -> (id, Some bytes)) dropped else [])
                  in
                  match decode moved with Ok got -> got = expected | Error _ -> false))
            case.rows)

(* Where an index value is laid out as TiDB writes it, and what it carries besides its columns. *)
type index_value = {
  version_one : bool;
  common : string option;
  partition : string option;
  restored : cell list option;
  handle : string option;
}
[@@deriving sexp_of]

let index_value case (value : index_value) =
  let restored = Option.map (fun cells -> encode ~large:false (List.map2 (fun column cell -> (column.source, cell)) case.columns cells)) value.restored in
  let segments =
    Option.fold ~none:"" ~some:(fun handle -> "\x7f" ^ String.init 2 (fun i -> Char.chr ((String.length handle lsr (8 * (1 - i))) land 0xff)) ^ handle) value.common
    ^ Option.fold ~none:"" ~some:(fun id -> "\x7e" ^ id) value.partition
    ^ Option.value ~default:"" restored
  in
  if value.version_one then "\x00\x7d\x01" ^ segments
  else
    let body = 1 + String.length segments in
    let padding = max 0 (10 - body) in
    let handle = Option.value ~default:"" value.handle in
    String.make 1 (Char.chr (padding + String.length handle)) ^ segments ^ String.make padding '\x00' ^ handle

let indexes =
  QCheck2.Test.make ~count:3000 ~name:"an index value keeps all it holds, its restored columns under the copy's ids, its padding and handle right"
    ~print:(fun (case, values) -> Base.Sexp.to_string_hum ([%sexp_of: case * index_value list] (case, values)))
    QCheck2.Gen.(
      let* case = gen in
      let count = List.length case.columns in
      let cell = oneof [ return Absent; return Null; map (fun bytes -> Held bytes) (string_size (int_range 0 6)) ] in
      let value =
        let* version_one = bool in
        let* common = option (string_size (int_range 9 20)) in
        let* partition = option (string_size (return 8)) in
        let* restored = option (list_size (return count) cell) in
        let* handle = option (string_size (return 8)) in
        return { version_one; common; partition; restored; handle = (if version_one || Option.is_some common then None else handle) }
      in
      pair (return case) (list_size (int_range 1 5) value))
    (fun (case, values) ->
      let source = info ~id:75L ~ids:(fun column -> column.source) ~origins:(fun _ -> Unset) case.columns
      and target = info ~id:300L ~ids:(fun column -> column.target) ~origins:(fun _ -> Unset) case.columns in
      match Renumber.make ~source ~target with
      | Error _ -> false
      | Ok plan ->
          let key = fst (Codec.table 75L) ^ "_i\x80\x00\x00\x00\x00\x00\x00\x01\x01abc" in
          List.for_all
            (fun (value : index_value) ->
              let bytes = index_value case value in
              match Renumber.value plan ~key bytes with
              | Error _ -> false
              | Ok moved when String.length bytes <= 9 -> moved = bytes
              | Ok moved -> moved = index_value { case with columns = List.map (fun column -> { column with source = column.target }) case.columns } value)
            values)

(* Which table a key is under. *)
type under = Own | Partition of int | Foreign [@@deriving sexp_of]

let keys =
  QCheck2.Test.make ~count:2000 ~name:"keys move to the copy's table, partition and index ids, keys of other indexes and tables stay behind"
    ~print:(fun (partitions, indexes, keys) ->
      Base.Sexp.to_string_hum ([%sexp_of: int * (int * int * bool) list * (under * int * string) list] (partitions, indexes, keys)))
    QCheck2.Gen.(
      let* partitions = oneof_weighted [ (2, return 0); (3, int_range 1 4) ] in
      triple (return partitions)
        (list_size (int_range 0 5) (triple (int_range 1 40) (int_range 1 40) bool))
        (list_size (int_range 1 20)
           (triple
              (oneof_weighted [ (2, return Own); (3, map (fun i -> Partition i) (int_range 0 (Int.max 0 (partitions - 1)))); (1, return Foreign) ])
              (int_range (-1) 45) (string_size (int_range 0 6)))))
    (fun (partitions, indexes, keys) ->
      let indexes = List.sort_uniq (fun (a, _, _) (b, _, _) -> compare a b) indexes in
      let targets = List.mapi (fun i (source, _, public) -> (source, i + 1, public)) indexes in
      let state public : Info.state = if public then Public else Changing in
      let layout listed : Info.layout = match listed with first :: rest -> Partitioned (first, rest) | [] -> Whole in
      let named ids = List.map (fun i : Info.partition -> { name = Printf.sprintf "p%d" i; id = Int64.of_int (ids i) }) (List.init partitions Fun.id) in
      let source : Info.t =
        {
          id = 75L;
          columns = [];
          indexes = List.map (fun (id, _, public) -> { Info.name = string_of_int id; id = Int64.of_int id; state = state public; scope = Local }) targets;
          handle = Row_number;
          layout = layout (named (fun i -> 80 + (((5 * i) + 2) mod 7)));
        }
      in
      let target : Info.t =
        {
          source with
          id = 300L;
          indexes = List.map (fun (id, moved, _) -> { Info.name = string_of_int id; id = Int64.of_int moved; state = Public; scope = Local }) targets;
          layout = layout (List.rev (named (fun i -> 400 + partitions - 1 - i)));
        }
      in
      let public = List.filter (fun (_, _, public) -> public) targets in
      let source = { source with indexes = List.filter (fun (index : Info.index) -> index.state = Public) source.indexes }
      and target = { target with indexes = List.filter (fun (index : Info.index) -> List.exists (fun (id, _, _) -> string_of_int id = index.name) public) target.indexes } in
      match Renumber.make ~source ~target with
      | Error _ -> false
      | Ok plan ->
          let id n = String.init 8 (fun i -> Char.chr (Int64.to_int (Int64.logand (Int64.shift_right_logical (Int64.logxor (Int64.of_int n) Int64.min_int) (8 * (7 - i))) 0xFFL))) in
          let prefix table = fst (Codec.table table) in
          let into under =
            match (under, partitions) with
            | Own, 0 -> Some (75L, 300L)
            | Partition _, 0 -> Some (75L, 300L)
            | Partition i, _ -> Some (Int64.of_int (80 + (((5 * i) + 2) mod 7)), Int64.of_int (400 + partitions - 1 - i))
            | Own, _ | Foreign, _ -> None
          in
          let table under = match (under, partitions) with Foreign, _ -> 999L | Own, _ | Partition _, 0 -> 75L | Partition i, _ -> Int64.of_int (80 + (((5 * i) + 2) mod 7)) in
          List.for_all
            (fun (under, index, suffix) ->
              let from = table under and into = into under in
              match index with
              | -1 -> Renumber.key plan (prefix from ^ "_r" ^ suffix) = Option.map (fun (_, into) -> prefix into ^ "_r" ^ suffix) into
              | index -> (
                  let key = prefix from ^ "_i" ^ id index ^ suffix in
                  match (List.find_opt (fun (source, _, _) -> source = index) public, into) with
                  | Some (_, moved, _), Some (_, into) -> Renumber.key plan key = Some (prefix into ^ "_i" ^ id moved ^ suffix)
                  | _ -> Renumber.key plan key = None))
            keys
          && Renumber.tables plan
             = List.sort compare
                 (if partitions = 0 then [ (75L, 300L) ]
                  else List.init partitions (fun i -> (Int64.of_int (80 + (((5 * i) + 2) mod 7)), Int64.of_int (400 + partitions - 1 - i))))
          && List.for_all (fun (from, into) -> Renumber.key plan (prefix from) = Some (prefix into)) (Renumber.tables plan))

let uvarint n =
  let buffer = Buffer.create 10 in
  let rec go n =
    if Int64.unsigned_compare n 0x80L < 0 then Buffer.add_char buffer (Char.chr (Int64.to_int n))
    else (
      Buffer.add_char buffer (Char.chr (Int64.to_int (Int64.logand n 0x7FL) lor 0x80));
      go (Int64.shift_right_logical n 7))
  in
  go n;
  Buffer.contents buffer

let varint n = uvarint (if Int64.compare n 0L >= 0 then Int64.shift_left n 1 else Int64.lognot (Int64.shift_left n 1))
let compact bytes = "\x02" ^ varint (Int64.of_int (String.length bytes)) ^ bytes

(* Values of every kind a row of the first format may hold, as TiDB's [codec] writes them. *)
let datum =
  QCheck2.Gen.(
    let int64 = oneof [ map Int64.of_int (int_range (-300) 300); map Int64.of_int int; return Int64.min_int; return Int64.max_int ] in
    let eight = string_size ~gen:char (return 8) in
    let groups bytes =
      let rec go pos acc =
        let chunk = String.sub bytes pos (Int.min 8 (String.length bytes - pos)) in
        let pad = 8 - String.length chunk in
        let acc = acc ^ chunk ^ String.make pad '\x00' in
        if pad = 0 then go (pos + 8) (acc ^ "\xff") else acc ^ String.make 1 (Char.chr (0xff - pad))
      in
      go 0 ""
    in
    let json =
      oneof
        [
          map (fun inner -> "\x01" ^ "\x00\x00\x00\x00" ^ (let size = String.length inner + 8 in String.init 4 (fun i -> Char.chr ((size lsr (8 * i)) land 0xff))) ^ inner) (string_size (int_range 0 20));
          map (fun inner -> "\x03" ^ "\x01\x00\x00\x00" ^ (let size = String.length inner + 8 in String.init 4 (fun i -> Char.chr ((size lsr (8 * i)) land 0xff))) ^ inner) (string_size (int_range 0 20));
          map (fun text -> "\x0c" ^ uvarint (Int64.of_int (String.length text)) ^ text) (string_size (int_range 0 200));
          map (fun bytes -> "\x09" ^ bytes) eight;
          map (fun bytes -> "\x0b" ^ bytes) eight;
          map (fun bytes -> "\x0f" ^ bytes) eight;
          return "\x04\x01";
          map (fun text -> "\x0d\xfc" ^ uvarint (Int64.of_int (String.length text)) ^ text) (string_size (int_range 0 10));
          map (fun bytes -> "\x11" ^ bytes) (string_size (return 12));
        ]
    in
    oneof
      [
        map (fun n -> "\x08" ^ varint n) int64;
        map (fun n -> "\x09" ^ uvarint n) int64;
        map compact (string_size (int_range 0 300));
        map (fun bytes -> "\x01" ^ groups bytes) (string_size (int_range 0 30));
        map (fun (flag, bytes) -> String.make 1 flag ^ bytes) (pair (oneof_list [ '\x03'; '\x04'; '\x05'; '\x07' ]) eight);
        map (fun (precision, frac) ->
            let precision = Int.max precision frac in
            let bin digits = (digits / 9 * 4) + [| 0; 1; 1; 2; 2; 3; 3; 4; 4; 4 |].(digits mod 9) in
            "\x06" ^ String.make 1 (Char.chr precision) ^ String.make 1 (Char.chr frac) ^ String.make (bin (precision - frac) + bin frac) '\x55')
          (pair (int_range 1 65) (int_range 0 30));
        map (fun inner -> "\x0a" ^ inner) json;
        map (fun count -> "\x14" ^ String.init 4 (fun i -> Char.chr ((count lsr (8 * i)) land 0xff)) ^ String.make (4 * count) '\x3f') (int_range 0 5);
      ])

let first_format =
  QCheck2.Test.make ~count:3000
    ~name:
      "a row of the first format reads under the copy's ids, in the copy's order, with the defaults it was written \
       before, without dropped columns when ids move, and stays when nothing in it moves"
    ~print:(fun (case, rows) -> Base.Sexp.to_string_hum (sexp_of_case case) ^ "\n" ^ String.concat "\n" (List.map Hex_encode.to_hex rows))
    QCheck2.Gen.(
      let* case = gen in
      let* data = list_size (return (List.length case.rows * (List.length case.columns + List.length case.dropped))) datum in
      return (case, data))
    (fun (case, data) ->
      let source = info ~id:75L ~ids:(fun column -> column.source) ~origins:(fun column -> column.origin) case.columns
      and target = info ~id:300L ~ids:(fun column -> column.target) ~origins:(fun _ -> Unset) case.columns in
      let key = fst (Codec.records 75L) ^ "\x80\x00\x00\x00\x00\x00\x00\x01" in
      let encoded pairs = match pairs with [] -> "\x00" | _ -> String.concat "" (List.map (fun (id, value) -> "\x08" ^ varint (Int64.of_int id) ^ value) pairs) in
      match Renumber.make ~source ~target with
      | Error _ -> false
      | Ok plan ->
          let kept = List.for_all (fun column -> column.source = column.target) case.columns in
          let width = List.length case.columns + List.length case.dropped in
          List.for_all
            (fun (index, ((cells, dropped), _)) ->
              let values = Array.of_list (List.filteri (fun i _ -> i >= index * width && i < (index + 1) * width) data) in
              let value i (cell : cell) = match cell with Null -> Some "\x00" | Held _ -> Some values.(i) | Absent -> None in
              let cells = List.combine case.columns cells in
              let present = List.filter_map (fun (i, (column, cell)) -> Option.map (fun v -> (column, v)) (value i cell)) (List.mapi (fun i c -> (i, c)) cells) in
              let gone = List.mapi (fun i (id, _) -> (id, values.(List.length case.columns + i))) dropped in
              let row = encoded (List.map (fun (column, v) -> (column.source, v)) present @ gone) in
              let lacking = List.filter_map (fun (column, cell) -> match (column.origin, cell) with Unwritable, Absent -> Some column.name | _ -> None) cells
              and filled = List.exists (fun (column, cell) -> match (column.origin, cell) with Writable _, Absent -> true | _ -> false) cells in
              match (Renumber.value plan ~key row, lacking) with
              | Error (`Renumber (Renumber.Missing name)), _ :: _ -> List.mem name lacking
              | Error _, _ | Ok _, _ :: _ -> false
              | Ok moved, [] when kept && not filled -> moved == row
              | Ok moved, [] ->
                  let expected =
                    List.filter_map
                      (fun (column, cell) ->
                        match (cell, column.origin) with
                        | Absent, Writable text -> Some (column.target, compact text)
                        | Absent, (Unset | Unwritable) -> None
                        | (Null | Held _), _ -> Option.map (fun v -> (column.target, v)) (List.assoc_opt column present))
                      cells
                    @ if kept then gone else []
                  in
                  String.equal moved (encoded expected))
            (List.mapi (fun i row -> (i, row)) case.rows))

let refusals () =
  let column name id : Info.column = { name; id; state = Public; missing = Unset; kind = Bytes } in
  let table columns : Info.t = { id = 1L; columns; indexes = [ { name = "k"; id = 1L; state = Public; scope = Local } ]; handle = Row_number; layout = Whole } in
  let shown source target =
    match Renumber.make ~source ~target with Ok _ -> "made" | Error (`Renumber error) -> Base.Sexp.to_string (Renumber.sexp_of_error error)
  in
  let a = table [ column "a" 1; column "b" 2 ] in
  Alcotest.(check string) "the same" "made" (shown a a);
  Alcotest.(check string) "a column of its own" "(Columns_differ(source(a b))(target(a c)))" (shown a (table [ column "a" 1; column "c" 2 ]));
  Alcotest.(check string) "an index of its own" "(Indexes_differ(source(k))(target(j)))" (shown a { a with indexes = [ { name = "j"; id = 1L; state = Public; scope = Local } ] });
  let partitioned names : Info.layout =
    match List.mapi (fun i name : Info.partition -> { name; id = Int64.of_int (10 + i) }) names with first :: rest -> Partitioned (first, rest) | [] -> Whole
  in
  Alcotest.(check string) "partitioned the same" "made" (shown { a with layout = partitioned [ "p"; "q" ] } { a with layout = partitioned [ "q"; "p" ] });
  Alcotest.(check string) "a partition of its own" "(Partitions_differ(source(p q))(target(p r)))"
    (shown { a with layout = partitioned [ "p"; "q" ] } { a with layout = partitioned [ "p"; "r" ] });
  Alcotest.(check string) "partitioned against not" "(Partitions_differ(source(p))(target()))" (shown { a with layout = partitioned [ "p" ] } a);
  Alcotest.(check string) "partitions being rearranged" "Rearranging" (shown a { a with layout = Rearranging });
  Alcotest.(check string) "a global index" "(Global_index k)"
    (shown { a with indexes = [ { name = "k"; id = 1L; state = Public; scope = Global } ] } a);
  Alcotest.(check string) "another handle" "(Handles_differ(source Row_number)(target Integer_key))" (shown a { a with handle = Integer_key });
  Alcotest.(check string) "a column being added" "(Changing b)" (shown { a with columns = [ column "a" 1; { (column "b" 2) with state = Changing } ] } a);
  Alcotest.(check string) "an index being built" "(Changing k)" (shown a { a with indexes = [ { name = "k"; id = 2L; state = Changing; scope = Local } ] });
  let plan = Result.get_ok (Renumber.make ~source:(table [ column "a" 2 ]) ~target:(table [ column "a" 1 ])) in
  let row = fst (Codec.records 1L) ^ "\x80\x00\x00\x00\x00\x00\x00\x01" in
  let value bytes = match Renumber.value plan ~key:row bytes with Ok _ -> "moved" | Error (`Renumber error) -> Base.Sexp.to_string (Renumber.sexp_of_error error) in
  Alcotest.(check string) "a row of the first format" "moved" (value "\x08\x04\x08\x04");
  Alcotest.(check string) "an empty row of the first format" "moved" (value "\x00");
  Alcotest.(check string) "a value of the first format cut short" "(Malformed 0804020A6162)" (value "\x08\x04\x02\x0a\x61\x62");
  Alcotest.(check string) "an unknown flag" "(Malformed 0804FA)" (value "\x08\x04\xfa");
  Alcotest.(check string) "a row cut short" "(Malformed 800001000000020A)" (value "\x80\x00\x01\x00\x00\x00\x02\x0a")

let padded () =
  let case = { columns = [ { name = "a"; source = 300; target = 1; origin = Unset } ]; dropped = []; rows = [] } in
  let source = info ~id:75L ~ids:(fun column -> column.source) ~origins:(fun _ -> Unset) case.columns
  and target = info ~id:300L ~ids:(fun column -> column.target) ~origins:(fun _ -> Unset) case.columns in
  let plan = Result.get_ok (Renumber.make ~source ~target) in
  let value = { version_one = false; common = None; partition = None; restored = Some [ Null ]; handle = Some "HHHHHHHH" } in
  let key = fst (Codec.table 75L) ^ "_i\x80\x00\x00\x00\x00\x00\x00\x01\x01" in
  let wide = index_value case value in
  let narrow = index_value { case with columns = [ { (List.hd case.columns) with source = 1 } ] } value in
  Alcotest.(check int) "the source's restored row is wide, with no padding" 19 (String.length wide);
  Alcotest.(check string) "the copy's is narrow and padded to ten before the handle" (Hex_encode.to_hex narrow)
    (Hex_encode.to_hex (Result.get_ok (Renumber.value plan ~key wide)));
  Alcotest.(check string) "padding" "0A8000000001000100004848484848484848" (Hex_encode.to_hex narrow)

let defaults () =
  let cases =
    List.filter_map
      (fun line -> match String.split_on_char '\t' line with [ name; table; before; after ] -> Some (name, table, before, after) | _ -> None)
      (In_channel.with_open_bin "tikv/defaults.tsv" In_channel.input_lines)
  in
  Alcotest.(check bool) "every case read" true (List.length cases >= 60);
  List.iter
    (fun (name, table, before, after) ->
      let source = Result.get_ok (Info.of_json table) in
      let target = { source with columns = List.map (fun (column : Info.column) -> { column with missing = Unset }) source.columns } in
      let plan = Result.get_ok (Renumber.make ~source ~target) in
      let key = fst (Codec.records source.id) ^ "\x80\x00\x00\x00\x00\x00\x00\x01" in
      let value hex =
        match Renumber.value plan ~key (Hex_encode.from_hex hex) with
        | Ok value -> String.lowercase_ascii (Hex_encode.to_hex value)
        | Error (`Renumber error) -> Base.Sexp.to_string (Renumber.sexp_of_error error)
      in
      Alcotest.(check string) (name ^ ": the row written before gets the default as TiDB writes it") after (value before);
      Alcotest.(check string) (name ^ ": the row written after stays") after (value after))
    cases

let first_defaults () =
  let cases =
    List.filter_map
      (fun line -> match String.split_on_char '\t' line with [ name; table; before; after ] -> Some (name, table, before, after) | _ -> None)
      (In_channel.with_open_bin "tikv/defaults-v1.tsv" In_channel.input_lines)
  in
  Alcotest.(check bool) "every case read" true (List.length cases >= 35);
  List.iter
    (fun (name, table, before, after) ->
      let source = Result.get_ok (Info.of_json table) in
      let target = { source with columns = List.map (fun (column : Info.column) -> { column with missing = Unset }) source.columns } in
      let plan = Result.get_ok (Renumber.make ~source ~target) in
      let key = fst (Codec.records source.id) ^ "\x80\x00\x00\x00\x00\x00\x00\x01" in
      let value hex =
        match Renumber.value plan ~key (Hex_encode.from_hex hex) with
        | Ok value -> String.lowercase_ascii (Hex_encode.to_hex value)
        | Error (`Renumber error) -> Base.Sexp.to_string (Renumber.sexp_of_error error)
      in
      Alcotest.(check string) (name ^ ": the row of the first format written before gets the default as TiDB writes it") after (value before);
      Alcotest.(check string) (name ^ ": the row written after stays") after (value after))
    cases

let cells () =
  let shown kind text = Option.fold ~none:"none" ~some:(fun cell -> String.lowercase_ascii (Hex_encode.to_hex cell)) (Cell.of_default kind text) in
  let check name expected kind text = Alcotest.(check string) name expected (shown kind text) in
  check "a timestamp of a server's zone" "none" (Timestamp { clock = Server; fsp = 0 }) "2020-01-02 03:04:05";
  check "finer than the column" "none" (Datetime { fsp = 0 }) "2020-01-02 03:04:05.5";
  check "as fine as the column" "20a107053144a519" (Datetime { fsp = 1 }) "2020-01-02 03:04:05.5";
  check "a date with a time" "none" Date "2020-01-02 00:00:00";
  check "a month too many" "none" Date "2020-13-01";
  check "JSON other than null" "none" Json "{}";
  check "too many integer digits" "none" (Decimal { precision = 3; scale = 1 }) "123.4";
  check "rounded up into one too many" "none" (Decimal { precision = 3; scale = 1 }) "99.95";
  check "rounded up within" "0301e309" (Decimal { precision = 3; scale = 1 }) "99.94";
  check "a negative zero" "020080" (Decimal { precision = 2; scale = 0 }) "-0";
  check "an exponent" "none" (Decimal { precision = 5; scale = 0 }) "1e3";
  check "a year of three digits" "none" Year "123";
  check "a year out of range" "none" Year "1900";
  check "seventy" "b207" Year "70";
  check "sixty-nine" "1508" Year "69";
  check "no such member" "none" (Enum [ "a" ]) "b";
  check "a member of another case" "none" (Enum [ "a" ]) "A";
  check "a set with no such member" "none" (Set [ "a"; "b" ]) "a,c";
  check "nine bytes of bits" "none" Bit "123456789";
  check "not a number" "none" (Integer Signed) "12a";
  check "a sign alone" "none" (Integer Signed) "-";
  check "an underscore" "none" (Integer Signed) "1_000";
  check "hexadecimal" "none" (Integer Unsigned) "0x10";
  check "unsigned and negative" "none" (Integer Unsigned) "-1";
  check "past the unsigned" "none" (Integer Unsigned) "18446744073709551616";
  check "past the signed" "none" (Integer Signed) "9223372036854775808";
  check "infinite" "none" Double "1e999";
  check "not a number at all" "none" Double "nan";
  check "a time of four digits of hours" "none" (Time { fsp = 0 }) "1000:00:00";
  check "a negative time" "00c0a68cf8ffffff" (Time { fsp = 0 }) "-00:00:32";
  check "a kind it does not write" "none" Unknown "1"

let widths =
  QCheck2.Test.make ~count:3000 ~name:"an integer default is the narrowest little-endian cell that reads back the same"
    ~print:(fun (n, sign) -> Printf.sprintf "%Ld %s" n (match sign with `Signed -> "signed" | `Unsigned -> "unsigned"))
    QCheck2.Gen.(
      pair
        (oneof [ int64; map Int64.of_int (int_range (-70000) 70000); oneof_list [ 127L; 128L; -128L; -129L; 255L; 256L; 65535L; 65536L; 0xFFFF_FFFFL; 0x1_0000_0000L; Int64.min_int; Int64.max_int; -1L ] ])
        (oneof_list [ `Signed; `Unsigned ]))
    (fun (n, sign) ->
      let kind, text =
        match sign with `Signed -> (Info.Integer Signed, Int64.to_string n) | `Unsigned -> (Info.Integer Unsigned, Printf.sprintf "%Lu" n)
      in
      match Cell.of_default kind text with
      | None -> false
      | Some cell ->
          let width = String.length cell in
          let padded = Bytes.make 8 '\000' in
          Bytes.blit_string cell 0 padded 0 width;
          let read = Bytes.get_int64_le padded 0 in
          let back =
            match sign with
            | `Signed -> Int64.shift_right (Int64.shift_left read (64 - (8 * width))) (64 - (8 * width))
            | `Unsigned -> read
          in
          let narrower =
            List.exists
              (fun narrow ->
                narrow < width
                &&
                match sign with
                | `Signed -> Int64.equal (Int64.shift_right (Int64.shift_left n (64 - (8 * narrow))) (64 - (8 * narrow))) n
                | `Unsigned -> Int64.equal (Int64.shift_right_logical (Int64.shift_left n (64 - (8 * narrow))) (64 - (8 * narrow))) n)
              [ 1; 2; 4 ]
          in
          List.mem width [ 1; 2; 4; 8 ] && Int64.equal back n && not narrower)

let decimals =
  QCheck2.Test.make ~count:3000 ~name:"a decimal default is rounded half away from zero and ordered as its value"
    ~print:(fun ((precision, scale), a, b) -> Printf.sprintf "(%d,%d) %s %s" precision scale a b)
    QCheck2.Gen.(
      let* precision = int_range 1 65 in
      let* scale = int_range 0 (min precision 30) in
      let number =
        let* negative = bool in
        let* whole = string_size ~gen:numeral (int_range 1 (max 1 (precision - scale))) in
        let* fraction = string_size ~gen:numeral (int_range 0 (scale + 2)) in
        let text = whole ^ if fraction = "" then "" else "." ^ fraction in
        return (if negative then "-" ^ text else text)
      in
      triple (return (precision, scale)) number number)
    (fun ((precision, scale), a, b) ->
      let kind = Info.Decimal { precision; scale } in
      let rounded text =
        let q = Q.of_string (String.concat "" (String.split_on_char '.' text)) in
        let digits = match String.index_opt text '.' with Some dot -> String.length text - dot - 1 | None -> 0 in
        let exact = Q.div q (Q.of_bigint (Z.pow (Z.of_int 10) digits)) in
        let scaled = Q.mul exact (Q.of_bigint (Z.pow (Z.of_int 10) scale)) in
        let away = Q.add (Q.abs scaled) (Q.of_ints 1 2) in
        let whole = Z.fdiv (Q.num away) (Q.den away) in
        if Q.sign scaled < 0 then Z.neg whole else whole
      in
      let fits text = Z.lt (Z.abs (rounded text)) (Z.pow (Z.of_int 10) precision) in
      match (Cell.of_default kind a, Cell.of_default kind b) with
      | Some x, Some y -> fits a && fits b && Int.compare (String.compare x y) 0 = Int.compare (Z.compare (rounded a) (rounded b)) 0
      | None, _ -> not (fits a)
      | _, None -> not (fits b))

let tests =
  [
    Alcotest.test_case "defaults written into rows as TiDB 8.5 writes them" `Quick defaults;
    Alcotest.test_case "defaults it does not write, and edges" `Quick cells;
    Alcotest.test_case "defaults written into rows of the first format as TiDB 8.5 writes them" `Quick first_defaults;
    QCheck_alcotest.to_alcotest widths;
    QCheck_alcotest.to_alcotest decimals;
    Alcotest.test_case "padding of an index value whose restored row narrows" `Quick padded;
    QCheck_alcotest.to_alcotest rows;
    QCheck_alcotest.to_alcotest first_format;
    QCheck_alcotest.to_alcotest indexes;
    QCheck_alcotest.to_alcotest keys;
    Alcotest.test_case "refusals" `Quick refusals;
  ]
