open Quiver_core

(** A value of a cell, as the model sees it. *)
type cell =
  | Flag of bool
  | Int of int
  | Long of int64
  | Real of float
  | Text of string
  | Null

(** The logical type a column of the scenario is created with. *)
type kind = Flags | Bytes8 | Shorts | Ints | Longs | Singles | Reals | Texts
[@@deriving enumerate]

(** One step of a batch scenario, run by the real batch and by the model. *)
type step = Add of int * cell | Commit

let ltype : kind -> Ltype.t = function
  | Flags -> Bool
  | Bytes8 -> Int8
  | Shorts -> Uint16
  | Singles -> Float32
  | Ints -> Int32
  | Longs -> Int64
  | Reals -> Float64
  | Texts -> String

let same a b =
  match (a, b) with
  | Real x, Real y ->
      Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
  | Flag x, Flag y -> Bool.equal x y
  | Int x, Int y -> x = y
  | Long x, Long y -> Int64.equal x y
  | Text x, Text y -> String.equal x y
  | Null, Null -> true
  | (Flag _ | Int _ | Long _ | Real _ | Text _ | Null), _ -> false

let add (any : Column.any @ local) cell =
  let (Column.Any column) = any in
  match (Column.repr column, cell) with
  | _, Null -> Ok (Column.add_null column)
  | Bool, Flag b -> Column.add column b
  | Int, Int n -> Column.add column n
  | Int64, Long n -> Column.add column n
  | Float, Real x -> Column.add column x
  | Text, Text s -> Column.add column s
  | (Bool | Int | Int64 | Float | Text), _ -> Error `Mismatch

let read (any : Column.any @ local) row =
  let (Column.Any column) = any in
  match (Column.presence column row, Column.repr column) with
  | Null, _ -> Null
  | Present, Int -> Int (Column.get column row)
  | Present, Int64 -> Long (Column.get column row)
  | Present, Float -> Real (Column.get column row)
  | Present, Text -> Text (Column.get column row)
  | Present, Bool -> Flag (Column.get column row)

let rows_of (batch : Batch.sealed @ local) =
  let columns = Batch.sealed_columns batch in
  let rows = ref [] in
  for row = Batch.sealed_rows batch - 1 downto 0 do
    let cells = ref [] in
    for j = Iarray.length columns - 1 downto 0 do
      cells := read (Iarray.get columns j) row :: !cells
    done;
    rows := !cells :: !rows
  done;
  !rows

let rec add_row (columns : Column.any Iarray.t @ local) j = function
  | [] -> ()
  | value :: rest ->
      ignore (add (Iarray.get columns j) value);
      add_row columns (j + 1) rest

let cell kind =
  QCheck2.Gen.(
    oneof_weighted
      [
        (1, pure Null);
        ( 6,
          match kind with
          | Flags -> map (fun b -> Flag b) bool
          | Bytes8 -> map (fun n -> Int n) (int_range (-128) 127)
          | Shorts -> map (fun n -> Int n) (int_range 0 0xFFFF)
          | Singles -> map (fun bits -> Real (Int32.float_of_bits bits)) int32
          | Ints -> map (fun n -> Int n) (int_range (-1_000_000) 1_000_000)
          | Longs -> map (fun n -> Long n) int64
          | Reals -> map (fun x -> Real x) float
          | Texts ->
              map (fun s -> Text s) (string_size ~gen:char (int_range 0 40)) );
      ])

let kinds =
  QCheck2.Gen.(
    list_size (int_range 1 5)
      (oneof_list all_of_kind))

let table =
  QCheck2.Gen.(
    let* kinds = kinds in
    let+ rows =
      list_size (int_range 0 60) (flatten_list (List.map cell kinds))
    in
    (kinds, rows))

let sealing =
  QCheck2.Test.make ~count:500 ~name:"seal agrees with a model of the scenario"
    QCheck2.Gen.(
      let* kinds = kinds in
      let+ steps =
        list_size (int_range 0 40)
          (oneof_weighted
             [
               (1, pure Commit);
               ( 4,
                 let* j = int_range 0 (List.length kinds - 1) in
                 let+ value = cell (List.nth kinds j) in
                 Add (j, value) );
             ])
      in
      (kinds, steps))
    (fun (kinds, steps) ->
      let batch = Batch.create (List.map ltype kinds) in
      let real =
        let rec apply (batch : Batch.t @ local) = function
          | [] -> ()
          | Add (j, value) :: rest ->
              ignore (add (Iarray.get (Batch.columns batch) j) value);
              apply batch rest
          | Commit :: rest ->
              Batch.commit_row batch;
              apply batch rest
        in
        apply (borrow_ batch) steps;
        match Batch.seal batch with
        | Ok sealed -> Ok (rows_of sealed)
        | Error (`Batch (Batch.Ragged { column; length; rows })) ->
            Error (column, length, rows)
        | Error (`Batch Batch.Malformed) -> Error (-1, -1, -1)
      and model =
        let rows = Base.List.count steps ~f:(( = ) Commit)
        and values =
          List.mapi
            (fun j _ ->
              List.filter_map
                (function
                  | Add (k, value) when k = j -> Some value
                  | Add _ | Commit -> None)
                steps)
            kinds
        in
        match
          List.find_mapi
            (fun j column ->
              match List.length column with
              | length when length = rows -> None
              | length -> Some (j, length))
            values
        with
        | Some (column, length) -> Error (column, length, rows)
        | None ->
            Ok
              (List.init rows (fun row ->
                   List.map (fun column -> List.nth column row) values))
      in
      match (real, model) with
      | Ok a, Ok b -> List.equal (List.equal same) a b
      | Error a, Error b -> a = b
      | Ok _, Error _ | Error _, Ok _ -> false)

let writing =
  QCheck2.Test.make ~count:300
    ~name:"writer ships every row once, in order, through a reused pool"
    ~print:(fun (tables, limit, batches) ->
      Printf.sprintf "%d tables, limit %d, %d batches" (List.length tables)
        limit batches)
    QCheck2.Gen.(
      triple
        (list_size (int_range 1 6) table)
        (int_range 1 4096) (int_range 1 4))
    (fun (tables, limit, batches) ->
      ignore ();
      let pool = Batch_pool.create ~batches in
      let shipped =
        List.map
          (fun (kinds, rows) ->
            let received = ref [] in
            let writer =
              Batch_writer.create ~limit_bytes:limit pool
                (module struct
                  type error = Error.t

                  let ship (batch : Batch.sealed @ unique) pool =
                    received := !received @ rows_of (borrow_ batch);
                    Ok (Batch_pool.give pool (Batch.unseal batch))
                end)
                (List.map ltype kinds)
            in
            let pending = ref rows in
            ignore
              (Batch_writer.fill writer (fun columns ->
                   match !pending with
                   | [] -> Ok Batch_writer.End
                   | row :: rest ->
                       pending := rest;
                       add_row columns 0 row;
                       Ok Row));
            !received)
          tables
      in
      List.for_all2
        (fun (_, rows) received -> List.equal (List.equal same) rows received)
        tables shipped)

let filled kinds rows =
  let batch = Batch.create (List.map ltype kinds) in
  let rec fill (batch : Batch.t @ local) = function
    | [] -> ()
    | row :: rest ->
        add_row (Batch.columns batch) 0 row;
        Batch.commit_row batch;
        fill batch rest
  in
  fill (borrow_ batch) rows;
  batch

let coding =
  QCheck2.Test.make ~count:500
    ~name:"a decoded batch holds the rows encoded, into any reused batch"
    QCheck2.Gen.(pair table table)
    (fun ((kinds, rows), (old_kinds, old_rows)) ->
      let original = filled kinds rows in
      match Batch.seal original with
      | Error _ -> false
      | Ok sealed -> (
          let out = Quiver_core.Byte_buffer.create () in
          Batch.encode sealed out;
          let target = filled old_kinds old_rows in
          Batch.reshape target (List.map ltype kinds);
          match Batch.decode target out.bs ~stop:out.len with
          | Ok decoded ->
              List.length rows = Batch.sealed_rows sealed
              && List.equal (List.equal same) (rows_of sealed) (rows_of decoded)
          | Error _ -> false))

let damaged =
  QCheck2.Test.make ~count:1000
    ~name:"cut or damaged encodings decode to an error or to rows, never raise"
    QCheck2.Gen.(
      triple table (int_range 0 1000)
        (list_small (pair (int_range 0 1000) char)))
    (fun ((kinds, rows), cut, flips) ->
      match Batch.seal (filled kinds rows) with
      | Error _ -> false
      | Ok sealed -> (
          let out = Quiver_core.Byte_buffer.create () in
          Batch.encode sealed out;
          let bytes = Bytes.sub out.bs 0 out.len in
          List.iter
            (fun (at, c) ->
              if Bytes.length bytes > 0 then
                Bytes.set bytes (at mod Bytes.length bytes) c)
            flips;
          let stop = Int.min cut (Bytes.length bytes) in
          let target = Batch.create (List.map ltype kinds) in
          match Batch.decode target bytes ~stop with
          | Ok decoded -> List.length (rows_of decoded) >= 0
          | Error (`Batch _) -> true))

let tests =
  List.map QCheck_alcotest.to_alcotest [ sealing; writing; coding; damaged ]
