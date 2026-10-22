open Base.Result.Let_syntax
open Quiver_core
open Sexplib0.Sexp_conv

(** A cell as the model sees it. *)
type value =
  | Null
  | Int of int
  | Text of string
  | Binary of string
  | Real of float
[@@deriving sexp_of]

(** The logical type of a generated column. *)
type kind = Ints | Texts | Binaries | Reals | Years [@@deriving enumerate, sexp_of]

let ltype : kind -> Ltype.t = function
  | Ints -> Int32
  | Texts -> String
  | Binaries -> Bytes
  | Reals -> Float64
  | Years -> Year

let same a b =
  match (a, b) with
  | Real x, Real y ->
      Int64.equal (Int64.bits_of_float x) (Int64.bits_of_float y)
  | Null, Null -> true
  | Int x, Int y -> x = y
  | Text x, Text y | Binary x, Binary y -> String.equal x y
  | (Null | Int _ | Text _ | Binary _ | Real _), _ -> false

let ident name = Result.get_ok (Ident.of_string name)

let value kind =
  QCheck2.Gen.(
    let text =
      string_size
        ~gen:
          (oneof
             [
               oneof_list
                 [ '\''; '"'; ','; '\n'; '\r'; '\\'; '\000'; '\026'; '\t'; 'N' ];
               char;
             ])
        (int_range 0 12)
    in
    oneof_weighted
      [
        (1, pure Null);
        ( 5,
          match kind with
          | Ints -> map (fun n -> Int n) (int_range (-2147483648) 2147483647)
          | Years ->
              map
                (fun n -> Int n)
                (oneof [ pure 0; int_range 1901 2155; int_range 0 9999 ])
          | Texts -> map (fun s -> Text s) text
          | Binaries -> map (fun s -> Binary s) text
          | Reals ->
              map
                (fun x -> Real x)
                (map (fun x -> if Float.is_finite x then x else 0.5) float) );
      ])

let table =
  QCheck2.Gen.(
    let* kinds =
      list_size (int_range 1 5)
        (oneof_list all_of_kind)
    in
    let+ rows =
      list_size (int_range 1 30) (flatten_list (List.map value kinds))
    in
    (kinds, rows))

let print scenario =
  Sexplib0.Sexp.to_string_hum ([%sexp_of: kind list * value list list] scenario)

let write (type (a : value mod contended portable))
    (module S : Sink_intf.S with type t = a)
    (make : Quiver_sink.Layout.t -> a) extension (kinds, rows) =
  let dir = Filename.temp_dir "quiver-sink" "" in
  let layout =
    Result.get_ok
      (Quiver_sink.Layout.create ~dir:(File_path.of_string dir) Plain)
  in
  let sink = make layout in
  let table =
    Result.get_ok
      (Schema.Table.make
         ~name:{ database = ident "db"; table = ident "t" }
         ~fields:
           (List.mapi
              (fun j kind ->
                {
                  Schema.Field.name = ident (Printf.sprintf "c%d" j);
                  ltype = ltype kind;
                  nullability = Nullable;
                })
              kinds)
         ~key:No_key ~estimate:{ rows = 0; bytes = 0 })
  in
  let batch = Batch.create (List.map ltype kinds) in
  let rec cells (columns : Column.any Iarray.t @ local) j = function
    | [] -> ()
    | cell :: rest ->
        (let (Column.Any column) = Iarray.get columns j in
         match (Column.repr column, cell) with
         | _, Null -> Column.add_null column
         | Int, Int n -> Support.get_ok (Column.add column n)
         | Text, (Text s | Binary s) -> Support.get_ok (Column.add column s)
         | Float, Real x -> Support.get_ok (Column.add column x)
         | _ -> Alcotest.fail "cell does not fit its column");
        cells columns (j + 1) rest
  in
  let rec fill (batch : Batch.t @ local) = function
    | [] -> ()
    | row :: rest ->
        cells (Batch.columns batch) 0 row;
        Batch.commit_row batch;
        fill batch rest
  in
  fill (borrow_ batch) rows;
  match
    let%bind sealed = Batch.seal batch in
    let%bind part = S.part sink table ~index:0 in
    let%bind () =
      match S.render (S.renderer part) sealed with
      | Ok rendered -> S.append part rendered
      | Error error -> Error error
    in
    let closed = Await.Await.Ivar.create () in
    S.close part (fun outcome ->
        Await.Await.Ivar.fill_if_empty closed
          (match outcome with Ok _ -> Ok () | Error error -> Error error));
    match Await.Await.Ivar.read Blocking.await closed with
    | Ok () -> Ok ()
    | Error error -> Error (`Sink error)
  with
  | Error _ -> Alcotest.fail "the sink failed"
  | Ok _ ->
      Stdio.In_channel.read_all
        (Filename.concat dir ("db.t.00000." ^ extension))

let unhex text =
  String.init
    (String.length text / 2)
    (fun i -> Char.chr (int_of_string ("0x" ^ String.sub text (2 * i) 2)))

let typed kinds fields =
  List.map2
    (fun kind (field, quoted) ->
      match (kind, quoted, field) with
      | _, `Bare, "\\N" -> Null
      | Ints, `Bare, n -> Int (int_of_string n)
      | Years, `Bare, n when String.length n = 4 -> Int (int_of_string n)
      | Reals, `Bare, x -> Real (float_of_string x)
      | Binaries, `Bare, hex -> Binary (unhex hex)
      | Texts, `Quoted, s -> Text s
      | _ -> Text ("unexpected " ^ field))
    kinds fields

(** RFC 4180 with a header, CRLF rows and bare [\N] for NULL. *)
let rfc4180 kinds text =
  let length = String.length text in
  let rec field i acc =
    if i < length && text.[i] = '"' then
      let buffer = Buffer.create 16 in
      let rec quoted j =
        if text.[j] = '"' && j + 1 < length && text.[j + 1] = '"' then (
          Buffer.add_char buffer '"';
          quoted (j + 2))
        else if text.[j] = '"' then j + 1
        else (
          Buffer.add_char buffer text.[j];
          quoted (j + 1))
      in
      let next = quoted (i + 1) in
      separator next ((Buffer.contents buffer, `Quoted) :: acc)
    else
      let stop = ref i in
      while !stop < length && text.[!stop] <> ',' && text.[!stop] <> '\r' do
        incr stop
      done;
      separator !stop ((String.sub text i (!stop - i), `Bare) :: acc)
  and separator i acc =
    if text.[i] = ',' then field (i + 1) acc else (List.rev acc, i + 2)
  in
  let rec rows i acc =
    if i >= length then List.rev acc
    else
      let fields, next = field i [] in
      rows next (fields :: acc)
  in
  match rows 0 [] with
  | _header :: rows -> List.map (typed kinds) rows
  | [] -> []

(** MySQL LOAD DATA with ENCLOSED BY '"' ESCAPED BY '\\', LF rows. *)
let load_data kinds text =
  let length = String.length text in
  let unescape = function
    | '0' -> '\000'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | 'Z' -> '\026'
    | c -> c
  in
  let rec field i acc =
    if text.[i] = '"' then
      let buffer = Buffer.create 16 in
      let rec quoted j =
        match text.[j] with
        | '\\' ->
            Buffer.add_char buffer (unescape text.[j + 1]);
            quoted (j + 2)
        | '"' -> j + 1
        | c ->
            Buffer.add_char buffer c;
            quoted (j + 1)
      in
      let next = quoted (i + 1) in
      separator next ((Buffer.contents buffer, `Quoted) :: acc)
    else
      let stop = ref i in
      while !stop < length && text.[!stop] <> ',' && text.[!stop] <> '\n' do
        incr stop
      done;
      separator !stop ((String.sub text i (!stop - i), `Bare) :: acc)
  and separator i acc =
    if text.[i] = ',' then field (i + 1) acc else (List.rev acc, i + 1)
  in
  let rec rows i acc =
    if i >= length then List.rev acc
    else
      let fields, next = field i [] in
      rows next (typed kinds fields :: acc)
  in
  rows 0 []

(** The VALUES of mydumper-style INSERT statements, with MySQL's string escapes,
    X'hex' and NULL. *)
let inserts kinds text =
  let unescape = function
    | '0' -> '\000'
    | 'b' -> '\b'
    | 'n' -> '\n'
    | 'r' -> '\r'
    | 't' -> '\t'
    | 'Z' -> '\026'
    | c -> c
  in
  let rec skip_to_values i =
    Option.map
      (fun at -> at + String.length ") VALUES\n")
      (Base.String.substr_index text ~pos:i ~pattern:") VALUES\n")
  and row i acc =
    let rec cells j kinds parsed =
      match kinds with
      | [] -> (List.rev parsed, j)
      | kind :: rest ->
          let j = if text.[j] = ',' then j + 1 else j in
          let cell, next =
            match text.[j] with
            | '\'' ->
                let buffer = Buffer.create 16 in
                let rec quoted k =
                  match text.[k] with
                  | '\\' ->
                      Buffer.add_char buffer (unescape text.[k + 1]);
                      quoted (k + 2)
                  | '\'' -> k + 1
                  | c ->
                      Buffer.add_char buffer c;
                      quoted (k + 1)
                in
                let next = quoted (j + 1) in
                (Text (Buffer.contents buffer), next)
            | 'X' ->
                let close = String.index_from text (j + 2) '\'' in
                ( Binary (unhex (String.sub text (j + 2) (close - j - 2))),
                  close + 1 )
            | _ ->
                let stop = ref j in
                while text.[!stop] <> ',' && text.[!stop] <> ')' do
                  incr stop
                done;
                let raw = String.sub text j (!stop - j) in
                ( (match (kind, raw) with
                  | _, "NULL" -> Null
                  | Ints, n -> Int (int_of_string n)
                  | Years, n when String.length n = 4 -> Int (int_of_string n)
                  | Reals, x -> Real (float_of_string x)
                  | (Texts | Binaries | Years), other ->
                      Text ("unexpected " ^ other)),
                  !stop )
          in
          cells next rest (cell :: parsed)
    in
    let values, next = cells (i + 1) kinds [] in
    let acc = values :: acc in
    match String.sub text (next + 1) 2 with
    | ",\n" -> row (next + 3) acc
    | _ -> (
        match skip_to_values (next + 1) with
        | Some start -> row start acc
        | None -> List.rev acc)
  in
  match skip_to_values 0 with Some start -> row start [] | None -> []

let property name parse write =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~count:300 ~name ~print table
       (fun ((kinds, rows) as input) ->
         let parsed = parse kinds (write input) in
         List.equal (List.equal same) rows parsed
         || QCheck2.Test.fail_reportf "parsed back: %s" (print (kinds, parsed))))

let tests =
  [
    property "CSV RFC 4180 parses back to the values" rfc4180
      (write
         (module Quiver_sink.Csv)
         (fun layout -> Quiver_sink.Csv.create layout Rfc4180)
         "csv");
    property "CSV for LOAD DATA parses back to the values" load_data
      (write
         (module Quiver_sink.Csv)
         (fun layout -> Quiver_sink.Csv.create layout Mysql_load_data)
         "csv");
    property "SQL INSERTs parse back to the values" inserts
      (write
         (module Quiver_sink.Sql)
         (fun layout -> Quiver_sink.Sql.create layout ~statement_bytes:200)
         "sql");
  ]

let json_table =
  Result.get_ok
    (Schema.Table.make
       ~name:{ database = ident "db"; table = ident "t" }
       ~fields:
         [
           { Schema.Field.name = ident "id"; ltype = Int32; nullability = Not_null };
           { Schema.Field.name = ident "j"; ltype = Json; nullability = Nullable };
         ]
       ~key:No_key ~estimate:{ rows = 0; bytes = 0 })

let json_rows = [ (1, Some "[0.5]"); (2, Some "[1, 0.49178151800806813]"); (3, None); (4, Some {|{"a": 1e25, "b": 9.999999999999999e24}|}) ]

let with_json_batch f =
  let batch = Batch.create [ Int32; Json ] in
  let rec fill (batch : Batch.t @ local) = function
    | [] -> ()
    | ((id : int), (text : string option)) :: rest ->
        let columns = Batch.columns batch in
        let () =
          let (Column.Any ids) = Iarray.get columns 0 in
          match Column.repr ids with Int -> (Support.get_ok (Column.add ids id) : unit) | _ -> Alcotest.fail "id"
        in
        let () =
          let (Column.Any texts) = Iarray.get columns 1 in
          match (Column.repr texts, text) with
          | _, None -> Column.add_null texts
          | Text, Some text -> Support.get_ok (Column.add texts text)
          | _ -> Alcotest.fail "json"
        in
        Batch.commit_row batch;
        fill batch rest
  in
  fill (borrow_ batch) json_rows;
  match Batch.seal batch with Ok sealed -> f sealed | Error _ -> Alcotest.fail "seal"

let tests =
  tests
  @ [
      Alcotest.test_case "a JSON number the server would misread is set apart in SQL" `Quick (fun () ->
          with_json_batch (fun sealed ->
              let out = Byte_buffer.create () in
              let () =
                match Quiver_sink.Sql.values (Quiver_sink.Sql.Insert.lines 1_000_000 json_table) sealed out ~rows:[ 0; 1; 2; 3 ] with
                | Ok () -> ()
                | Error _ -> Alcotest.fail "values"
              in
              Alcotest.(check string)
                "insert"
                "INSERT INTO `t` (`id`,`j`) VALUES\n\
                 (1,'[0.5]'),\n\
                 (2,JSON_SET('[1, 0.49178151800806813]','$[1]',0.49178151800806813e0)),\n\
                 (3,NULL),\n\
                 (4,JSON_SET('{\\\"a\\\": 1e25, \\\"b\\\": 9.999999999999999e24}','$.\\\"b\\\"',9.999999999999999e24));\n"
                (Byte_buffer.contents out)));
      Alcotest.test_case "CSV refuses such a row, or leaves it out for an INSERT" `Quick (fun () ->
          with_json_batch (fun sealed ->
              let lines = Quiver_sink.Csv.Dialect.lines Mysql_load_data json_table in
              let out = Byte_buffer.create () in
              let () =
                match Quiver_sink.Csv.Dialect.write lines sealed out with
                | Error (`Misread_json (_, column)) -> Alcotest.(check string) "column" "j" (column :> string)
                | Error _ | Ok () -> Alcotest.fail "written"
              in
              let out = Byte_buffer.create () in
              match Quiver_sink.Csv.write_apart Rapidjson lines sealed out with
              | Ok apart ->
                  Alcotest.(check (list int)) "left out" [ 1; 3 ] apart;
                  Alcotest.(check string) "kept" "1,\"[0.5]\"\n3,\\N\n" (Byte_buffer.contents out);
                  let out = Byte_buffer.create () in
                  let () =
                    match Quiver_sink.Csv.write_apart Verbatim lines sealed out with
                    | Ok apart -> Alcotest.(check (list int)) "a text column keeps every row" [] apart
                    | Error _ -> Alcotest.fail "write_apart"
                  in
                  Alcotest.(check int) "rows written" 4 (List.length (String.split_on_char '\n' (String.trim (Byte_buffer.contents out))))
              | Error _ -> Alcotest.fail "write_apart"));
    ]
