open Sexplib0.Sexp_conv

let ok = function
  | Ok value -> value
  | Error (`Cfg error) -> Alcotest.failf "cfg: %s" (Sexplib0.Sexp.to_string (Quiver_innodb.Cfg.sexp_of_error error))
  | Error (`Layout error) ->
      Alcotest.failf "layout: %s" (Sexplib0.Sexp.to_string (Quiver_innodb.Layout.sexp_of_error error))
  | Error (`Tablespace error) ->
      Alcotest.failf "tablespace: %s" (Sexplib0.Sexp.to_string (Quiver_innodb.Tablespace.sexp_of_error error))
  | Error (`File error) -> Alcotest.failf "file: %s" (Sexplib0.Sexp.to_string (Quiver_innodb.File.sexp_of_error error))
  | Error (`Column error) ->
      Alcotest.failf "column: %s" (Sexplib0.Sexp.to_string (Quiver_core.Column.sexp_of_error error))

let fixture name = In_channel.with_open_bin (Filename.concat "innodb" name) In_channel.input_all

let collations =
  List.map (fun id -> (id, Quiver_innodb.Layout.Utf8mb4)) ([ 45; 46 ] @ List.init 24 (fun k -> 224 + k) @ List.init 17 (fun k -> 255 + k)
  @ [ 273; 274; 275 ] @ List.init 18 (fun k -> 277 + k) @ [ 296; 297; 298; 300 ] @ List.init 21 (fun k -> 303 + k))
  @ List.map (fun id -> (id, Quiver_innodb.Layout.Utf8mb3)) ([ 33; 76; 83; 223 ] @ List.init 24 (fun k -> 192 + k))
  @ List.map (fun id -> (id, Quiver_innodb.Layout.Latin1)) [ 5; 8; 15; 31; 47; 48; 49; 94 ]


let tests =
  [
    Alcotest.test_case "CRC-32C of the standard check string" `Quick (fun () ->
        Alcotest.(check int)
          "123456789" 0xE3069283
          (Quiver_innodb.Crc32c.digest (Bytes.of_string "123456789") ~pos:0 ~len:9));
    Alcotest.test_case "every page the server wrote carries its CRC-32C" `Quick (fun () ->
        let file = Bytes.of_string (fixture "narrow.ibd") in
        let page = 16384 in
        List.iter
          (fun n ->
            let at = n * page in
            let sum =
              Quiver_innodb.Crc32c.digest file ~pos:(at + 4) ~len:22
              lxor Quiver_innodb.Crc32c.digest file ~pos:(at + 38) ~len:(page - 46)
            in
            Alcotest.(check int)
              (Printf.sprintf "page %d" n)
              (Int32.to_int (Bytes.get_int32_be file at) land 0xFFFF_FFFF)
              sum)
          [ 0; 1; 2; 3; 4 ]);
    Alcotest.test_case "the server's .cfg files parse" `Quick (fun () ->
        List.iter
          (fun name ->
            match Quiver_innodb.Cfg.parse (fixture name) with
            | Ok cfg ->
                Alcotest.(check (list string))
                  (name ^ " clustered fields")
                  (match name with
                  | "narrow.cfg" -> [ "id"; "DB_TRX_ID"; "DB_ROLL_PTR"; "v" ]
                  | _ -> [ "id"; "DB_TRX_ID"; "DB_ROLL_PTR"; "v"; "s"; "b" ])
                  (List.map (fun (field : Quiver_innodb.Cfg.Index.field) -> field.column) cfg.clustered.fields);
                Alcotest.(check int) (name ^ " root") 4 cfg.clustered.root;
                Alcotest.(check int)
                  (name ^ " nullable")
                  (match name with "narrow.cfg" -> 0 | _ -> 3)
                  cfg.clustered.n_nullable
            | Error (`Cfg error) ->
                Alcotest.failf "%s: %s" name
                  (Sexplib0.Sexp.to_string (Quiver_innodb.Cfg.sexp_of_error error)))
          [ "narrow.cfg"; "wide.cfg" ]);
  ]

let fill : type a.
    a Quiver_core.Column.t -> a Quiver_core.Column.repr -> [ `Null | `Int of int | `Text of string ] -> unit =
 fun column repr value ->
  let added : (unit, [ `Column of Quiver_core.Column.error ]) result =
    match (repr, value) with
    | _, `Null ->
        Quiver_core.Column.add_null column;
        Ok ()
    | Int64, `Int v -> Quiver_core.Column.add column (Int64.of_int v)
    | Int, `Int v -> Quiver_core.Column.add column v
    | Text, `Text v -> Quiver_core.Column.add column v
    | _ -> Alcotest.fail "value of another type"
  in
  ok added

let column ltype values =
  let (Quiver_core.Column.Any column) = Quiver_core.Column.create ltype in
  List.iter (fill column (Quiver_core.Column.repr column)) values;
  Quiver_core.Column.Any column

(** A table built through a file, read back whole: runs filled in a shuffled
    order, handed to [finish] in that order. *)
let built ~cfg ~template ~dumped rows ~cuts ~order ~batch =
  let cfg = ok (Quiver_innodb.Cfg.parse (fixture cfg)) in
  let layout = ok (Quiver_innodb.Layout.of_cfg cfg ~collations dumped) in
  let path = Filename.temp_file "quiver" ".ibd" in
  let file = ok (Quiver_innodb.File.create path) in
  let scratch = ok (Quiver_innodb.File.create (path ^ ".sort")) in
  let space = ok (Quiver_innodb.Tablespace.create ~template:(fixture template) layout ~secondary:[] file ~scratch ~threads:4) in
  let rows = Array.of_list rows in
  let bounds = List.sort_uniq compare ((0 :: cuts) @ [ Array.length rows ]) in
  let slices =
    List.map (fun (a, b) -> Array.sub rows a (b - a)) (List.combine (Base.List.drop_last_exn bounds) (List.tl bounds))
  in
  let indexed = List.mapi (fun r slice -> (r, slice)) slices in
  let rank r = List.nth order (r mod List.length order) in
  let fill_order = List.sort (fun (a, _) (b, _) -> compare (rank a, a) (rank b, b)) indexed in
  let finished =
    List.map
      (fun (_, slice) ->
        let run = Quiver_innodb.Tablespace.run space ~rank:0 in
        let rec go at =
          if at < Array.length slice then (
            let chunk = Array.sub slice at (Int.min batch (Array.length slice - at)) in
            let columns =
              Iarray.of_list
                (List.mapi
                   (fun c (_, ltype) -> column ltype (Array.to_list (Array.map (fun row -> List.nth row c) chunk)))
                   dumped)
            in
            let records = ok (Quiver_innodb.Tablespace.encode space columns ~rows:(Array.length chunk)) in
            ok (Quiver_innodb.Tablespace.add run records);
            go (at + batch))
        in
        go 0;
        ok (Quiver_innodb.Tablespace.close run))
      fill_order
  in
  let (_ : int) = ok (Quiver_innodb.Tablespace.finish space finished) in
  Quiver_innodb.File.close file;
  Quiver_innodb.File.close scratch;
  let bytes = Bytes.of_string (In_channel.with_open_bin path In_channel.input_all) in
  Sys.remove path;
  Sys.remove (path ^ ".sort");
  (layout, bytes)

let read file =
  let text = Bytes.to_string file in
  fun n -> Bytes.of_string (String.sub text (n * Quiver_innodb.Page.size) Quiver_innodb.Page.size)

let checked layout file =
  Quiver_innodb.Check.tablespace layout ~secondary:[] ~pages:(Bytes.length file / Quiver_innodb.Page.size) ~read:(read file)

(** The leaf records read back on their own: down the leftmost pointers, along
    the leaves, each field's bytes cut by the layout, [None] when NULL. *)
let stored (layout : Quiver_innodb.Layout.t) file =
  let size = Quiver_innodb.Page.size in
  let u16 at = Bytes.get_uint16_be file at in
  let u32 at = Int32.to_int (Bytes.get_int32_be file at) land 0xFFFF_FFFF in
  let first page = (page * size) + ((99 + u16 ((page * size) + 97)) land 0xFFFF) in
  let rec leftmost page =
    if u16 ((page * size) + 64) = 0 then page
    else
      let origin = first page in
      let data, _ =
        List.fold_left
          (fun (data, at) (field : Quiver_innodb.Layout.field) ->
            let null =
              match field.nullability with
              | Not_null -> false
              | Null_bit bit -> Bytes.get_uint8 file (origin - 6 - (bit / 8)) land (1 lsl (bit mod 8)) <> 0
            in
            match Quiver_innodb.Layout.length field.storage with
            | _ when null -> (data, at)
            | Fixed n -> (data + n, at)
            | Varying { big } ->
                let b = Bytes.get_uint8 file at in
                if big && b land 0x80 <> 0 then (data + (((b land 0x3F) lsl 8) lor Bytes.get_uint8 file (at - 1)), at - 2)
                else (data + b, at - 1))
          (0, origin - 6 - layout.null_bytes)
          (Base.List.take layout.fields layout.key)
      in
      leftmost (u32 (origin + data))
  in
  let lob at =
    let first = u32 (at + 4) in
    let rec pieces (page, offset) acc =
      if page = Quiver_innodb.Page.null then String.concat "" (List.rev acc)
      else
        let node = (page * size) + offset in
        let piece = u32 (node + 48) and length = u16 (node + 52) in
        let start = if piece = first then (first * size) + 696 else (piece * size) + 49 in
        pieces (u32 (node + 6), u16 (node + 10)) (Bytes.sub_string file start length :: acc)
    in
    let value = pieces (u32 ((first * size) + 68), u16 ((first * size) + 72)) [] in
    if String.length value = u32 (at + 16) then value else Alcotest.fail "a stored column's length"
  in
  let decode origin =
    let lengths = ref (origin - 6 - layout.null_bytes) and at = ref origin in
    List.map
      (fun (field : Quiver_innodb.Layout.field) ->
        let null =
          match field.nullability with
          | Not_null -> false
          | Null_bit bit -> Bytes.get_uint8 file (origin - 6 - (bit / 8)) land (1 lsl (bit mod 8)) <> 0
        in
        let outside = ref false in
        let length =
          match (null, Quiver_innodb.Layout.length field.storage) with
          | true, _ -> 0
          | false, Fixed n -> n
          | false, Varying { big } ->
              let b = Bytes.get_uint8 file !lengths in
              if big && b land 0x80 <> 0 then (
                let l = ((b land 0x3F) lsl 8) lor Bytes.get_uint8 file (!lengths - 1) in
                lengths := !lengths - 2;
                outside := b land 0x40 <> 0;
                l)
              else (
                lengths := !lengths - 1;
                b)
        in
        let raw = if !outside then lob (!at + length - 20) else Bytes.sub_string file !at length in
        at := !at + length;
        (field, if null then None else Some raw))
      layout.fields
  in
  let rec leaves page acc =
    let rec records origin acc =
      let next = (origin + u16 ((page * size) + origin - 2)) land 0xFFFF in
      if next = 112 then acc else records next (decode ((page * size) + next) :: acc)
    in
    let acc = records 99 acc in
    let next = u32 ((page * size) + 12) in
    if next = Quiver_innodb.Page.null then List.rev acc else leaves next acc
  in
  leaves (leftmost layout.root) []

(** The records' values, system fields left out: integers as numbers, the rest
    as their bytes. *)
let decoded layout file =
  List.map
    (List.filter_map (fun ((field : Quiver_innodb.Layout.field), raw) ->
         match (field.storage, raw) with
         | (Trx_id | Roll_ptr), _ -> None
         | _, None -> Some `Null
         | Integer { bytes = 8; sign = Signed; _ }, Some raw ->
             let value = Int64.logxor (String.get_int64_be raw 0) Int64.min_int in
             Some (if Int64.equal (Int64.of_int (Int64.to_int value)) value then `Int (Int64.to_int value) else `Null)
         | Integer { bytes; sign; _ }, Some raw ->
             let unsigned = String.fold_left (fun acc c -> (acc lsl 8) lor Char.code c) 0 raw in
             let value = match sign with Signed -> unsigned - (1 lsl ((8 * bytes) - 1)) | Unsigned -> unsigned in
             Some (`Int value)
         | _, Some raw -> Some (`Text raw)))
    (stored layout file)

type shape = Narrow | Wide [@@deriving sexp_of]

(** Rows of one of the two server-made tables, a split of them into runs, the
    order the runs fill in, and the batch size. *)
type scenario = { shape : shape; rows : int; seed : int; cuts : int list; order : int list; batch : int }
[@@deriving sexp_of]

let scenario =
  QCheck2.Gen.(
    let* rows = int_range 0 3000 in
    let+ shape = oneof_list [ Narrow; Wide ]
    and+ seed = int
    and+ cuts = list_size (int_range 0 4) (int_range 0 rows)
    and+ order = list_size (return 5) (int_range 0 100)
    and+ batch = int_range 1 700 in
    { shape; rows; seed; cuts; order; batch })

let rows_of { shape; rows; seed; _ } =
  let random = Random.State.make [| seed |] in
  let start = Random.State.full_int random (1 lsl 40) - (1 lsl 39) in
  let keys =
    List.rev
      (snd
         (List.fold_left
            (fun (key, acc) _ -> (key + 1 + Random.State.int random 1000, key :: acc))
            (start, []) (List.init rows Fun.id)))
  in
  let int32 () = `Int (Random.State.full_int random (1 lsl 32) - (1 lsl 31)) in
  let maybe f = if Random.State.int random 5 = 0 then `Null else f () in
  let alphabet = [| "a"; " "; "é"; "ж"; "😀"; "\000"; "'" |] in
  List.map
    (fun key ->
      match shape with
      | Narrow -> [ `Int key; int32 () ]
      | Wide ->
          [
            `Int key;
            maybe int32;
            maybe (fun () ->
                if Random.State.bool random then
                  `Text (String.make [| 127; 128; 129; 255; 256; 400 |].(Random.State.int random 6) 'x')
                else
                  let n = [| 0; 1; 31; 32; 33; 64; 100 |].(Random.State.int random 7) in
                  `Text (String.concat "" (List.init n (fun _ -> alphabet.(Random.State.int random 7)))));
            maybe (fun () -> `Text (String.init (Random.State.int random 21) (fun _ -> Char.chr (Random.State.int random 256))));
          ])
    keys

let build_scenario scenario =
  let cfg, template, dumped =
    match scenario.shape with
    | Narrow -> ("narrow.cfg", "narrow.ibd", [ ("id", Quiver_core.Ltype.Int64); ("v", Int32) ])
    | Wide -> ("wide.cfg", "wide.ibd", [ ("id", Quiver_core.Ltype.Int64); ("v", Int32); ("s", String); ("b", Bytes) ])
  in
  let rows = rows_of scenario in
  let layout, file = built ~cfg ~template ~dumped rows ~cuts:scenario.cuts ~order:scenario.order ~batch:scenario.batch in
  (rows, layout, file)

let check_error = function
  | Ok (_ : Quiver_innodb.Check.t) -> "passed"
  | Error (`Check error) -> Sexplib0.Sexp.to_string (Quiver_innodb.Check.sexp_of_error error)

let reseal file n =
  let at = n * Quiver_innodb.Page.size in
  let page = Bytes.sub file at Quiver_innodb.Page.size in
  let sum = Int32.of_int (Quiver_innodb.Page.checksum page ~pos:0) in
  Bytes.set_int32_be file at sum;
  Bytes.set_int32_be file (at + Quiver_innodb.Page.size - 8) sum

let tests =
  tests
  @ [
      Alcotest.test_case "the server's empty tablespaces pass the check" `Quick (fun () ->
          List.iter
            (fun (cfg, template, dumped) ->
              let layout = ok (Quiver_innodb.Layout.of_cfg (ok (Quiver_innodb.Cfg.parse (fixture cfg))) ~collations dumped) in
              let file = Bytes.of_string (fixture template) in
              match checked layout file with
              | Ok checked -> Alcotest.(check int) template 0 checked.rows
              | Error _ as failed -> Alcotest.failf "%s: %s" template (check_error failed))
            [
              ("narrow.cfg", "narrow.ibd", [ ("id", Quiver_core.Ltype.Int64); ("v", Int32) ]);
              ("wide.cfg", "wide.ibd", [ ("id", Quiver_core.Ltype.Int64); ("v", Int32); ("s", String); ("b", Bytes) ]);
            ]);
      Alcotest.test_case "a broken file fails the check" `Quick (fun () ->
          let _, layout, file = build_scenario { shape = Narrow; rows = 20_000; seed = 1; cuts = [ 7_000 ]; order = [ 1; 0 ]; batch = 500 } in
          let size = Quiver_innodb.Page.size in
          let u16 at = Bytes.get_uint16_be file at and u32 at = Int32.to_int (Bytes.get_int32_be file at) land 0xFFFF_FFFF in
          let root = layout.root in
          let first page = (99 + u16 ((page * size) + 97)) land 0xFFFF in
          let leaf = u32 ((root * size) + first root + 8) in
          let second = u32 ((leaf * size) + 12) in
          let after page origin = (origin + u16 ((page * size) + origin - 2)) land 0xFFFF in
          let mutations =
            [
              ("checksum", fun file -> Bytes.set_uint8 file ((leaf * size) + 200) (Bytes.get_uint8 file ((leaf * size) + 200) lxor 1); []);
              ( "keys swapped",
                fun file ->
                  let a = (leaf * size) + first leaf and b = (leaf * size) + after leaf (first leaf) in
                  let x = Bytes.sub file a 8 in
                  Bytes.blit file b file a 8;
                  Bytes.blit x 0 file b 8;
                  [ leaf ] );
              ( "n_owned",
                fun file ->
                  let o = (leaf * size) + after leaf (after leaf (after leaf (first leaf))) in
                  Bytes.set_uint8 file (o - 5) (Bytes.get_uint8 file (o - 5) land 0xF0 lor 5);
                  [ leaf ] );
              ( "MIN_REC",
                fun file ->
                  let o = (root * size) + first root in
                  Bytes.set_uint8 file (o - 5) (Bytes.get_uint8 file (o - 5) land 0x0F);
                  [ root ] );
              ( "XDES bit",
                fun file ->
                  let k = second / 64 and i = second mod 64 in
                  let at = 150 + (40 * k) + 24 + (2 * i / 8) in
                  Bytes.set_uint8 file at (Bytes.get_uint8 file at lor (1 lsl (2 * i mod 8)));
                  [ 0 ] );
              ( "N_RECS",
                fun file ->
                  Bytes.set_uint16_be file ((leaf * size) + 54) (u16 ((leaf * size) + 54) + 1);
                  [ leaf ] );
              ( "prev link",
                fun file ->
                  Bytes.set_int32_be file ((second * size) + 8) (Int32.of_int root);
                  [ second ] );
              ( "child pointer",
                fun file ->
                  Bytes.set_int32_be file
                    ((root * size) + first root + 8)
                    (Int32.of_int second);
                  [ root ] );
              ( "FRAG_N_USED",
                fun file ->
                  Bytes.set_int32_be file 58 (Int32.of_int (u32 58 + 1));
                  [ 0 ] );
              ( "NOT_FULL_N_USED",
                fun file ->
                  let inode = u16 ((root * size) + 74 + 8) in
                  Bytes.set_int32_be file ((2 * size) + inode + 8) (Int32.of_int (u32 ((2 * size) + inode + 8) + 1));
                  [ 2 ] );
            ]
          in
          Alcotest.(check string) "unbroken" "passed" (check_error (checked layout file));
          List.iter
            (fun (name, mutate) ->
              let copy = Bytes.copy file in
              List.iter (reseal copy) (mutate copy);
              if check_error (checked layout copy) = "passed" then Alcotest.failf "%s passed the check" name)
            mutations);
    ]
  @ [
      Alcotest.test_case "runs spanning several full extents and three levels read back" `Quick (fun () ->
          let scenario =
            { shape = Narrow; rows = 500_000; seed = 3; cuts = [ 40_000; 40_001; 370_000 ]; order = [ 2; 0; 3; 1 ]; batch = 4096 }
          in
          let rows, layout, file = build_scenario scenario in
          match checked layout file with
          | Error _ as failed -> Alcotest.fail (check_error failed)
          | Ok checked ->
              Alcotest.(check int) "rows" 500_000 checked.rows;
              Alcotest.(check bool) "read back" true (decoded layout file = rows));
      Alcotest.test_case "keys out of order are refused" `Quick (fun () ->
          let cfg = ok (Quiver_innodb.Cfg.parse (fixture "narrow.cfg")) in
          let dumped = [ ("id", Quiver_core.Ltype.Int64); ("v", Int32) ] in
          let layout = ok (Quiver_innodb.Layout.of_cfg cfg ~collations dumped) in
          let path = Filename.temp_file "quiver" ".ibd" in
          let file = ok (Quiver_innodb.File.create path) in
          let scratch = ok (Quiver_innodb.File.create (path ^ ".sort")) in
          let space = ok (Quiver_innodb.Tablespace.create ~template:(fixture "narrow.ibd") layout ~secondary:[] file ~scratch ~threads:4) in
          let run = Quiver_innodb.Tablespace.run space ~rank:0 in
          let columns = Iarray.of_list [ column Int64 [ `Int 5; `Int 3 ]; column Int32 [ `Int 0; `Int 0 ] ] in
          let records = ok (Quiver_innodb.Tablespace.encode space columns ~rows:2) in
          let added = Quiver_innodb.Tablespace.add run records in
          Quiver_innodb.File.close file;
          Quiver_innodb.File.close scratch;
          Sys.remove path;
  Sys.remove (path ^ ".sort");
          match added with
          | Error (`Tablespace Quiver_innodb.Tablespace.Out_of_order) -> ()
          | Ok () | Error _ -> Alcotest.fail "rows out of order accepted");
    ]
  @ List.map QCheck_alcotest.to_alcotest
      [
        QCheck2.Test.make ~count:200
          ~name:"any split of key-ordered rows into runs builds a tablespace that passes the check and reads back the rows"
          ~print:(fun scenario -> Sexplib0.Sexp.to_string_hum (sexp_of_scenario scenario))
          scenario
          (fun scenario ->
            let rows, layout, file = build_scenario scenario in
            match checked layout file with
            | Error _ as failed -> QCheck2.Test.fail_reportf "%s" (check_error failed)
            | Ok checked ->
                checked.rows = List.length rows
                && (decoded layout file = rows || QCheck2.Test.fail_reportf "rows read back differ"));
      ]

let tests =
  tests
  @ [
      Alcotest.test_case "tablespaces for the server to import, when asked" `Quick (fun () ->
          match Sys.getenv_opt "QUIVER_IBD_OUT" with
          | None -> ()
          | Some dir ->
              let write name scenario =
                let rows, _, file = build_scenario scenario in
                let out = Filename.concat dir name in
                Out_channel.with_open_bin out (fun channel -> Out_channel.output_bytes channel file);
                Out_channel.with_open_bin (out ^ ".csv") (fun channel ->
                    List.iter
                      (fun row ->
                        Out_channel.output_string channel
                          (String.concat ","
                             (List.map
                                (function
                                  | `Null -> "\\N"
                                  | `Int v -> string_of_int v
                                  | `Text v -> String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq v))))
                                row)
                          ^ "\n"))
                      rows)
              in
              write "narrow.ibd" { shape = Narrow; rows = 1_000_000; seed = 5; cuts = [ 250_000; 500_000; 750_000 ]; order = [ 3; 1; 0; 2 ]; batch = 4096 };
              write "wide.ibd" { shape = Wide; rows = 200_000; seed = 6; cuts = [ 50_000; 100_000; 150_000 ]; order = [ 2; 0; 3; 1 ]; batch = 1000 });
    ]

(** A value that crosses to other threads. *)
type cell : immutable_data = Null | Int of int | Text of string

let cell = function `Null -> Null | `Int v -> Int v | `Text v -> Text v

let filled : type a. a Quiver_core.Column.t -> a Quiver_core.Column.repr -> cell -> (unit, string) result =
 fun column repr value ->
  let added =
    match (repr, value) with
    | _, Null ->
        Quiver_core.Column.add_null column;
        Ok ()
    | Int64, Int v -> Quiver_core.Column.add column (Int64.of_int v)
    | Int, Int v -> Quiver_core.Column.add column v
    | Text, Text v -> Quiver_core.Column.add column v
    | _ -> Ok ()
  in
  Result.map_error (fun (`Column error) -> Sexplib0.Sexp.to_string (Quiver_core.Column.sexp_of_error error)) added

let column_of ltype values =
  let (Quiver_core.Column.Any column) = Quiver_core.Column.create ltype in
  let repr = Quiver_core.Column.repr column in
  let rec go = function
    | [] -> Ok (Quiver_core.Column.Any column)
    | value :: rest -> Result.bind (filled column repr value) (fun () -> go rest)
  in
  go values

let described = function
  | `Tablespace error -> Sexplib0.Sexp.to_string (Quiver_innodb.Tablespace.sexp_of_error error)
  | `File error -> Sexplib0.Sexp.to_string (Quiver_innodb.File.sexp_of_error error)

let tests =
  tests
  @ [
      Alcotest.test_case "runs on threads of their own build one tablespace" `Quick (fun () ->
          List.iter
            (fun scenario ->
              let cfg, template, ltypes =
                match scenario.shape with
                | Narrow -> ("narrow.cfg", "narrow.ibd", [ Quiver_core.Ltype.Int64; Int32 ])
                | Wide -> ("wide.cfg", "wide.ibd", [ Quiver_core.Ltype.Int64; Int32; String; Bytes ])
              in
              let names = [ "id"; "v"; "s"; "b" ] in
              let dumped = List.combine (Base.List.take names (List.length ltypes)) ltypes in
              let layout = ok (Quiver_innodb.Layout.of_cfg (ok (Quiver_innodb.Cfg.parse (fixture cfg))) ~collations dumped) in
              let path = Filename.temp_file "quiver" ".ibd" in
              let file = ok (Quiver_innodb.File.create path) in
              let scratch = ok (Quiver_innodb.File.create (path ^ ".sort")) in
              let space = ok (Quiver_innodb.Tablespace.create ~template:(fixture template) layout ~secondary:[] file ~scratch ~threads:4) in
              let rows = rows_of scenario in
              let runs = 8 in
              let per = (List.length rows + runs - 1) / runs in
              let slices =
                Iarray.of_list
                  (List.init runs (fun r ->
                       Iarray.of_list (List.map (List.map cell) (Base.List.take (Base.List.drop rows (r * per)) per))))
              in
              let batch = scenario.batch in
              let finished =
                Quiver_core.Concurrent.each runs (fun r ->
                    let slice = Iarray.get slices r in
                    let run = Quiver_innodb.Tablespace.run space ~rank:0 in
                    let rec go at =
                      if at >= Iarray.length slice then Ok ()
                      else
                        let n = Int.min batch (Iarray.length slice - at) in
                        let chunk = List.init n (fun k -> Iarray.get slice (at + k)) in
                        let columns =
                          Base.Result.all
                            (List.mapi (fun c ltype -> column_of ltype (List.map (fun row -> List.nth row c) chunk)) ltypes)
                        in
                        match columns with
                        | Error _ as failed -> failed
                        | Ok columns -> (
                            match Quiver_innodb.Tablespace.encode space (Iarray.of_list columns) ~rows:n with
                            | Error error -> Error (described error)
                            | Ok records -> (
                                match Quiver_innodb.Tablespace.add run records with
                                | Error error -> Error (described error)
                                | Ok () -> go (at + batch)))
                    in
                    let built : (Quiver_innodb.Tablespace.finished, string) result =
                      match go 0 with
                      | Error message -> Error message
                      | Ok () -> Result.map_error described (Quiver_innodb.Tablespace.close run)
                    in
                    built)
              in
              let finished = List.map (function Ok finished -> finished | Error message -> Alcotest.fail message) finished in
              let (_ : int) = ok (Quiver_innodb.Tablespace.finish space finished) in
              Quiver_innodb.File.close file;
              Quiver_innodb.File.close scratch;
              let bytes = Bytes.of_string (In_channel.with_open_bin path In_channel.input_all) in
              Sys.remove path;
  Sys.remove (path ^ ".sort");
              match checked layout bytes with
              | Error _ as failed -> Alcotest.fail (check_error failed)
              | Ok checked ->
                  Alcotest.(check int) "rows" (List.length rows) checked.rows;
                  Alcotest.(check bool) "read back" true (decoded layout bytes = rows))
            [
              { shape = Narrow; rows = 300_000; seed = 11; cuts = []; order = []; batch = 2048 };
              { shape = Wide; rows = 60_000; seed = 12; cuts = []; order = []; batch = 700 };
            ]);
    ]

let micros y m d h mi s us =
  (((Quiver_core.Moment.days_since_epoch (Core.Date.create_exn ~y ~m:(Core.Month.of_int_exn m) ~d) * 86_400)
   + (h * 3600) + (mi * 60) + s)
   * 1_000_000)
  + us

let days y m d = Quiver_core.Moment.days_since_epoch (Core.Date.create_exn ~y ~m:(Core.Month.of_int_exn m) ~d)
let duration sign h mi s us = sign * ((((h * 3600) + (mi * 60) + s) * 1_000_000) + us)
let hex text = String.init (String.length text / 2) (fun k -> Char.chr (int_of_string ("0x" ^ String.sub text (2 * k) 2)))
let fsp n = match Quiver_core.Ltype.Fsp.of_int n with Ok fsp -> fsp | Error (`Ltype _) -> Alcotest.failf "fsp %d" n
let decimal precision scale =
  match Quiver_core.Ltype.Decimal.make ~precision ~scale with
  | Ok decimal -> Quiver_core.Ltype.Decimal decimal
  | Error _ -> Alcotest.fail "decimal"

(** The server's own rows of every type the writer stores, in batch form: [typed-rows.ibd] holds them as
    inserted by [INSERT] (in [test/innodb], from an empty [typed.ibd]). *)
let typed =
  let ( * ) name (ltype : Quiver_core.Ltype.t) = (name, ltype) in
  [
    ("id" * Quiver_core.Ltype.Int32, [ `Int 1; `Int 1; `Int 2; `Int 3 ]);
    ( "at" * Datetime (fsp 6),
      [ `Int (micros 1000 1 1 0 0 0 0); `Int (micros 2020 2 29 12 34 56 789012); `Int (micros 9999 12 31 23 59 59 999999);
        `Int (micros 1969 12 31 23 59 59 999999) ] );
    ("d" * Date, [ `Int (days 1000 1 1); `Int (days 2020 2 29); `Int (days 9999 12 31); `Null ]);
    ("dt" * Datetime (fsp 0), [ `Int (micros 1000 1 1 0 0 0 0); `Int (micros 2020 2 29 12 34 56 0); `Int (micros 9999 12 31 23 59 59 0); `Null ]);
    ( "dt3" * Datetime (fsp 3),
      [ `Int (micros 1969 12 31 23 59 59 999000); `Int (micros 2020 2 29 12 34 56 789000); `Int (micros 9999 12 31 23 59 59 999000); `Null ] );
    ( "ts" * Timestamp (fsp 2),
      [ `Int (micros 1970 1 1 0 0 1 0); `Int (micros 2020 2 29 12 34 56 780000); `Int (micros 1999 12 31 23 59 59 990000); `Null ] );
    ( "ts6" * Timestamp (fsp 6),
      [ `Int (micros 2038 1 19 3 14 7 999999); `Int (micros 2020 2 29 12 34 56 789012); `Int (micros 1999 12 31 23 59 59 1); `Null ] );
    ("tm" * Time (fsp 0), [ `Int (duration (-1) 838 59 59 0); `Int (duration 1 838 59 59 0); `Int 0; `Null ]);
    ("tm1" * Time (fsp 1), [ `Int (duration (-1) 0 0 0 500000); `Int (duration 1 0 0 0 500000); `Int (duration (-1) 0 0 1 0); `Null ]);
    ("tm4" * Time (fsp 4), [ `Int (duration (-1) 1 2 3 400); `Int (duration 1 1 2 3 400); `Int (duration (-1) 0 0 0 100); `Null ]);
    ("tm6" * Time (fsp 6), [ `Int (duration (-1) 12 0 0 1); `Int (duration 1 12 0 0 1); `Int (duration (-1) 0 0 0 1); `Null ]);
    ("y" * Year, [ `Int 0; `Int 1901; `Int 2155; `Null ]);
    ("dc" * decimal 20 6, [ `Text "-12345678901234.123456"; `Text "12345678901234.123456"; `Text "0.000000"; `Null ]);
    ("dc2" * decimal 5 0, [ `Text "-99999"; `Text "99999"; `Text "0"; `Null ]);
    ( "dc3" * decimal 30 15,
      [ `Text "-123456789012345.123456789012345"; `Text "123456789012345.123456789012345"; `Text "0.000000000000000"; `Null ] );
    ("dc4" * decimal 9 9, [ `Text "-0.000000001"; `Text "0.999999999"; `Text "0.000000000"; `Null ]);
    ("f" * Float32, [ `Float (-1.5); `Float (Int32.float_of_bits 0x7F7FFFFFl); `Float 0.; `Null ]);
    ("db" * Float64, [ `Float (-2.2250738585072014e-308); `Float 1.7976931348623157e308; `Float 0.; `Null ]);
    ("e" * Enum [ "a"; "b"; "c" ], [ `Text ""; `Text "c"; `Text "a"; `Null ]);
    ("st" * Set [ "x"; "y"; "z" ], [ `Text ""; `Text "x,z"; `Text "x,y,z"; `Null ]);
    ("b" * Uint64, [ `Int 0; `Int 0b1010101010; `Int 1023; `Null ]);
    ("b64" * Uint64, [ `Int 0; `Int64 (-1L); `Int 1; `Null ]);
    ("c" * String, [ `Text ""; `Text "ab"; `Text "abcde"; `Null ]);
    ("c70" * String, [ `Text ""; `Text "ж"; `Text (String.concat "" (List.init 70 (fun _ -> "ж"))); `Null ]);
    ("cb" * String, [ `Text ""; `Text "Ab"; `Text "ж"; `Null ]);
    ("bn" * Bytes, [ `Text ""; `Text "ab"; `Text "abcd"; `Null ]);
    ( "g" * Geometry,
      [ `Text (hex "000000000101000000000000000000F03F0000000000000040");
        `Text
          (hex
             "0000000001020000000300000000000000000000000000000000000000000000000000F03F000000000000F03F00000000000000400000000000000040");
        `Text (hex "000000000101000000000000000000F8BF0000000000000240"); `Null ] );
    ("u8" * Uint8, [ `Int 0; `Int 255; `Int 1; `Null ]);
    ("m" * Int32, [ `Int (-8388608); `Int 8388607; `Int 0; `Null ]);
    ("vc" * String, [ `Text ""; `Text "é😀"; `Text (String.make 300 'x'); `Null ]);
  ]

let typed_value : type a.
    a Quiver_core.Column.t ->
    a Quiver_core.Column.repr ->
    [ `Null | `Int of int | `Int64 of int64 | `Float of float | `Text of string ] ->
    unit =
 fun column repr value ->
  let added : (unit, [ `Column of Quiver_core.Column.error ]) result =
    match (repr, value) with
    | _, `Null ->
        Quiver_core.Column.add_null column;
        Ok ()
    | Int64, `Int v -> Quiver_core.Column.add column (Int64.of_int v)
    | Int64, `Int64 v -> Quiver_core.Column.add column v
    | Int, `Int v -> Quiver_core.Column.add column v
    | Float, `Float v -> Quiver_core.Column.add column v
    | Text, `Text v -> Quiver_core.Column.add column v
    | _ -> Alcotest.fail "value of another type"
  in
  ok added

let typed_column (ltype, values) =
  let (Quiver_core.Column.Any column) = Quiver_core.Column.create ltype in
  List.iter (typed_value column (Quiver_core.Column.repr column)) values;
  Quiver_core.Column.Any column

let typed_build ?(table = "typed") ?(runs = 1) typed =
  let dumped = List.map fst typed in
  let layout = ok (Quiver_innodb.Layout.of_cfg (ok (Quiver_innodb.Cfg.parse (fixture (table ^ ".cfg")))) ~collations dumped) in
  let path = Filename.temp_file "quiver" ".ibd" in
  let file = ok (Quiver_innodb.File.create path) in
  let scratch = ok (Quiver_innodb.File.create (path ^ ".sort")) in
  let space = ok (Quiver_innodb.Tablespace.create ~template:(fixture (table ^ ".ibd")) layout ~secondary:[] file ~scratch ~threads:4) in
  let rows = List.length (snd (List.hd typed)) in
  let cuts = List.init runs (fun r -> (r * rows / runs, ((r + 1) * rows / runs) - (r * rows / runs))) in
  let started = List.mapi (fun rank cut -> (cut, Quiver_innodb.Tablespace.run space ~rank)) cuts in
  let built =
    let%bind.Base.Result finished =
      Base.Result.all
        (List.rev_map
           (fun ((from, count), run) ->
             let columns =
               Iarray.of_list
                 (List.map (fun ((_, ltype), values) -> typed_column (ltype, Base.List.sub values ~pos:from ~len:count)) typed)
             in
             let%bind.Base.Result records = Quiver_innodb.Tablespace.encode space columns ~rows:count in
             let%bind.Base.Result () = Quiver_innodb.Tablespace.add run records in
             Quiver_innodb.Tablespace.close run)
           started)
    in
    Quiver_innodb.Tablespace.finish space finished
  in
  Quiver_innodb.File.close file;
  Quiver_innodb.File.close scratch;
  let bytes = Bytes.of_string (In_channel.with_open_bin path In_channel.input_all) in
  Sys.remove path;
  Sys.remove (path ^ ".sort");
  (layout, Result.map (fun (_ : int) -> bytes) built)

(** [utf8mb3.ibd]'s table: text of one to three bytes a character in [CHAR], [VARCHAR], [TEXT] and
    a [LONGTEXT] off its record, as [utf8mb3-rows.ibd] holds them after [INSERT]. *)
let utf8mb3 =
  let ( * ) name (ltype : Quiver_core.Ltype.t) = (name, ltype) in
  let repeat n text = String.concat "" (List.init n (fun _ -> text)) in
  [
    ("id" * Quiver_core.Ltype.Int32, [ `Int 1; `Int 2; `Int 3; `Int 4 ]);
    ("c" * String, [ `Text ""; `Text "ab"; `Text (repeat 5 "€"); `Null ]);
    ("c70" * String, [ `Text ""; `Text "ж"; `Text (repeat 70 "€"); `Null ]);
    ("c100" * String, [ `Text ""; `Text "a b"; `Text (repeat 100 "€"); `Text "x" ]);
    ("vc" * String, [ `Text ""; `Text "é€"; `Text (repeat 300 "€"); `Null ]);
    ("v80" * String, [ `Text ""; `Text (repeat 80 "€"); `Text (repeat 80 "ж"); `Null ]);
    ("t" * String, [ `Text ""; `Text "tëxt"; `Text (repeat 1000 "ж"); `Null ]);
    ("v4" * String, [ `Text ""; `Text "😀"; `Text (repeat 10 "😀"); `Null ]);
    ("tl" * String, [ `Text ""; `Text "short"; `Text (repeat 20000 "€"); `Null ]);
  ]

(** [generated.ibd]'s table: [STORED] generated columns between written ones, read after them as
    the dump reads them, and a [VIRTUAL] one the record lacks; [generated-rows.ibd] holds the rows
    after [INSERT]. *)
let generated =
  let ( * ) name (ltype : Quiver_core.Ltype.t) = (name, ltype) in
  [
    ("id" * Quiver_core.Ltype.Int32, [ `Int 1; `Int 2; `Int 3; `Int 4 ]);
    ("a" * Int32, [ `Int 5; `Null; `Int (-7); `Int 2147483 ]);
    ("s" * String, [ `Text "ab"; `Text "жук"; `Null; `Text "Émile" ]);
    ("tail" * String, [ `Text "x"; `Null; `Text "tail"; `Text "" ]);
    ("total" * Int64, [ `Int 1005; `Null; `Int 2993; `Int 2151483 ]);
    ("up" * String, [ `Text "AB"; `Text "ЖУК"; `Null; `Text "ÉMILE" ]);
    ( "dt" * Datetime (fsp 0),
      [ `Int (micros 2020 1 2 0 0 0 0); `Int (micros 2020 1 3 0 0 0 0); `Int (micros 2020 1 4 0 0 0 0); `Int (micros 2020 1 5 0 0 0 0) ] );
  ]

(** [nokey.ibd]'s table, without a primary key: rows 1 to 700 as [nokey-rows.ibd] holds them after
    [INSERT], over several pages, with NULLs and a [TEXT] of 20 000 [ж] off its record. *)
let nokey =
  let k = List.init 700 (fun k -> k + 1) in
  [
    (("a", Quiver_core.Ltype.Int32), List.map (fun k -> if k mod 9 = 0 then `Null else `Int ((k * 7) - 1000)) k);
    (("b", Quiver_core.Ltype.String), List.map (fun k -> if k mod 4 = 0 then `Null else `Text (String.make (k mod 100) (Char.chr (97 + (k mod 26))))) k);
    ( ("c", Quiver_core.Ltype.String),
      List.map
        (fun k ->
          if k = 350 then `Text (String.concat "" (List.init 20000 (fun _ -> "ж")))
          else if k mod 3 = 0 then `Null
          else `Text ("t" ^ string_of_int k))
        k );
    (("d", Quiver_core.Ltype.Int64), List.map (fun k -> `Int k) k);
  ]

(** [latin1.ibd]'s table: [latin1] text of every byte in fixed [CHAR], [VARCHAR], [TEXT] and a
    [LONGTEXT] off its record, as [latin1-rows.ibd] holds it after [INSERT]; [latin1.txt] is the
    same rows as the dump reads them, in [utf8mb4], in hex. *)
let latin1 =
  let rows =
    List.map
      (fun line -> String.split_on_char '\t' line)
      (List.filter (fun line -> line <> "") (String.split_on_char '\n' (fixture "latin1.txt")))
  in
  let column k = List.map (fun row -> match List.nth row k with "NULL" -> `Null | text -> `Text (hex text)) rows in
  (("id", Quiver_core.Ltype.Int32), List.map (fun row -> `Int (int_of_string (List.hd row))) rows)
  :: List.mapi (fun k name -> ((name, Quiver_core.Ltype.String), column (k + 1))) [ "c"; "cb"; "v"; "vg"; "t"; "tl" ]

(** [textkey.ibd]'s table, keyed by [(VARCHAR utf8mb4_0900_ai_ci, INT)]: 3000 rows in the
    collation's order, keys up to 149 bytes, as [textkey-rows.ibd] holds them after [INSERT];
    [textkey.txt] is the rows in that order. *)
let textkey =
  let rows =
    List.map (String.split_on_char '\t') (List.filter (fun line -> line <> "") (String.split_on_char '\n' (fixture "textkey.txt")))
  in
  [
    (("name", Quiver_core.Ltype.String), List.map (fun row -> `Text (hex (List.nth row 0))) rows);
    (("n", Quiver_core.Ltype.Int32), List.map (fun row -> `Int (int_of_string (List.nth row 1))) rows);
    (("v", Quiver_core.Ltype.String), List.map (fun row -> match List.nth row 2 with "NULL" -> `Null | v -> `Text (hex v)) rows);
  ]

let as_server table typed =
  let layout, built = typed_build ~table typed in
  let bytes = ok built in
  let () = match checked layout bytes with Ok _ -> () | Error _ as failed -> Alcotest.fail (check_error failed) in
  let server = Bytes.of_string (fixture (table ^ "-rows.ibd")) in
  let names = List.map (fun ((name, _), _) -> name) typed in
  let user records =
    List.map
      (List.filter_map (fun ((field : Quiver_innodb.Layout.field), raw) ->
           if field.source < 0 then None else Some (List.nth names field.source, raw)))
      records
  in
  let ours = user (stored layout bytes) and theirs = user (stored layout server) in
  Alcotest.(check int) "records" (List.length theirs) (List.length ours);
  List.iteri
    (fun r (ours, theirs) ->
      List.iter2
        (fun (name, ours) (_, theirs) ->
          let shown = Option.fold ~none:"NULL" ~some:(fun raw -> String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq raw)))) in
          Alcotest.(check string) (Printf.sprintf "row %d %s" r name) (shown theirs) (shown ours))
        ours theirs)
    (List.combine ours theirs)

let tests =
  tests
  @ [
      Alcotest.test_case "every stored type is written byte for byte as the server writes it" `Quick (fun () ->
          as_server "typed" typed);
      Alcotest.test_case "utf8mb3 text is written byte for byte as the server writes it" `Quick (fun () ->
          as_server "utf8mb3" utf8mb3);
      Alcotest.test_case "stored generated columns read after the others are written in their place" `Quick (fun () ->
          as_server "generated" generated);
      Alcotest.test_case "a table without a primary key is written as the server writes it, numbered by its runs" `Quick
        (fun () ->
          as_server "nokey" nokey;
          List.iter
            (fun runs ->
              let layout, built = typed_build ~table:"nokey" ~runs nokey in
              let bytes = ok built in
              let () = match checked layout bytes with Ok _ -> () | Error _ as failed -> Alcotest.fail (check_error failed) in
              let ids =
                List.map
                  (fun record ->
                    match record with
                    | ((field : Quiver_innodb.Layout.field), Some raw) :: _ when field.storage = Row_id -> raw
                    | _ -> Alcotest.fail "no row id first")
                  (stored layout bytes)
              in
              Alcotest.(check int) "rows" 700 (List.length ids);
              Alcotest.(check bool) "ascending" true (List.sort_uniq String.compare ids = ids))
            [ 1; 3; 7 ]);
      Alcotest.test_case "latin1 text is transcoded byte for byte as the server stores it" `Quick (fun () ->
          as_server "latin1" latin1;
          List.iter
            (fun (name, value, expected) ->
              let replaced =
                List.map
                  (fun (((field, _) as column), values) ->
                    if field = name then (column, List.mapi (fun r v -> if r = 1 then `Text value else v) values) else (column, values))
                  latin1
              in
              match typed_build ~table:"latin1" replaced with
              | _, Error (`Tablespace error) ->
                  Alcotest.(check string) name expected (Sexplib0.Sexp.to_string (Quiver_innodb.Tablespace.sexp_of_error error))
              | _, Error _ -> Alcotest.failf "%s: another error" name
              | _, Ok _ -> Alcotest.failf "%s: built" name)
            [ ("v", "ж", "(Unstorable 3)"); ("t", "\xc2\x80", "(Unstorable 5)"); ("tl", "😀", "(Unstorable 6)");
              ("v", "\xc0\xa9", "(Unstorable 3)"); ("c", "abcdef", "(Value_too_long 6)"); ("c", "€€€€€€", "(Value_too_long 6)") ]);
      Alcotest.test_case "a key in a collation's order is written as the server writes it, runs by rank" `Quick (fun () ->
          as_server "textkey" textkey;
          let ours runs =
            let layout, built = typed_build ~table:"textkey" ~runs textkey in
            let bytes = ok built in
            let () = match checked layout bytes with Ok _ -> () | Error _ as failed -> Alcotest.fail (check_error failed) in
            List.map (List.map snd) (stored layout bytes)
          in
          let one = ours 1 in
          List.iter (fun runs -> Alcotest.(check bool) (Printf.sprintf "%d runs" runs) true (ours runs = one)) [ 2; 5 ]);
      Alcotest.test_case "a character of four bytes is refused in utf8mb3 text" `Quick (fun () ->
          List.iter
            (fun name ->
              let replaced =
                List.map
                  (fun (((field, _) as column), values) ->
                    if field = name then (column, List.mapi (fun r v -> if r = 1 then `Text "a😀" else v) values) else (column, values))
                  utf8mb3
              in
              match typed_build ~table:"utf8mb3" replaced with
              | _, Error (`Tablespace (Unstorable _)) -> ()
              | _, Error _ -> Alcotest.failf "%s: another error" name
              | _, Ok _ -> Alcotest.failf "%s: built" name)
            [ "c"; "vc"; "t"; "tl" ];
          match typed_build ~table:"utf8mb3" utf8mb3 with _, Ok _ -> () | _, Error _ -> Alcotest.fail "the rows themselves");

      Alcotest.test_case "dates read unchecked are stored as the same dates read checked" `Quick (fun () ->
          let fields days =
            let date = Quiver_core.Moment.date_of_days days in
            (Core.Date.year date lsl 9) lor (Core.Month.to_int (Core.Date.month date) lsl 5) lor Core.Date.day date
          in
          let unchecked micros =
            let days = Base.Int.( /% ) micros Quiver_core.Moment.micros_per_day in
            (fields days lsl 37) lor (micros - (days * Quiver_core.Moment.micros_per_day))
          in
          let relaxed =
            List.map
              (fun (((name, (ltype : Quiver_core.Ltype.t)) as column), values) ->
                match ltype with
                | Date -> ((name, Quiver_core.Ltype.Unchecked_date), List.map (function `Int v -> `Int (fields v) | v -> v) values)
                | Datetime fsp | Timestamp fsp ->
                    ((name, Quiver_core.Ltype.Unchecked_datetime fsp), List.map (function `Int v -> `Int (unchecked v) | v -> v) values)
                | _ -> (column, values))
              typed
          in
          let _, checked = typed_build typed and _, relaxed = typed_build relaxed in
          Alcotest.(check bool) "same file" true (Bytes.equal (ok checked) (ok relaxed)));
      Alcotest.test_case "a value the column cannot hold is refused" `Quick (fun () ->
          let replaced name value =
            List.map
              (fun (((field, _) as column), values) ->
                if field = name then (column, List.mapi (fun r v -> if r = 1 then value else v) values) else (column, values))
              typed
          in
          List.iter
            (fun (name, value, expected) ->
              match typed_build (replaced name value) with
              | _, Error (`Tablespace error) ->
                  Alcotest.(check string) name expected (Sexplib0.Sexp.to_string (Quiver_innodb.Tablespace.sexp_of_error error))
              | _, Error (`File _) -> Alcotest.fail "file"
              | _, Ok _ -> Alcotest.failf "%s: built" name)
            [
              ("e", `Text "d", "(Unstorable 18)");
              ("st", `Text "x,w", "(Unstorable 19)");
              ("st", `Text "x,", "(Unstorable 19)");
              ("dc", `Text "123456789012345.0", "(Unstorable 12)");
              ("dc", `Text "1.0000001", "(Unstorable 12)");
              ("dc", `Text "1e5", "(Unstorable 12)");
              ("bn", `Text "abcde", "(Value_too_long 5)");
            ]);
      Alcotest.test_case "an unchecked TIMESTAMP stores its zero as 0 and refuses a day that is not" `Quick (fun () ->
          let fields y m d = `Int (((y lsl 9) lor (m lsl 5) lor d) lsl 37) in
          let relaxed values =
            List.map
              (fun (((name, (ltype : Quiver_core.Ltype.t)) as column), rows) ->
                match (name, ltype) with
                | "ts", Timestamp fsp -> ((name, Quiver_core.Ltype.Unchecked_datetime fsp), values)
                | "ts6", Timestamp fsp -> ((name, Quiver_core.Ltype.Unchecked_datetime fsp), List.map (fun _ -> `Null) rows)
                | _ -> (column, rows))
              typed
          in
          let layout, built = typed_build (relaxed [ `Int 0; fields 2020 2 29; `Int 0; `Null ]) in
          let stored = stored layout (ok built) in
          let field (record : (Quiver_innodb.Layout.field * string option) list) =
            List.find_map
              (fun ((field : Quiver_innodb.Layout.field), raw) ->
                if field.source >= 0 && fst (fst (List.nth typed field.source)) = "ts" then Some raw else None)
              record
          in
          Alcotest.(check (list (option string)))
            "zero" [ Some "\000\000\000\000\000"; Some "\000\000\000\000\000" ]
            (List.filter_map (fun record -> field record) [ List.nth stored 0; List.nth stored 2 ]);
          List.iter
            (fun (y, m, d) ->
              match typed_build (relaxed [ `Int 0; fields y m d; `Int 0; `Null ]) with
              | _, Error (`Tablespace (Unstorable 5)) -> ()
              | _, Error _ -> Alcotest.failf "%d-%d-%d: another error" y m d
              | _, Ok _ -> Alcotest.failf "%d-%d-%d: built" y m d)
            [ (2021, 2, 29); (2020, 0, 1); (2020, 1, 0); (1969, 12, 31); (2040, 1, 1) ]);
      Alcotest.test_case "text in another character set than the dump's is not built" `Quick (fun () ->
          let cfg = ok (Quiver_innodb.Cfg.parse (fixture "typed.cfg")) in
          let dumped = List.map fst typed in
          match Quiver_innodb.Layout.of_cfg cfg ~collations:(List.filter (fun (id, _) -> id <> 255) collations) dumped with
          | Error (`Layout (Charset { column = "c"; collation = 255 })) -> ()
          | Error (`Layout error) -> Alcotest.failf "%s" (Sexplib0.Sexp.to_string (Quiver_innodb.Layout.sexp_of_error error))
          | Ok _ -> Alcotest.fail "built");
    ]

let repeat n text = String.concat "" (List.init n (fun _ -> text))

(** Rows of [outside.ibd]'s table, as [outside-rows.ibd] holds them after [INSERT]: values from a
    few bytes to 210 000, so the server stored the long ones off their records. *)
let outside_rows =
  [
    [ `Int 1; `Text "short"; `Text "\000\255"; `Text "v"; `Text "s" ];
    [ `Int 2; `Text (repeat 400 "abcdefghijklmnopqrstuvwxyz"); `Null; `Null; `Text "two" ];
    [ `Int 3; `Null; `Text (repeat 70000 "\000\001\254"); `Text (repeat 3000 "ж"); `Null ];
    [ `Int 4; `Text (String.make 15680 'q'); `Text (String.make 15681 '\127'); `Text (String.make 3000 'x'); `Text "edge" ];
    [ `Int 5; `Null; `Null; `Null; `Null ];
    [ `Int 6; `Text (repeat 4000 "é"); `Text (String.make 8000 'B'); `Text (String.make 100 'y'); `Text "six" ];
  ]

let outside_columns = [ ("id", Quiver_core.Ltype.Int64); ("t", String); ("b", Bytes); ("v", String); ("s", String) ]

let outside_layout () =
  ok (Quiver_innodb.Layout.of_cfg (ok (Quiver_innodb.Cfg.parse (fixture "outside.cfg"))) ~collations outside_columns)

let outside_build rows ~cuts =
  let layout = outside_layout () in
  let path = Filename.temp_file "quiver" ".ibd" in
  let file = ok (Quiver_innodb.File.create path) in
  let scratch = ok (Quiver_innodb.File.create (path ^ ".sort")) in
  let space = ok (Quiver_innodb.Tablespace.create ~template:(fixture "outside.ibd") layout ~secondary:[] file ~scratch ~threads:4) in
  let rows = Array.of_list rows in
  let bounds = List.sort_uniq compare ((0 :: cuts) @ [ Array.length rows ]) in
  let slices = List.filteri (fun k _ -> k + 1 < List.length bounds) bounds in
  let finished =
    List.mapi
      (fun k from ->
        let until = List.nth bounds (k + 1) in
        let run = Quiver_innodb.Tablespace.run space ~rank:0 in
        let slice = Array.to_list (Array.sub rows from (until - from)) in
        let columns =
          Iarray.of_list
            (List.mapi (fun c (_, ltype) -> typed_column (ltype, List.map (fun row -> List.nth row c) slice)) outside_columns)
        in
        let records = ok (Quiver_innodb.Tablespace.encode space columns ~rows:(List.length slice)) in
        ok (Quiver_innodb.Tablespace.add run records);
        ok (Quiver_innodb.Tablespace.close run))
      slices
  in
  let (_ : int) = ok (Quiver_innodb.Tablespace.finish space (List.rev finished)) in
  Quiver_innodb.File.close file;
  Quiver_innodb.File.close scratch;
  let bytes = Bytes.of_string (In_channel.with_open_bin path In_channel.input_all) in
  Sys.remove path;
  Sys.remove (path ^ ".sort");
  (layout, bytes)

let read_back layout file =
  List.map
    (List.filter_map (fun ((field : Quiver_innodb.Layout.field), raw) ->
         match (field.source, raw) with
         | -1, _ -> None
         | 0, Some raw -> Some (`Int (Int64.to_int (Int64.logxor (String.get_int64_be raw 0) Int64.min_int)))
         | _, None -> Some `Null
         | _, Some raw -> Some (`Text raw)))
    (stored layout file)

let shown rows =
  String.concat "\n"
    (List.map
       (fun row ->
         String.concat " "
           (List.map
              (function
                | `Null -> "NULL" | `Int v -> string_of_int v
                | `Text v -> Printf.sprintf "%d:%s" (String.length v) (Digest.to_hex (Digest.string v)))
              row))
       rows)

let tests =
  tests
  @ [
      Alcotest.test_case "the server's columns stored off their records pass the check and read back" `Quick (fun () ->
          let layout = outside_layout () in
          let server = Bytes.of_string (fixture "outside-rows.ibd") in
          let () = match checked layout server with Ok _ -> () | Error _ as failed -> Alcotest.fail (check_error failed) in
          Alcotest.(check string) "rows" (shown outside_rows) (shown (read_back layout server)));
      Alcotest.test_case "the longest columns of a row too long for a page go off its record" `Quick (fun () ->
          List.iter
            (fun cuts ->
              let layout, file = outside_build outside_rows ~cuts in
              let () = match checked layout file with Ok _ -> () | Error _ as failed -> Alcotest.fail (check_error failed) in
              Alcotest.(check string) "rows" (shown outside_rows) (shown (read_back layout file)))
            [ []; [ 3 ]; [ 1; 2; 4; 5 ] ]);
      Alcotest.test_case "a column of five megabytes takes two pages of its pieces' index" `Quick (fun () ->
          let rows = [ [ `Int 7; `Text (String.init 5_000_000 (fun k -> Char.chr (97 + (k mod 26)))); `Null; `Null; `Null ] ] in
          let layout, file = outside_build rows ~cuts:[] in
          let () = match checked layout file with Ok _ -> () | Error _ as failed -> Alcotest.fail (check_error failed) in
          Alcotest.(check string) "rows" (shown rows) (shown (read_back layout file)));
      Alcotest.test_case "a broken column off its record fails the check" `Quick (fun () ->
          let layout, file = outside_build outside_rows ~cuts:[ 3 ] in
          let size = Quiver_innodb.Page.size in
          let pages_of kind =
            List.filter
              (fun n -> Bytes.get_uint16_be file ((n * size) + 24) = kind)
              (List.init (Bytes.length file / size) Fun.id)
          in
          let first = List.hd (pages_of 24) and data = List.hd (pages_of 23) and index = List.hd (pages_of 22) in
          let chained =
            List.find (fun n -> Bytes.get_int32_be file ((n * size) + 12) <> Int32.of_int Quiver_innodb.Page.null) (pages_of 24)
          in
          List.iter
            (fun (name, mutate) ->
              let copy = Bytes.copy file in
              List.iter (reseal copy) (mutate copy);
              match checked layout copy with
              | Error (`Check _) -> ()
              | Ok _ -> Alcotest.failf "%s: passed" name)
            [
              ("first page type", fun f -> Bytes.set_uint16_be f ((first * size) + 24) 23; [ first ]);
              ("first page length", fun f -> Bytes.set_int32_be f ((first * size) + 54) 7l; [ first ]);
              ("data page length", fun f -> Bytes.set_int32_be f ((data * size) + 39) 5l; [ data ]);
              ( "index list length",
                fun f ->
                  Bytes.set_int32_be f ((first * size) + 64) (Int32.succ (Bytes.get_int32_be f ((first * size) + 64)));
                  [ first ] );
              ("piece version", fun f -> Bytes.set_int32_be f ((first * size) + 96 + 56) 2l; [ first ]);
              ("free list back link", fun f -> Bytes.set_int32_be f ((index * size) + 39 + 60 + 60) 3l; [ index ]);
              ( "index page chain",
                fun f ->
                  Bytes.set_int32_be f ((chained * size) + 12) (Int32.of_int Quiver_innodb.Page.null);
                  [ chained ] );
            ]);
      QCheck_alcotest.to_alcotest
        (QCheck2.Test.make ~count:40 ~name:"rows of any length build into a file that passes the check and reads back"
           ~print:(fun (lengths, cuts) ->
             Printf.sprintf "%s cuts %s"
               (String.concat ";" (List.map (fun (a, b, c) -> Printf.sprintf "%d,%d,%d" a b c) lengths))
               (String.concat "," (List.map string_of_int cuts)))
           QCheck2.Gen.(
             let length = oneof [ int_range 0 100; int_range 3000 9000; int_range 15000 40000; int_range 150_000 250_000 ] in
             let* rows = list_size (int_range 1 12) (triple length length (int_range 0 3000)) in
             let* cuts = list_size (int_range 0 3) (int_range 0 (List.length rows)) in
             return (rows, cuts))
           (fun (lengths, cuts) ->
             let rows =
               List.mapi
                 (fun k (t, b, v) ->
                   let maybe n text = if n = 0 then `Null else `Text text in
                   [ `Int k; maybe t (String.init t (fun j -> Char.chr (97 + ((j + k) mod 26))));
                     maybe b (String.init b (fun j -> Char.chr ((j * 7) land 0xFF)));
                     maybe v (String.make v 'v'); `Text (string_of_int k) ])
                 lengths
             in
             let layout, file = outside_build rows ~cuts in
             let () = match checked layout file with Ok _ -> () | Error _ as failed -> Alcotest.fail (check_error failed) in
             shown rows = shown (read_back layout file)));
    ]

let json_texts () = String.split_on_char '\n' (String.trim (fixture "json.txt"))

let tests =
  tests
  @ [
      Alcotest.test_case "every JSON text the server prints becomes the binary it was printed from" `Quick (fun () ->
          let texts = json_texts () in
          let columns = [ ("id", Quiver_core.Ltype.Int32); ("j", Json) ] in
          let layout = ok (Quiver_innodb.Layout.of_cfg (ok (Quiver_innodb.Cfg.parse (fixture "json.cfg"))) ~collations columns) in
          let path = Filename.temp_file "quiver" ".ibd" in
          let file = ok (Quiver_innodb.File.create path) in
          let scratch = ok (Quiver_innodb.File.create (path ^ ".sort")) in
          let space = ok (Quiver_innodb.Tablespace.create ~template:(fixture "json.ibd") layout ~secondary:[] file ~scratch ~threads:4) in
          let run = Quiver_innodb.Tablespace.run space ~rank:0 in
          let batch =
            Iarray.of_list
              [ typed_column (Quiver_core.Ltype.Int32, List.mapi (fun k _ -> `Int (k + 1)) texts);
                typed_column (Quiver_core.Ltype.Json, List.map (fun text -> `Text text) texts) ]
          in
          ok (Quiver_innodb.Tablespace.add run (ok (Quiver_innodb.Tablespace.encode space batch ~rows:(List.length texts))));
          let (_ : int) = ok (Quiver_innodb.Tablespace.finish space [ ok (Quiver_innodb.Tablespace.close run) ]) in
          Quiver_innodb.File.close file;
          Quiver_innodb.File.close scratch;
          let built = Bytes.of_string (In_channel.with_open_bin path In_channel.input_all) in
          Sys.remove path;
  Sys.remove (path ^ ".sort");
          let () = match checked layout built with Ok _ -> () | Error _ as failed -> Alcotest.fail (check_error failed) in
          let values file =
            List.map
              (fun record ->
                List.filter_map
                  (fun ((field : Quiver_innodb.Layout.field), raw) -> if field.source = 1 then Some raw else None)
                  record)
              (stored layout file)
          in
          let hex = Option.fold ~none:"NULL" ~some:(fun raw -> String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq raw)))) in
          let theirs = values (Bytes.of_string (fixture "json-rows.ibd")) and ours = values built in
          Alcotest.(check int) "documents" (List.length theirs) (List.length ours);
          List.iteri
            (fun k (theirs, ours) ->
              let text = List.nth texts k in
              let shown = if String.length text > 200 then String.sub text 0 200 ^ "…" else text in
              Alcotest.(check (list string)) (Printf.sprintf "document %d %s" (k + 1) shown) (List.map hex theirs) (List.map hex ours))
            (List.combine theirs ours));
      Alcotest.test_case "text that is not JSON is refused" `Quick (fun () ->
          List.iter
            (fun text ->
              let bytes = Bytes.of_string text in
              match Quiver_innodb.Json.binary bytes ~pos:0 ~length:(Bytes.length bytes) with
              | Error (`Json _) -> ()
              | Ok _ -> Alcotest.failf "%S: accepted" text)
            [ ""; "nul"; "[1,]"; "{\"a\" 1}"; "01"; "1."; "1e"; "-"; "\"\\x\""; "\"\\ud800\""; "\"\\udc00\"";
              "\"a\001\""; "[1] x"; "1e400"; String.make 101 '[' ^ String.make 101 ']'; "\"abc" ]);
    ]

let tests =
  tests
  @ [
      Alcotest.test_case "a JSON number is set apart exactly when the server reads its own text as another double" `Quick
        (fun () ->
          let lines = String.split_on_char '\n' (String.trim (fixture "json-numbers.txt")) in
          let bits text = Int64.bits_of_float (float_of_string text) in
          let wrong =
            List.filter_map
              (fun line ->
                match String.split_on_char '\t' line with
                | [ printed; reread ] -> (
                    let bytes = Bytes.of_string printed in
                    let found = Quiver_sink.Json_numbers.misread bytes ~pos:0 ~length:(Bytes.length bytes) in
                    match (found, Int64.equal (bits printed) (bits reread)) with
                    | [], true -> None
                    | [ { path = "$"; literal } ], false when Int64.equal (bits literal) (bits printed) -> None
                    | _ -> Some line)
                | _ -> Some line)
              lines
          in
          Alcotest.(check int) "numbers" 30002 (List.length lines);
          Alcotest.(check (list string)) "wrong" [] (Base.List.take wrong 5));
      Alcotest.test_case "numbers set apart carry their JSON path" `Quick (fun () ->
          let text = {|{"a": [1, 0.49178151800806813, {"k \"q\"": 9.999999999999999e24}], "b": 0.5, "": [[8.690000000000001e-181]]}|} in
          let bytes = Bytes.of_string text in
          Alcotest.(check (list (pair string string)))
            "found"
            [ ("$.\"a\"[1]", "0.49178151800806813e0"); ("$.\"a\"[2].\"k \\\"q\\\"\"", "9.999999999999999e24");
              ("$.\"\"[0][0]", "8.690000000000001e-181") ]
            (List.map
               (fun ({ path; literal } : Quiver_sink.Json_numbers.misread) -> (path, literal))
               (Quiver_sink.Json_numbers.misread bytes ~pos:0 ~length:(Bytes.length bytes))));
    ]

(** [secondary.ibd]'s table with six secondary keys the server built by [ALTER TABLE … ADD KEY]
    over 20 000 rows, as [secondary-rows.ibd] holds them: [UNIQUE] over [BINARY(4)], [INT],
    [INT UNSIGNED], [(DATETIME(2), DECIMAL(10,3))], a [VARBINARY] prefix and one naming the
    primary key; NULL in every nullable key. [secondary.txt] is the rows. *)
let secondary_rows =
  let rows =
    List.map (String.split_on_char '\t') (List.filter (fun line -> line <> "") (String.split_on_char '\n' (fixture "secondary.txt")))
  in
  let field k f = List.map (fun row -> match List.nth row k with "NULL" -> `Null | v -> f v) rows in
  [
    (("id", Quiver_core.Ltype.Int64), field 0 (fun v -> `Int64 (Int64.of_string v)));
    (("a", Int32), field 1 (fun v -> `Int (int_of_string v)));
    (("b", Uint32), field 2 (fun v -> `Int (int_of_string v)));
    (("d", Datetime (fsp 2)), field 3 (fun v -> `Int64 (Int64.of_string v)));
    (("m", decimal 10 3), field 4 (fun v -> `Text v));
    (("bin", Bytes), field 5 (fun v -> `Text (hex v)));
    (("vb", Bytes), field 6 (fun v -> `Text (hex v)));
    (("s", String), field 7 (fun v -> `Text (hex v)));
  ]

let secondary_layouts cfg =
  let cfg = ok (Quiver_innodb.Cfg.parse (fixture cfg)) in
  let layout = ok (Quiver_innodb.Layout.of_cfg cfg ~collations (List.map fst secondary_rows)) in
  (layout, List.map (fun index -> ok (Quiver_innodb.Layout.secondary layout cfg index)) cfg.secondary)

let secondary_built typed ~cuts ~batch ~threads =
  let dumped = List.map fst typed in
  let cfg = ok (Quiver_innodb.Cfg.parse (fixture "secondary.cfg")) in
  let layout = ok (Quiver_innodb.Layout.of_cfg cfg ~collations dumped) in
  let others = List.map (fun index -> ok (Quiver_innodb.Layout.secondary layout cfg index)) cfg.secondary in
  let path = Filename.temp_file "quiver" ".ibd" in
  let file = ok (Quiver_innodb.File.create path) in
  let scratch = ok (Quiver_innodb.File.create (path ^ ".sort")) in
  let space =
    ok (Quiver_innodb.Tablespace.create ~template:(fixture "secondary.ibd") layout ~secondary:others file ~scratch ~threads)
  in
  let rows = List.length (snd (List.hd typed)) in
  let bounds = List.sort_uniq compare ((0 :: List.map (fun cut -> cut mod (rows + 1)) cuts) @ [ rows ]) in
  let ranges = List.combine (Base.List.drop_last_exn bounds) (List.tl bounds) in
  let built =
    let%bind.Base.Result finished =
      Base.Result.all
        (List.rev_map
           (fun (from, until) ->
             let run = Quiver_innodb.Tablespace.run space ~rank:0 in
             let rec go at =
               if at < until then
                 let n = Int.min batch (until - at) in
                 let columns =
                   Iarray.of_list
                     (List.map (fun ((_, ltype), values) -> typed_column (ltype, Base.List.sub values ~pos:at ~len:n)) typed)
                 in
                 let%bind.Base.Result records = Quiver_innodb.Tablespace.encode space columns ~rows:n in
                 let%bind.Base.Result () = Quiver_innodb.Tablespace.add run records in
                 go (at + n)
               else Ok ()
             in
             let%bind.Base.Result () = go from in
             Quiver_innodb.Tablespace.close run)
           ranges)
    in
    Quiver_innodb.Tablespace.finish space finished
  in
  Quiver_innodb.File.close file;
  Quiver_innodb.File.close scratch;
  let bytes = Bytes.of_string (In_channel.with_open_bin path In_channel.input_all) in
  Sys.remove path;
  Sys.remove (path ^ ".sort");
  Result.map (fun (_ : int) -> (layout, others, bytes)) built

let secondary_build ~runs =
  let rows = List.length (snd (List.hd secondary_rows)) in
  ok (secondary_built secondary_rows ~cuts:(List.init runs (fun r -> r * rows / runs)) ~batch:777 ~threads:8)

let shown_raw = Option.fold ~none:"NULL" ~some:(fun raw -> String.concat "" (List.map (fun c -> Printf.sprintf "%02x" (Char.code c)) (List.of_seq (String.to_seq raw))))

let tests =
  tests
  @ [
      Alcotest.test_case "secondary indexes are written record for record as the server builds them" `Quick (fun () ->
          let server = Bytes.of_string (fixture "secondary-rows.ibd") in
          let _, server_others = secondary_layouts "secondary-rows.cfg" in
          List.iter
            (fun runs ->
              let layout, others, bytes = secondary_build ~runs in
              let () =
                match
                  Quiver_innodb.Check.tablespace layout ~secondary:others ~pages:(Bytes.length bytes / Quiver_innodb.Page.size)
                    ~read:(read bytes)
                with
                | Ok { rows; _ } -> Alcotest.(check int) "rows" 20000 rows
                | Error _ as failed -> Alcotest.fail (Printf.sprintf "%d runs: %s" runs (check_error failed))
              in
              List.iteri
                (fun k ((ours : Quiver_innodb.Layout.t), theirs) ->
                  let raw records = List.map (List.map (fun (_, raw) -> shown_raw raw)) records in
                  let ours = raw (stored ours bytes) and theirs = raw (stored theirs server) in
                  Alcotest.(check int) (Printf.sprintf "%d runs, index %d records" runs k) (List.length theirs) (List.length ours);
                  List.iteri
                    (fun r (ours, theirs) ->
                      if ours <> theirs then
                        Alcotest.failf "%d runs, index %d, record %d: ours %s, the server's %s" runs k r
                          (String.concat "," ours) (String.concat "," theirs))
                    (List.combine ours theirs))
                (List.combine others server_others))
            [ 1; 7 ]);
    ]

(** Rows of [secondary.ibd]'s table from a few values each, so keys repeat, are NULL and
    begin one another; [UNIQUE] values repeat only when [twin] says so. *)
type keyed = { rows : int; seed : int; cuts : int list; batch : int; twin : bool; threads : int } [@@deriving sexp_of]

let keyed_rows { rows; seed; twin; _ } =
  let random = Random.State.make [| seed |] in
  let pick values = values.(Random.State.int random (Array.length values)) in
  let maybe f = if Random.State.int random 4 = 0 then `Null else f () in
  let ids = List.init rows (fun i -> (i * 5) + Random.State.int random 5) in
  let bins =
    List.map
      (fun id ->
        maybe (fun () ->
            let b = Bytes.create 4 in
            Bytes.set_int32_be b 0 (Int32.of_int (id * 7919));
            `Text (Bytes.to_string b)))
      ids
  in
  let bins =
    match (twin, List.filter (fun bin -> bin <> `Null) bins) with
    | true, first :: _ :: _ ->
        let at = Random.State.int random rows in
        List.mapi (fun i bin -> if i = at then first else bin) bins
    | _ -> bins
  in
  [
    (("id", Quiver_core.Ltype.Int64), List.map (fun id -> `Int64 (Int64.of_int id)) ids);
    (("a", Int32), List.map (fun _ -> `Int (pick [| -3; -1; 0; 1; 2; 2147483647; -2147483648 |])) ids);
    (("b", Uint32), List.map (fun _ -> maybe (fun () -> `Int (pick [| 0; 1; 5; 4294967295 |]))) ids);
    ( ("d", Datetime (fsp 2)),
      List.map (fun _ -> `Int64 (Int64.of_int (pick [| 0; 10_000; 86_400_000_000; 1_577_836_800_990_000 |]))) ids );
    (("m", decimal 10 3), List.map (fun _ -> maybe (fun () -> `Text (pick [| "-1.000"; "0.000"; "1.500"; "-0.001"; "1234567.999" |]))) ids);
    (("bin", Bytes), bins);
    ( ("vb", Bytes),
      List.map
        (fun _ ->
          maybe (fun () -> `Text (String.init (Random.State.int random 15) (fun _ -> pick [| '\000'; '\001'; '\255' |]))))
        ids );
    (("s", String), List.map (fun _ -> `Text (String.make (Random.State.int random 5) 'x')) ids);
  ]

let tests =
  tests
  @ [
      QCheck_alcotest.to_alcotest
        (QCheck2.Test.make ~count:60
           ~name:"secondary indexes hold every row's key, in the order a plain sort gives, and refuse a repeated unique key"
           ~print:(fun keyed -> Sexplib0.Sexp.to_string (sexp_of_keyed keyed))
           QCheck2.Gen.(
             let* rows = int_range 0 2500 in
             let+ seed = int
             and+ cuts = list_size (int_range 0 6) (int_range 0 rows)
             and+ batch = int_range 1 600
             and+ twin = map (fun k -> k = 0) (int_range 0 4)
             and+ threads = int_range 1 9 in
             { rows; seed; cuts; batch; twin; threads })
           (fun keyed ->
             let typed = keyed_rows keyed in
             let bins = List.filter (fun bin -> bin <> `Null) (List.assoc ("bin", Quiver_core.Ltype.Bytes) typed) in
             let repeated = List.length (List.sort_uniq compare bins) < List.length bins in
             match (secondary_built typed ~cuts:keyed.cuts ~batch:keyed.batch ~threads:keyed.threads, repeated) with
             | Error (`Tablespace (Quiver_innodb.Tablespace.Duplicate _)), true -> true
             | Error _, _ -> QCheck2.Test.fail_reportf "the build failed"
             | Ok (layout, others, bytes), _ -> (
                 match
                   ( Quiver_innodb.Check.tablespace layout ~secondary:others
                       ~pages:(Bytes.length bytes / Quiver_innodb.Page.size) ~read:(read bytes),
                     repeated )
                 with
                 | Error (`Check (Quiver_innodb.Check.Duplicate _)), true -> true
                 | Error _, _ -> QCheck2.Test.fail_reportf "check: %s" (check_error (Quiver_innodb.Check.tablespace layout ~secondary:others ~pages:(Bytes.length bytes / Quiver_innodb.Page.size) ~read:(read bytes)))
                 | Ok _, true -> QCheck2.Test.fail_reportf "a repeated unique key passed"
                 | Ok { rows; _ }, false ->
                     let clustered = stored layout bytes in
                     rows = keyed.rows
                     && List.for_all
                          (fun (index : Quiver_innodb.Layout.t) ->
                            let projected =
                              List.map
                                (fun record ->
                                  List.map
                                    (fun (field : Quiver_innodb.Layout.field) ->
                                      let raw =
                                        List.find_map
                                          (fun ((stored : Quiver_innodb.Layout.field), raw) ->
                                            if stored.source = field.source then Some raw else None)
                                          record
                                      in
                                      match (Option.join raw, field.part) with
                                      | Some raw, Prefix n -> Some (String.sub raw 0 (Int.min n (String.length raw)))
                                      | raw, (Prefix _ | Whole) -> raw)
                                    index.fields)
                                clustered
                            in
                            let model = List.sort compare projected in
                            let ours = List.map (List.map snd) (stored index bytes) in
                            ours = model || QCheck2.Test.fail_reportf "index %d differs from the sorted rows" index.index)
                          others)));
    ]

let origins file n =
  let at = n * Quiver_innodb.Page.size in
  let rec go origin acc =
    let next = (origin + Bytes.get_uint16_be file (at + origin - 2)) land 0xFFFF in
    if next = 112 then List.rev acc else go next (next :: acc)
  in
  go 99 []

let last_leaf file id =
  let size = Quiver_innodb.Page.size in
  Option.get
    (List.find_opt
       (fun n ->
         let at = n * size in
         Bytes.get_uint16_be file (at + 24) = 17855
         && Int64.to_int (Bytes.get_int64_be file (at + 66)) = id
         && Bytes.get_uint16_be file (at + 64) = 0
         && Bytes.get_int32_be file (at + 12) = -1l
         && List.length (origins file n) > 2)
       (List.init (Bytes.length file / size) Fun.id))

let tests =
  tests
  @ [
      Alcotest.test_case "a secondary index out of order or with a repeated unique key fails the check" `Quick (fun () ->
          let layout, others, bytes = secondary_build ~runs:1 in
          let index name = List.nth others (match name with `Unique -> 0 | `Plain -> 1) in
          let broken name change =
            let file = Bytes.copy bytes in
            let n = last_leaf file (index name).index in
            change file (n * Quiver_innodb.Page.size) (origins file n);
            reseal file n;
            Quiver_innodb.Check.tablespace layout ~secondary:others ~pages:(Bytes.length file / Quiver_innodb.Page.size)
              ~read:(read file)
          in
          let () =
            match
              broken `Plain (fun file at origins -> Bytes.set_uint8 file (at + List.nth origins 1) 0xFF)
            with
            | Error (`Check (Quiver_innodb.Check.Order _)) -> ()
            | failed -> Alcotest.fail ("a key out of order: " ^ check_error failed)
          in
          match
            broken `Unique (fun file at origins ->
                let id origin = Bytes.get_int64_be file (at + origin + 4) in
                let rec pair = function
                  | a :: (b :: _ as rest) -> if Int64.compare (id a) (id b) < 0 then (a, b) else pair rest
                  | [ _ ] | [] -> Alcotest.fail "no pair to repeat"
                in
                let first, second = pair origins in
                Bytes.blit file (at + first) file (at + second) 4)
          with
          | Error (`Check (Quiver_innodb.Check.Duplicate _)) -> ()
          | failed -> Alcotest.fail ("a repeated unique key: " ^ check_error failed));
    ]

let tests =
  tests
  @ [
      Alcotest.test_case "a table InnoDB clusters by a unique key, having no primary key, is not built" `Quick (fun () ->
          match
            Quiver_innodb.Layout.of_cfg
              (ok (Quiver_innodb.Cfg.parse (fixture "unique.cfg")))
              ~collations
              [ ("code", Quiver_core.Ltype.Int32); ("v", String) ]
          with
          | Error (`Layout (Quiver_innodb.Layout.Clustered_by "uc")) -> ()
          | Error (`Layout error) -> Alcotest.fail (Sexplib0.Sexp.to_string (Quiver_innodb.Layout.sexp_of_error error))
          | Ok _ -> Alcotest.fail "built");
    ]
