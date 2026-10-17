open Base.Result.Let_syntax
(** One cell of a binary row as the server would send it. *)

type value =
  | Integer of Quiver_mysql.Rows.width * Quiver_mysql.Rows.sign * int
  | Big of int64
  | Single of int32
  | Double of int64
  | Day of (int * int * int)
  | Instant of (int * int * int) * (int * int * int) * int
  | Duration of {
      negative : bool;
      days : int;
      clock : int * int * int;
      micros : int;
    }
  | Bitfield of string
  | Text of string

let ltype : value -> Quiver_core.Ltype.t = function
  | Integer (W1, Signed, _) -> Int8
  | Integer (W1, Unsigned, _) -> Uint8
  | Integer (W2, Signed, _) -> Int16
  | Integer (W2, Unsigned, _) -> Uint16
  | Integer ((W4 | W8), Signed, _) -> Int32
  | Integer ((W4 | W8), Unsigned, _) -> Uint32
  | Big _ -> Int64
  | Single _ -> Float32
  | Double _ -> Float64
  | Day _ -> Date
  | Instant _ -> Datetime Quiver_core.Ltype.Fsp.microseconds
  | Duration _ -> Time Quiver_core.Ltype.Fsp.microseconds
  | Bitfield _ -> Uint64
  | Text _ -> String

let encode cells =
  let out = Buffer.create 256 in
  Buffer.add_char out '\x00';
  let bitmap = Bytes.make ((List.length cells + 9) / 8) '\x00' in
  List.iteri
    (fun j cell ->
      if Option.is_none cell then
        let bit = j + 2 in
        Bytes.set bitmap (bit lsr 3)
          (Char.chr
             (Char.code (Bytes.get bitmap (bit lsr 3)) lor (1 lsl (bit land 7)))))
    cells;
  Buffer.add_bytes out bitmap;
  let lenenc length =
    if length < 251 then Buffer.add_uint8 out length
    else if length < 0x10000 then (
      Buffer.add_uint8 out 0xFC;
      Buffer.add_uint16_le out length)
    else (
      Buffer.add_uint8 out 0xFD;
      Buffer.add_uint16_le out (length land 0xFFFF);
      Buffer.add_uint8 out (length lsr 16))
  in
  let date (year, month, day) =
    Buffer.add_uint16_le out year;
    Buffer.add_uint8 out month;
    Buffer.add_uint8 out day
  and clock (hours, minutes, seconds) =
    Buffer.add_uint8 out hours;
    Buffer.add_uint8 out minutes;
    Buffer.add_uint8 out seconds
  in
  List.iter
    (Option.iter (function
      | Integer (W1, _, n) -> Buffer.add_uint8 out (n land 0xFF)
      | Integer (W2, _, n) -> Buffer.add_uint16_le out (n land 0xFFFF)
      | Integer (W4, _, n) -> Buffer.add_int32_le out (Int32.of_int n)
      | Integer (W8, _, n) -> Buffer.add_int64_le out (Int64.of_int n)
      | Big n | Double n -> Buffer.add_int64_le out n
      | Single n -> Buffer.add_int32_le out n
      | Day ymd ->
          Buffer.add_uint8 out 4;
          date ymd
      | Instant (ymd, hms, micros) -> (
          match (hms, micros) with
          | (0, 0, 0), 0 ->
              Buffer.add_uint8 out 4;
              date ymd
          | _, 0 ->
              Buffer.add_uint8 out 7;
              date ymd;
              clock hms
          | _ ->
              Buffer.add_uint8 out 11;
              date ymd;
              clock hms;
              Buffer.add_int32_le out (Int32.of_int micros))
      | Duration { negative; days; clock = hms; micros } -> (
          match (negative, days, hms, micros) with
          | false, 0, (0, 0, 0), 0 -> Buffer.add_uint8 out 0
          | _ ->
              Buffer.add_uint8 out (if micros = 0 then 8 else 12);
              Buffer.add_uint8 out (if negative then 1 else 0);
              Buffer.add_int32_le out (Int32.of_int days);
              clock hms;
              if micros <> 0 then Buffer.add_int32_le out (Int32.of_int micros))
      | Bitfield bytes | Text bytes ->
          lenenc (String.length bytes);
          Buffer.add_string out bytes))
    cells;
  Buffer.contents out

let days (year, month, day) =
  Option.map
    (fun moment -> fst (Ptime.Span.to_d_ps (Ptime.to_span moment)))
    (Ptime.of_date (year, month, day))

let micros_of_clock (hours, minutes, seconds) micros =
  (((((hours * 60) + minutes) * 60) + seconds) * 1_000_000) + micros

let value =
  QCheck2.Gen.(
    let ymd =
      let* year = int_range 0 9999 and* month = int_range 0 13 in
      let+ day = int_range 0 31 in
      (year, month, day)
    and hms =
      let* hours = int_range 0 23 and* minutes = int_range 0 59 in
      let+ seconds = int_range 0 59 in
      (hours, minutes, seconds)
    and micros = oneof [ pure 0; int_range 0 999_999 ] in
    oneof
      [
        (let* width = oneof_list [ Quiver_mysql.Rows.W1; W2; W4 ]
         and* sign = oneof_list [ Quiver_mysql.Rows.Signed; Unsigned ] in
         let bits = match width with W1 -> 8 | W2 -> 16 | W4 | W8 -> 32 in
         let+ n =
           match sign with
           | Signed -> int_range (-(1 lsl (bits - 1))) ((1 lsl (bits - 1)) - 1)
           | Unsigned -> int_range 0 ((1 lsl bits) - 1)
         in
         Integer (width, sign, n));
        map (fun n -> Big n) int64;
        map
          (fun n -> Single n)
          (map
             (fun bits ->
               if Float.is_finite (Int32.float_of_bits bits) then bits else 0l)
             int32);
        map (fun n -> Double n) int64;
        map (fun d -> Day d) ymd;
        (let* d = ymd and* c = hms in
         let+ m = micros in
         Instant (d, c, m));
        (let* negative = bool and* days = int_range 0 34 and* c = hms in
         let+ m = micros in
         Duration { negative; days; clock = c; micros = m });
        map (fun s -> Bitfield s) (string_size ~gen:char (int_range 1 8));
        map
          (fun s -> Text s)
          (string_size ~gen:char
             (oneof
                [
                  int_range 0 300;
                  pure 250;
                  pure 251;
                  pure 65535;
                  pure 65536;
                  pure 70000;
                ]));
      ])

let print cells =
  String.concat "; "
    (List.map
       (function
         | None -> "NULL"
         | Some (Integer (w, sign, n)) ->
             Printf.sprintf "int(%s,%s,%d)"
               (match w with W1 -> "1" | W2 -> "2" | W4 -> "4" | W8 -> "8")
               (match sign with Signed -> "s" | Unsigned -> "u")
               n
         | Some (Big n) -> Printf.sprintf "big %Ld" n
         | Some (Single n) -> Printf.sprintf "single %ld" n
         | Some (Double n) -> Printf.sprintf "double %Ld" n
         | Some (Day (y, m, d)) -> Printf.sprintf "day %d-%d-%d" y m d
         | Some (Instant ((y, m, d), (h, mi, s), us)) ->
             Printf.sprintf "instant %d-%d-%d %d:%d:%d.%d" y m d h mi s us
         | Some (Duration { negative; days; clock = h, mi, s; micros }) ->
             Printf.sprintf "duration %b %d %d:%d:%d.%d" negative days h mi s
               micros
         | Some (Bitfield b) -> Printf.sprintf "bits %S" b
         | Some (Text t) -> Printf.sprintf "text(%d)" (String.length t))
       cells)

let tests =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:1000 ~name:"binary rows decode into columns"
         ~print
         QCheck2.Gen.(list_size (int_range 1 12) (option value))
         (fun cells ->
           let wire =
             Quiver_mysql.Wire.create
               (Test_wire.source (Test_wire.frames [ encode cells ]) [])
           and cursor = Quiver_mysql.Wire.Cursor.create ()
           and columns =
             List.map
               (fun cell ->
                 Quiver_core.Column.create
                   (Option.fold ~none:Quiver_core.Ltype.String ~some:ltype cell))
               cells
           in
           let decoded =
             let%bind () = Quiver_mysql.Wire.next wire cursor in
             Quiver_mysql.Wire.Cursor.decode cursor (fun reader ->
                 Quiver_mysql.Wire.Cursor.skip reader 1;
                 let bitmap =
                   Quiver_mysql.Wire.Cursor.take reader
                     ((List.length cells + 9) / 8)
                 in
                 Base.Result.all
                   (List.map
                      (fun (j, (cell, Quiver_core.Column.Any column)) ->
                        match
                          (Quiver_mysql.Rows.presence reader ~bitmap j, cell)
                        with
                        | Null, None ->
                            Quiver_core.Column.add_null column;
                            Ok `Stored
                        | Present, Some cell -> (
                            let stored = function
                              | Ok () -> Ok `Stored
                              | Error `Invalid_date -> Ok `Invalid_date
                              | Error (`Column _) ->
                                  Error (`Wire Quiver_mysql.Wire.Malformed)
                            in
                            match (cell, Quiver_core.Column.repr column) with
                            | Integer (width, sign, _), Int ->
                                stored
                                  (Quiver_mysql.Rows.add_integer reader width
                                     sign column)
                            | Big _, Int64 ->
                                stored
                                  (Quiver_mysql.Rows.add_int64 reader column)
                            | Single _, Float ->
                                stored
                                  (Quiver_mysql.Rows.add_float32 reader column)
                            | Double _, Float ->
                                stored
                                  (Quiver_mysql.Rows.add_float64 reader column)
                            | Day _, Int ->
                                stored
                                  (Quiver_mysql.Rows.add_date reader column)
                            | Instant _, Int64 ->
                                stored
                                  (Quiver_mysql.Rows.add_datetime reader column)
                            | Duration _, Int64 ->
                                stored
                                  (Quiver_mysql.Rows.add_time reader column)
                            | Bitfield _, Int64 ->
                                stored
                                  (Quiver_mysql.Rows.add_bits reader column)
                            | Text _, Text ->
                                stored
                                  (Quiver_mysql.Rows.add_text reader column)
                            | _ -> Error (`Wire Quiver_mysql.Wire.Malformed))
                        | (Null | Present), _ ->
                            Error (`Wire Quiver_mysql.Wire.Malformed))
                      (List.mapi
                         (fun j pair -> (j, pair))
                         (List.combine cells columns))))
           in
           match decoded with
           | Error _ -> false
           | Ok outcomes ->
               List.for_all2
                 (fun (cell, outcome) (Quiver_core.Column.Any column) ->
                   match (cell, outcome) with
                   | None, _ -> Quiver_core.Column.presence column 0 = Null
                   | Some cell, `Invalid_date -> (
                       match cell with
                       | Day ymd | Instant (ymd, _, _) ->
                           Option.is_none (days ymd)
                       | _ -> false)
                   | Some cell, `Stored -> (
                       match (cell, Quiver_core.Column.repr column) with
                       | Integer (_, _, n), Int ->
                           Quiver_core.Column.get column 0 = n
                       | Big n, Int64 ->
                           Int64.equal
                             (Int64_u.to_int64
                                (Quiver_core.Column.get_int64 column 0))
                             n
                       | Single n, Float ->
                           Int32.equal
                             (Int32.bits_of_float
                                (Float_u.to_float
                                   (Quiver_core.Column.get_float column 0)))
                             n
                       | Double n, Float ->
                           Int64.equal
                             (Int64.bits_of_float
                                (Float_u.to_float
                                   (Quiver_core.Column.get_float column 0)))
                             n
                       | Day ymd, Int ->
                           days ymd = Some (Quiver_core.Column.get column 0)
                       | Instant (ymd, hms, micros), Int64 ->
                           Option.map
                             (fun d ->
                               Int64.of_int
                                 ((d * 86_400_000_000)
                                + micros_of_clock hms micros))
                             (days ymd)
                           = Some
                               (Int64_u.to_int64
                                  (Quiver_core.Column.get_int64 column 0))
                       | Duration { negative; days; clock; micros }, Int64 ->
                           let magnitude =
                             (days * 86_400_000_000)
                             + micros_of_clock clock micros
                           in
                           Int64.equal
                             (Int64_u.to_int64
                                (Quiver_core.Column.get_int64 column 0))
                             (Int64.of_int
                                (if negative then -magnitude else magnitude))
                       | Bitfield bytes, Int64 ->
                           Int64.equal
                             (Int64_u.to_int64
                                (Quiver_core.Column.get_int64 column 0))
                             (String.fold_left
                                (fun acc c ->
                                  Int64.logor (Int64.shift_left acc 8)
                                    (Int64.of_int (Char.code c)))
                                0L bytes)
                       | Text text, Text ->
                           String.equal (Quiver_core.Column.get column 0) text
                       | _ -> false))
                 (List.combine cells outcomes)
                 columns));
  ]
