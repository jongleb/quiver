open Base.Result.Let_syntax

type width = W1 | W2 | W4 | W8 [@@deriving sexp_of ~portable]
type sign = Signed | Unsigned [@@deriving sexp_of ~portable]

type kind =
  | Integer of { width : width; sign : sign }
  | Float32
  | Float64
  | Date
  | Datetime
  | Time
  | Bits
  | Text
[@@deriving sexp_of ~portable]

(** Whether the server still has rows of the result to send. *)
type state = Streaming | Finished of { warnings : int }

type t = {
  link : Link.t;
  statement : int;
  kinds : kind array;
  bitmap : int;  (** Bytes of the NULL bitmap of a row. *)
  mutable state : state;
}

let null_bitmap_offset = 2
let date_bytes = 4
let with_clock_bytes = 7
let with_micros_bytes = 11

let statement_id command id =
  let out = Buffer.create 10 in
  Buffer.add_string out (Wire.command command);
  Buffer.add_int32_le out (Int32.of_int id);
  out

let open_ link sql =
  let%bind () = Link.command link (Wire.command Prepare ^ sql) in
  let%bind statement, columns, parameters =
    Link.receive_on link (fun reader ->
        let first = Wire.Cursor.uint8 reader in
        match Wire.marker first with
        | Ok_packet ->
            let statement = Wire.Cursor.uint32 reader in
            let columns = Wire.Cursor.uint16 reader in
            Ok (statement, columns, Wire.Cursor.uint16 reader)
        | Err_packet -> Error (Link.refused reader)
        | Auth_more_data | Local_infile | Eof_packet | Data ->
            Error (`Link (Link.Unexpected_packet first)))
  in
  let rec definitions count kinds =
    if count = 0 then Ok (Array.of_list (List.rev kinds))
    else
      let%bind kind =
        Link.receive_on link (fun reader ->
            let names =
              [ `Catalog; `Schema; `Table; `Org_table; `Name; `Org_name ]
            and charset_and_length = 6
            and unsigned_flag = 0x20 in
            List.iter
              (fun _ -> Wire.Cursor.skip reader (Wire.Cursor.lenenc_int reader))
              names;
            ignore (Wire.Cursor.lenenc_int reader);
            Wire.Cursor.skip reader charset_and_length;
            let field_type =
              match Wire.Cursor.uint8 reader with
              | 1 -> `Tiny
              | 2 -> `Short
              | 3 -> `Long
              | 4 -> `Float
              | 5 -> `Double
              | 7 -> `Timestamp
              | 8 -> `Longlong
              | 9 -> `Int24
              | 10 -> `Date
              | 11 -> `Time
              | 12 -> `Datetime
              | 13 -> `Year
              | 14 -> `Newdate
              | 16 -> `Bit
              | _ -> `Length_encoded
            in
            let sign =
              match Wire.Cursor.uint16 reader land unsigned_flag with
              | 0 -> Signed
              | _ -> Unsigned
            in
            let kind =
              match field_type with
              | `Tiny -> Integer { width = W1; sign }
              | `Short | `Year -> Integer { width = W2; sign }
              | `Long | `Int24 -> Integer { width = W4; sign }
              | `Longlong -> Integer { width = W8; sign }
              | `Float -> Float32
              | `Double -> Float64
              | `Date | `Newdate -> Date
              | `Timestamp | `Datetime -> Datetime
              | `Time -> Time
              | `Bit -> Bits
              | `Length_encoded -> Text
            in
            Ok kind)
      in
      definitions (count - 1) (kind :: kinds)
  in
  let%bind _ = definitions parameters [] in
  let%bind kinds = definitions columns [] in
  let execute = statement_id Execute statement in
  let no_cursor = 0 and iterations = 1l in
  Buffer.add_uint8 execute no_cursor;
  Buffer.add_int32_le execute iterations;
  let%bind () = Link.command link (Buffer.contents execute) in
  let%bind count =
    Link.receive_on link (fun reader ->
        match Wire.marker (Wire.Cursor.peek reader) with
        | Err_packet ->
            Wire.Cursor.skip reader 1;
            Error (Link.refused reader)
        | Ok_packet | Auth_more_data | Local_infile | Eof_packet | Data ->
            Ok (Wire.Cursor.lenenc_int reader))
  in
  if count <> Array.length kinds then
    Error (`Link (Link.Unexpected_packet count))
  else
    let%map _ = definitions count [] in
    {
      link;
      statement;
      kinds;
      bitmap = (count + null_bitmap_offset + 7) / 8;
      state = Streaming;
    }

let width t = Array.length t.kinds
let warnings t = match t.state with Streaming -> 0 | Finished { warnings } -> warnings
let kind t j = t.kinds.(j)

let next t ~row =
  let step reader =
    let first = Wire.Cursor.peek reader in
    if first = Wire.ok_byte then (
      Wire.Cursor.skip reader 1;
      match row reader ~bitmap:(Wire.Cursor.take reader t.bitmap) with
      | Ok () -> Ok Quiver_core.Batch_writer.Row
      | Error error -> Error error)
    else if
      first = Wire.eof_byte && Wire.Cursor.remaining reader < Wire.frame_limit
    then (
      Wire.Cursor.skip reader 1;
      let (_ : int) = Wire.Cursor.lenenc_int reader in
      let (_ : int) = Wire.Cursor.lenenc_int reader in
      let status_bytes = 2 in
      Wire.Cursor.skip reader status_bytes;
      t.state <- Finished { warnings = Wire.Cursor.uint16 reader };
      Ok Quiver_core.Batch_writer.End)
    else if first = Wire.err_byte then (
      Wire.Cursor.skip reader 1;
      t.state <- Finished { warnings = 0 };
      Error (Link.refused reader))
    else Error (`Link (Link.Unexpected_packet first))
  in
  match t.state with
  | Finished _ -> Ok Quiver_core.Batch_writer.End
  | Streaming -> Link.receive_on t.link step [@nontail]

let close t =
  let rec drain () =
    match next t ~row:(fun _ ~bitmap:_ -> Ok ()) with
    | Ok Quiver_core.Batch_writer.Row -> drain ()
    | Ok End -> Ok ()
    | Error error -> Error error
  in
  let%bind () = drain () in
  Link.command t.link
    (Buffer.contents (statement_id Close_statement t.statement))

let[@inline] [@zero_alloc] presence reader ~bitmap j :
    Quiver_core.Column.presence =
  let bit = j + null_bitmap_offset in
  if
    Base_bigstring.unsafe_get_uint8
      (Wire.Cursor.buffer reader)
      ~pos:(bitmap + (bit lsr 3))
    land (1 lsl (bit land 7))
    = 0
  then Present
  else Null

let[@inline] [@zero_alloc] add_integer reader width sign column =
  let value =
    match (width, sign) with
    | W1, Signed -> (Wire.Cursor.uint8 reader lxor 0x80) - 0x80
    | W1, Unsigned -> Wire.Cursor.uint8 reader
    | W2, Signed -> (Wire.Cursor.uint16 reader lxor 0x8000) - 0x8000
    | W2, Unsigned -> Wire.Cursor.uint16 reader
    | W4, Signed -> (Wire.Cursor.uint32 reader lxor 0x8000_0000) - 0x8000_0000
    | W4, Unsigned -> Wire.Cursor.uint32 reader
    | W8, (Signed | Unsigned) -> Int64.to_int (Int64_u.to_int64 (Wire.Cursor.int64 reader))
  in
  Quiver_core.Column.add column value

let[@inline] [@zero_alloc] add_int64 reader column =
  Quiver_core.Column.add_int64 column (Wire.Cursor.int64 reader)

let[@zero_alloc] add_bits reader column =
  let length = Wire.Cursor.lenenc_int reader in
  let start = Wire.Cursor.take reader length
  and buffer = Wire.Cursor.buffer reader in
  let rec fold acc k =
    if k = length then acc
    else
      fold
        (Int64_u.bit_or
           (Int64_u.shift_left acc 8)
           (Int64_u.of_int
              (Base_bigstring.unsafe_get_uint8 buffer ~pos:(start + k))))
        (k + 1)
  in
  Quiver_core.Column.add_int64 column (fold #0L 0)

let[@inline] [@zero_alloc] add_float32 reader column =
  Quiver_core.Column.add_float column
    (Float_u.of_float
       (Int32.float_of_bits (Int32.of_int (Wire.Cursor.uint32 reader))))

let[@inline] [@zero_alloc] add_float64 reader column =
  Quiver_core.Column.add_float column
    (Int64_u.float_of_bits (Wire.Cursor.int64 reader))

let[@inline] [@zero_alloc] day reader =
  let year = Wire.Cursor.uint16 reader in
  let month = Wire.Cursor.uint8 reader in
  Quiver_core.Temporal.days_of_date ~year ~month ~day:(Wire.Cursor.uint8 reader)

let[@zero_alloc] add_date reader column =
  match Wire.Cursor.uint8 reader with
  | 0 -> Error `Invalid_date
  | length -> (
      let days = day reader in
      Wire.Cursor.skip reader (length - date_bytes);
      match days with
      | Null -> Error `Invalid_date
      | This days -> Quiver_core.Column.add column days)

let[@inline] [@zero_alloc] clock reader length =
  if length < with_clock_bytes then 0
  else
    let hours = Wire.Cursor.uint8 reader in
    let minutes = Wire.Cursor.uint8 reader in
    let seconds = Wire.Cursor.uint8 reader in
    ((((hours * 60) + minutes) * 60) + seconds)
    * Quiver_core.Moment.micros_per_second
    + if length = with_micros_bytes then Wire.Cursor.uint32 reader else 0

let[@zero_alloc] add_datetime reader column =
  match Wire.Cursor.uint8 reader with
  | 0 -> Error `Invalid_date
  | length -> (
      let days = day reader in
      let clock = clock reader length in
      match days with
      | Null -> Error `Invalid_date
      | This days ->
          Quiver_core.Column.add_int64 column
            (Int64_u.of_int
               ((days * Quiver_core.Moment.micros_per_day) + clock)))

let[@zero_alloc] add_unchecked_date reader column =
  match Wire.Cursor.uint8 reader with
  | 0 -> Quiver_core.Column.add column 0
  | length ->
      let year = Wire.Cursor.uint16 reader in
      let month = Wire.Cursor.uint8 reader in
      let day = Wire.Cursor.uint8 reader in
      Wire.Cursor.skip reader (length - date_bytes);
      if month <= 12 && day <= 31 then
        Quiver_core.Column.add column
          (Quiver_core.Temporal.pack_date ~year ~month ~day)
      else Error `Invalid_date

let[@zero_alloc] add_unchecked_datetime reader column =
  match Wire.Cursor.uint8 reader with
  | 0 -> Quiver_core.Column.add_int64 column #0L
  | length ->
      let year = Wire.Cursor.uint16 reader in
      let month = Wire.Cursor.uint8 reader in
      let day = Wire.Cursor.uint8 reader in
      let micros = clock reader length in
      if
        year <= Quiver_core.Moment.max_year
        && month <= 12 && day <= 31
        && micros < Quiver_core.Moment.micros_per_day
      then
        Quiver_core.Column.add_int64 column
          (Quiver_core.Temporal.pack_datetime ~year ~month ~day ~micros)
      else Error `Invalid_date

let[@zero_alloc] add_time reader column =
  match Wire.Cursor.uint8 reader with
  | 0 -> Quiver_core.Column.add_int64 column #0L
  | length ->
      let negative = Wire.Cursor.uint8 reader in
      let days = Wire.Cursor.uint32 reader in
      let sign_byte = 1 in
      let magnitude =
        (days * Quiver_core.Moment.micros_per_day)
        + clock reader (length - sign_byte)
      in
      let micros = if negative <> 0 then -magnitude else magnitude in
      Quiver_core.Column.add_int64 column (Int64_u.of_int micros)

let[@inline] [@zero_alloc] add_text reader column =
  let length = Wire.Cursor.lenenc_int reader in
  let off = Wire.Cursor.take reader length in
  Quiver_core.Column.add_bigstring column
    (Wire.Cursor.buffer reader)
    ~off ~len:length
