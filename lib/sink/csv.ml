let rfc4180_escapes = Escape.make (function '"' -> Some "\"\"" | _ -> None)

let load_data_escapes =
  Escape.make (function
    | '\\' -> Some "\\\\"
    | '"' -> Some "\\\""
    | '\000' -> Some "\\0"
    | '\n' -> Some "\\n"
    | '\r' -> Some "\\r"
    | '\t' -> Some "\\t"
    | '\026' -> Some "\\Z"
    | _ -> None)

let newline : Quiver_core.Output_format.dialect -> string = function
  | Rfc4180 -> "\r\n"
  | Mysql_load_data -> "\n"

module Dialect = struct
  type settings = Quiver_core.Output_format.dialect

  type lines = {
    escapes : Escape.t;
    newline : string;
    table : Quiver_core.Schema.Table.t;
    fields : Quiver_core.Schema.Field.t iarray;
  }

  let extension = "csv"
  let format dialect = Quiver_core.Output_format.Csv dialect

  let lines (dialect : settings)
      ({ fields = first, rest; _ } as table : Quiver_core.Schema.Table.t) =
    let escapes =
      match dialect with Rfc4180 -> rfc4180_escapes | Mysql_load_data -> load_data_escapes
    in
    { escapes; newline = newline dialect; table; fields = Iarray.of_list (first :: rest) }

  let preamble (dialect : settings)
      ({ fields = first, rest; _ } : Quiver_core.Schema.Table.t) =
    match dialect with
    | Mysql_load_data -> None
    | Rfc4180 ->
        Some (fun (out : Quiver_core.Byte_buffer.t @ local) ->
            Base.List.iteri (first :: rest) ~f:(fun j ({ name; _ } : Quiver_core.Schema.Field.t) ->
                if j > 0 then Quiver_core.Byte_buffer.add_char out ',';
                Quiver_core.Byte_buffer.add_char out '"';
                Escape.add_string rfc4180_escapes (name :> string) out;
                Quiver_core.Byte_buffer.add_char out '"');
            Quiver_core.Byte_buffer.append_string out (newline Rfc4180))

  let write lines (batch : Quiver_core.Batch.sealed @ local)
      (out : Quiver_core.Byte_buffer.t @ local) =
    let columns = Quiver_core.Batch.sealed_columns batch in
    let checked j result =
      match result with
      | Ok () -> Ok ()
      | Error `Unrepresentable_date ->
          Error (`Unrepresentable_date (lines.table.name, (Iarray.get lines.fields j).name))
    in
    let value row j (Quiver_core.Column.Any column) =
      match Quiver_core.Column.presence column row with
      | Null ->
          Quiver_core.Byte_buffer.append_string out "\\N";
          Ok ()
      | Present -> (
          match (Quiver_core.Column.repr column, (Iarray.get lines.fields j).ltype) with
          | Bool, _ ->
              let digit = if Quiver_core.Column.get column row then '1' else '0' in
              Quiver_core.Byte_buffer.add_char out digit;
              Ok ()
          | Int, Date ->
              checked j (Quiver_core.Temporal.date out ~days:(Quiver_core.Column.get column row))
          | Int, Unchecked_date ->
              Quiver_core.Temporal.unchecked_date out (Quiver_core.Column.get column row);
              Ok ()
          | Int, Year ->
              Quiver_core.Digits.padded out ~width:4 (Quiver_core.Column.get column row);
              Ok ()
          | Int, _ ->
              Quiver_core.Digits.int out (Quiver_core.Column.get column row);
              Ok ()
          | Int64, Uint64 ->
              Quiver_core.Digits.uint64 out (Quiver_core.Column.get_int64 column row);
              Ok ()
          | Int64, Time fsp ->
              Quiver_core.Temporal.time out fsp ~micros:(Quiver_core.Column.get_int64 column row);
              Ok ()
          | Int64, (Datetime fsp | Timestamp fsp) ->
              checked j
                (Quiver_core.Temporal.datetime out fsp
                   ~micros:(Quiver_core.Column.get_int64 column row))
          | Int64, Unchecked_datetime fsp ->
              Quiver_core.Temporal.unchecked_datetime out fsp
                (Quiver_core.Column.get_int64 column row);
              Ok ()
          | Int64, _ ->
              Quiver_core.Digits.int64 out (Quiver_core.Column.get_int64 column row);
              Ok ()
          | Float, _ ->
              Render.float out (Quiver_core.Column.get_float column row);
              Ok ()
          | Text, (Bytes | Geometry) ->
              Render.hex out column row;
              Ok ()
          | Text, Decimal _ ->
              Render.text out column row;
              Ok ()
          | Text, _ ->
              Quiver_core.Byte_buffer.add_char out '"';
              Escape.add lines.escapes column row out;
              Quiver_core.Byte_buffer.add_char out '"';
              Ok ())
    in
    let width = Iarray.length columns and count = Quiver_core.Batch.sealed_rows batch in
    let rec cells row j =
      if j = width then Ok ()
      else
        let () = if j > 0 then Quiver_core.Byte_buffer.add_char out ',' in
        match value row j (Iarray.get columns j) with
        | Ok () -> cells row (j + 1)
        | Error error -> Error error
    in
    let rec rows row =
      if row = count then Ok ()
      else
        match cells row 0 with
        | Error error -> Error error
        | Ok () ->
            Quiver_core.Byte_buffer.append_string out lines.newline;
            rows (row + 1)
    in
    rows 0 [@nontail]
end

include Files.Make (Dialect)
