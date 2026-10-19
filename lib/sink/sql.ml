(** Whether an [INSERT] is open in the output and where it started. *)
type statement = Closed | Open of int

let escapes =
  Escape.make (function
    | '\000' -> Some "\\0"
    | '\'' -> Some "\\'"
    | '"' -> Some "\\\""
    | '\b' -> Some "\\b"
    | '\n' -> Some "\\n"
    | '\r' -> Some "\\r"
    | '\t' -> Some "\\t"
    | '\026' -> Some "\\Z"
    | '\\' -> Some "\\\\"
    | _ -> None)

module Insert = struct
  type settings = int

  type lines = {
    table : Quiver_core.Schema.Table.t;
    fields : Quiver_core.Schema.Field.t iarray;
    insert : string;
    limit : int;
  }

  let extension = "sql"
  let format _ = Quiver_core.Output_format.Sql

  let lines limit
      ({ name = { table = name; _ }; fields = first, rest; _ } as table :
        Quiver_core.Schema.Table.t) =
    let columns =
      List.map (fun { Quiver_core.Schema.Field.name; _ } -> Quiver_core.Ident.backquoted name) (first :: rest)
    in
    {
      table;
      fields = Iarray.of_list (first :: rest);
      insert =
        "INSERT INTO " ^ Quiver_core.Ident.backquoted name ^ " (" ^ String.concat "," columns ^ ") VALUES\n";
      limit;
    }

  let preamble _ _ =
    Some (fun (out : Quiver_core.Byte_buffer.t @ local) ->
        Quiver_core.Byte_buffer.append_string out Layout.header)

  let write lines (batch : Quiver_core.Batch.sealed @ local)
      (out : Quiver_core.Byte_buffer.t @ local) =
    let columns = Quiver_core.Batch.sealed_columns batch in
    let quoted j result =
      match result with
      | Ok () ->
          Quiver_core.Byte_buffer.add_char out '\'';
          Ok ()
      | Error `Unrepresentable_date ->
          Error (`Unrepresentable_date (lines.table.name, (Iarray.get lines.fields j).name))
    in
    let value row j (Quiver_core.Column.Any column) =
      match Quiver_core.Column.presence column row with
      | Null ->
          Quiver_core.Byte_buffer.append_string out "NULL";
          Ok ()
      | Present -> (
          match (Quiver_core.Column.repr column, (Iarray.get lines.fields j).ltype) with
          | Bool, _ ->
              let digit = if Quiver_core.Column.get column row then '1' else '0' in
              Quiver_core.Byte_buffer.add_char out digit;
              Ok ()
          | Int, Date ->
              Quiver_core.Byte_buffer.add_char out '\'';
              quoted j (Quiver_core.Temporal.date out ~days:(Quiver_core.Column.get column row))
          | Int, Unchecked_date ->
              Quiver_core.Byte_buffer.add_char out '\'';
              Quiver_core.Temporal.unchecked_date out (Quiver_core.Column.get column row);
              Quiver_core.Byte_buffer.add_char out '\'';
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
              Quiver_core.Byte_buffer.add_char out '\'';
              Quiver_core.Temporal.time out fsp ~micros:(Quiver_core.Column.get_int64 column row);
              Quiver_core.Byte_buffer.add_char out '\'';
              Ok ()
          | Int64, (Datetime fsp | Timestamp fsp) ->
              Quiver_core.Byte_buffer.add_char out '\'';
              quoted j
                (Quiver_core.Temporal.datetime out fsp
                   ~micros:(Quiver_core.Column.get_int64 column row))
          | Int64, Unchecked_datetime fsp ->
              Quiver_core.Byte_buffer.add_char out '\'';
              Quiver_core.Temporal.unchecked_datetime out fsp
                (Quiver_core.Column.get_int64 column row);
              Quiver_core.Byte_buffer.add_char out '\'';
              Ok ()
          | Int64, _ ->
              Quiver_core.Digits.int64 out (Quiver_core.Column.get_int64 column row);
              Ok ()
          | Float, _ ->
              Render.float out (Quiver_core.Column.get_float column row);
              Ok ()
          | Text, (Bytes | Geometry) ->
              Quiver_core.Byte_buffer.append_string out "X'";
              Render.hex out column row;
              Quiver_core.Byte_buffer.add_char out '\'';
              Ok ()
          | Text, Decimal _ ->
              Render.text out column row;
              Ok ()
          | Text, _ ->
              Quiver_core.Byte_buffer.add_char out '\'';
              Escape.add escapes column row out;
              Quiver_core.Byte_buffer.add_char out '\'';
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
    let rec rows row statement =
      if row = count then
        let () =
          match statement with
          | Open _ -> Quiver_core.Byte_buffer.append_string out ";\n"
          | Closed -> ()
        in
        Ok ()
      else
        let start =
          match statement with
          | Open start ->
              Quiver_core.Byte_buffer.append_string out ",\n";
              start
          | Closed ->
              let start = Quiver_core.Byte_buffer.length out in
              Quiver_core.Byte_buffer.append_string out lines.insert;
              start
        in
        Quiver_core.Byte_buffer.add_char out '(';
        match cells row 0 with
        | Error error -> Error error
        | Ok () ->
            Quiver_core.Byte_buffer.add_char out ')';
            if Quiver_core.Byte_buffer.length out - start >= lines.limit then
              let () = Quiver_core.Byte_buffer.append_string out ";\n" in
              rows (row + 1) Closed
            else
              let next =
                match statement with
                | Open _ -> statement
                | Closed -> Open start
              in
              rows (row + 1) next
    in
    rows 0 Closed [@nontail]
end

include Files.Make (Insert)

let create layout ~statement_bytes = create layout statement_bytes
