open Base.Result.Let_syntax

type t = { layout : Layout.t; dialect : Quiver_core.Output_format.dialect }

type lines = {
  escapes : Escape.t;
  newline : string;
  table : Quiver_core.Schema.Table.t;
  fields : Quiver_core.Schema.Field.t iarray;
}

type renderer = {
  compression : Compression.t;
  buffers : Quiver_core.Buffer_pool.t;
  lines : lines;
}

type part = { output : Output.t; renderer : renderer; mutable rows : int }

type rendered = { frame : Output.frame; rows : int }

type error =
  | Output of Output.error
  | Unrepresentable_date of {
      table : Quiver_core.Schema.Name.t @@ aliased;
      column : Quiver_core.Ident.t @@ aliased;
    }
  | Unsplittable_definition of Quiver_core.Schema.Name.t
      (** A routine, trigger or view with [";\n"] inside a string literal, which
          myloader would split. *)
[@@deriving sexp_of ~portable]

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

let line_end : Quiver_core.Output_format.dialect -> string = function
  | Rfc4180 -> "\r\n"
  | Mysql_load_data -> "\n"

let sink result =
  Result.map_error (fun (`Output error) -> `Sink (Output error)) result

let create layout dialect = { layout; dialect }
let format t = Quiver_core.Output_format.Csv t.dialect
let database t database ~ddl = sink (Layout.database t.layout database ~ddl)
let table t table ~ddl ~parts:_ = sink (Layout.table t.layout table ~ddl)

let objects t database definitions =
  Result.map_error
    (function
      | `Output error -> `Sink (Output error)
      | `Unsplittable name -> `Sink (Unsplittable_definition name))
    (Layout.objects t.layout database definitions)

let lines dialect
    ({ fields = first, rest; _ } as table : Quiver_core.Schema.Table.t) =
  {
    escapes =
      (match (dialect : Quiver_core.Output_format.dialect) with
      | Rfc4180 -> rfc4180_escapes
      | Mysql_load_data -> load_data_escapes);
    newline = line_end dialect;
    table;
    fields = Iarray.of_list (first :: rest);
  }

let part t ({ fields = first, rest; _ } as table : Quiver_core.Schema.Table.t)
    ~index =
  let compression = Layout.compression t.layout
  and buffers = Layout.buffers t.layout in
  let%bind output = sink (Layout.part t.layout table ~index ~extension:"csv") in
  let%map () =
    match t.dialect with
    | Mysql_load_data -> Ok ()
    | Rfc4180 ->
        let header = Output.start buffers in
        (let out = borrow_ header in
         let rec names j = function
           | [] -> ()
           | ({ name; _ } : Quiver_core.Schema.Field.t) :: rest ->
               if j > 0 then Quiver_core.Byte_buffer.add_char out ',';
               Quiver_core.Byte_buffer.add_char out '"';
               Escape.add_string rfc4180_escapes (name :> string) out;
               Quiver_core.Byte_buffer.add_char out '"';
               names (j + 1) rest
         in
         names 0 (first :: rest);
         Quiver_core.Byte_buffer.append_string out (line_end Rfc4180));
        sink (Output.emit output buffers header)
  in
  {
    output;
    renderer = { compression; buffers; lines = lines t.dialect table };
    rows = 0;
  }

let add_lines lines (batch : Quiver_core.Batch.sealed @ local)
    (out : Quiver_core.Byte_buffer.t @ local) =
  let columns = Quiver_core.Batch.sealed_columns batch in
  let unrepresentable j result =
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
            Quiver_core.Byte_buffer.add_char out
              (if Quiver_core.Column.get column row then '1' else '0');
            Ok ()
        | Int, Date ->
            unrepresentable j
              (Quiver_core.Temporal.date out
                 ~days:(Quiver_core.Column.get column row))
        | Int, Unchecked_date ->
            Quiver_core.Temporal.unchecked_date out
              (Quiver_core.Column.get column row);
            Ok ()
        | Int, Year ->
            Quiver_core.Digits.padded out ~width:4
              (Quiver_core.Column.get column row);
            Ok ()
        | Int, _ ->
            Quiver_core.Digits.int out (Quiver_core.Column.get column row);
            Ok ()
        | Int64, Uint64 ->
            Quiver_core.Digits.uint64 out
              (Quiver_core.Column.get_int64 column row);
            Ok ()
        | Int64, Time fsp ->
            Quiver_core.Temporal.time out fsp
              ~micros:(Quiver_core.Column.get_int64 column row);
            Ok ()
        | Int64, (Datetime fsp | Timestamp fsp) ->
            unrepresentable j
              (Quiver_core.Temporal.datetime out fsp
                 ~micros:(Quiver_core.Column.get_int64 column row))
        | Int64, Unchecked_datetime fsp ->
            Quiver_core.Temporal.unchecked_datetime out fsp
              (Quiver_core.Column.get_int64 column row);
            Ok ()
        | Int64, _ ->
            Quiver_core.Digits.int64 out
              (Quiver_core.Column.get_int64 column row);
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
  let width = Iarray.length columns
  and count = Quiver_core.Batch.sealed_rows batch in
  let rec cells row j =
    if j = width then Ok ()
    else (
      if j > 0 then Quiver_core.Byte_buffer.add_char out ',';
      match value row j (Iarray.get columns j) with
      | Ok () -> cells row (j + 1)
      | Error error -> Error error)
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

let renderer part = part.renderer

let render renderer (batch : Quiver_core.Batch.sealed @ local) =
  let out = Output.start renderer.buffers in
  match add_lines renderer.lines batch (borrow_ out) with
  | Error (`Unrepresentable_date (table, column)) ->
      Error (`Sink (Unrepresentable_date { table; column }))
  | Ok () -> (
      match Output.seal renderer.buffers renderer.compression out with
      | Ok frame -> Ok { frame; rows = Quiver_core.Batch.sealed_rows batch }
      | Error (`Output error) -> Error (`Sink (Output error)))

let append part { frame; rows } =
  match Output.append part.output frame with
  | Ok () ->
      part.rows <- part.rows + rows;
      Ok ()
  | Error (`Output error) -> Error (`Sink (Output error))

let close part answer =
  answer
    (Result.map_error
       (fun (`Output error) -> Output error)
       (Output.close part.output ~rows:part.rows))

let discard part = Output.discard part.output

let finish t ~source ~tables ~manifest =
  sink (Layout.finish t.layout ~source ~tables ~manifest)
