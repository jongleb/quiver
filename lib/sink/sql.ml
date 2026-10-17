open Base.Result.Let_syntax

type t = { layout : Layout.t; statement_bytes : int }

type renderer = {
  compression : Compression.t;
  buffers : Quiver_core.Buffer_pool.t;
  table : Quiver_core.Schema.Table.t;
  fields : Quiver_core.Schema.Field.t iarray;
  insert : string;
  limit : int;
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

let sink result =
  Result.map_error (fun (`Output error) -> `Sink (Output error)) result

let create layout ~statement_bytes = { layout; statement_bytes }
let database t database ~ddl = sink (Layout.database t.layout database ~ddl)
let table t table ~ddl ~parts:_ = sink (Layout.table t.layout table ~ddl)
let format _ = Quiver_core.Output_format.Sql

let objects t database definitions =
  Result.map_error
    (function
      | `Output error -> `Sink (Output error)
      | `Unsplittable name -> `Sink (Unsplittable_definition name))
    (Layout.objects t.layout database definitions)

let part t
    ({ name = { table = name; _ }; fields = first, rest; _ } as table :
      Quiver_core.Schema.Table.t) ~index =
  let compression = Layout.compression t.layout
  and buffers = Layout.buffers t.layout in
  let%bind output = sink (Layout.part t.layout table ~index ~extension:"sql") in
  let preamble = Output.start buffers in
  Quiver_core.Byte_buffer.append_string (borrow_ preamble) Layout.header;
  let%map () = sink (Output.emit output buffers preamble) in
  {
    output;
    rows = 0;
    renderer =
  {
    compression;
    buffers;
    table;
    fields = Iarray.of_list (first :: rest);
    insert =
      "INSERT INTO " ^ Layout.quote name ^ " ("
      ^ String.concat ","
          (List.map
             (fun { Quiver_core.Schema.Field.name; _ } -> Layout.quote name)
             (first :: rest))
      ^ ") VALUES\n";
    limit = t.statement_bytes;
  };
  }

let renderer part = part.renderer

let render renderer (batch : Quiver_core.Batch.sealed @ local) =
  let write (out : Quiver_core.Byte_buffer.t @ local) =
    let columns = Quiver_core.Batch.sealed_columns batch in
    let unrepresentable j =
      Error
        (`Sink
           (Unrepresentable_date
              { table = renderer.table.name; column = (Iarray.get renderer.fields j).name }))
    in
    let quoted j result =
      match result with
      | Ok () ->
          Quiver_core.Byte_buffer.add_char out '\'';
          Ok ()
      | Error `Unrepresentable_date -> unrepresentable j
    in
    let value row j (Quiver_core.Column.Any column) =
      match Quiver_core.Column.presence column row with
      | Null ->
          Quiver_core.Byte_buffer.append_string out "NULL";
          Ok ()
      | Present -> (
          match (Quiver_core.Column.repr column, (Iarray.get renderer.fields j).ltype) with
          | Bool, _ ->
              Quiver_core.Byte_buffer.add_char out
                (if Quiver_core.Column.get column row then '1' else '0');
              Ok ()
          | Int, Date ->
              Quiver_core.Byte_buffer.add_char out '\'';
              quoted j
                (Quiver_core.Temporal.date out
                   ~days:(Quiver_core.Column.get column row))
          | Int, Unchecked_date ->
              Quiver_core.Byte_buffer.add_char out '\'';
              Quiver_core.Temporal.unchecked_date out
                (Quiver_core.Column.get column row);
              Quiver_core.Byte_buffer.add_char out '\'';
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
              Quiver_core.Byte_buffer.add_char out '\'';
              Quiver_core.Temporal.time out fsp
                ~micros:(Quiver_core.Column.get_int64 column row);
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
              Quiver_core.Digits.int64 out
                (Quiver_core.Column.get_int64 column row);
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
    let rec rows row statement =
      if row = count then (
        (match statement with
        | Open _ -> Quiver_core.Byte_buffer.append_string out ";\n"
        | Closed -> ());
        Ok ())
      else
        let start =
          match statement with
          | Open start ->
              Quiver_core.Byte_buffer.append_string out ",\n";
              start
          | Closed ->
              let start = Quiver_core.Byte_buffer.length out in
              Quiver_core.Byte_buffer.append_string out renderer.insert;
              start
        in
        Quiver_core.Byte_buffer.add_char out '(';
        match cells row 0 with
        | Error error -> Error error
        | Ok () ->
            Quiver_core.Byte_buffer.add_char out ')';
            if Quiver_core.Byte_buffer.length out - start >= renderer.limit then (
              Quiver_core.Byte_buffer.append_string out ";\n";
              rows (row + 1) Closed)
            else
              rows (row + 1)
                (match statement with
                | Open _ -> statement
                | Closed -> Open start)
    in
    rows 0 Closed [@nontail]
  in
  let out = Output.start renderer.buffers in
  match write (borrow_ out) with
  | Error error -> Error error
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
