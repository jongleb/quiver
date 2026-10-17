open Base.Result.Let_syntax

type part = {
  file : File_path.Part.t;
  rows : int;
  bytes : int;
  stored_bytes : int;
  checksum : Checksum.t;
}
[@@deriving sexp ~portable]

let yojson_of_part { file; rows; bytes; stored_bytes; checksum } =
  `Assoc
    [
      ("file", Codec.yojson_of_file file);
      ("rows", `Int rows);
      ("bytes", `Int bytes);
      ("stored_bytes", `Int stored_bytes);
      ("checksum", Checksum.yojson_of_t checksum);
    ]

let part_of_yojson json =
  {
    file = Codec.field "file" Codec.file_of_yojson json;
    rows = Codec.field "rows" int_of_yojson json;
    bytes = Codec.field "bytes" int_of_yojson json;
    stored_bytes = Codec.field "stored_bytes" int_of_yojson json;
    checksum = Codec.field "checksum" Checksum.t_of_yojson json;
  }

type reading = {
  reader : int;
  started : Core.Time_ns.Span.t;
  duration : Core.Time_ns.Span.t;
  pool_wait : Core.Time_ns.Span.t;
  batches : int;
  allocated_words : int;
}
[@@deriving sexp_of ~portable]

type writing = {
  rendering : Core.Time_ns.Span.t;
  appending : Core.Time_ns.Span.t;
  reordering : Core.Time_ns.Span.t;
  committing : Core.Time_ns.Span.t;
}
[@@deriving sexp_of ~portable]

type state =
  | Pending
  | Done of { part : part; reading : reading; writing : writing }
[@@deriving sexp_of ~portable]

let yojson_of_state = function
  | Pending -> `List [ `String "Pending" ]
  | Done
      {
        part;
        reading =
          { reader; started; duration; pool_wait; batches; allocated_words };
        writing = { rendering; appending; reordering; committing };
      } ->
      `List
        [
          `String "Done";
          `Assoc
            [
              ("part", yojson_of_part part);
              ("reader", `Int reader);
              ("started_ns", Codec.yojson_of_span started);
              ("duration_ns", Codec.yojson_of_span duration);
              ("pool_wait_ns", Codec.yojson_of_span pool_wait);
              ("batches", `Int batches);
              ("allocated_words", `Int allocated_words);
              ("render_ns", Codec.yojson_of_span rendering);
              ("append_ns", Codec.yojson_of_span appending);
              ("reorder_wait_ns", Codec.yojson_of_span reordering);
              ("commit_ns", Codec.yojson_of_span committing);
            ];
        ]

type entry = {
  table : Schema.Name.t;
  key : Ident.t list;
  range : Chunk.Range.t option;
  state : state;
}
[@@deriving yojson_of]

type t = {
  version : int;
  level : Consistency.any;
  format : Output_format.t;
  position : Yojson.Safe.t;
  schema : Schema.t;
  chunks : entry list;
}

let yojson_of_t { version; level; format; position; schema; chunks } =
  `Assoc
    [
      ("version", `Int version);
      ("level", Consistency.yojson_of_any level);
      ("format", Output_format.yojson_of_t format);
      ("position", position);
      ("schema", Schema.yojson_of_t schema);
      ("chunks", `List (List.map yojson_of_entry chunks));
    ]

let make ~level ~format ~position ~schema chunks =
  {
    version = 1;
    level;
    format;
    position;
    schema;
    chunks =
      List.map
        (fun ((chunk : Chunk.t), state) ->
          match chunk with
          | Whole { name; _ } -> { table = name; key = []; range = None; state }
          | Range { table = { name; _ }; key; range } ->
              {
                table = name;
                key =
                  List.map
                    (fun ({ name; _ } : Schema.Field.t) -> name)
                    (fst key :: snd key);
                range = Some range;
                state;
              })
        chunks;
  }

type error = Malformed of string | Unfinished of Schema.Name.t
[@@deriving sexp_of ~portable]

type written = { part : part }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]
(** The part of a finished chunk, the rest of its state left unread. *)

(** A chunk's state as stored. *)
type stored = Pending | Done of written [@@deriving of_yojson]

type chunk = { table : Schema.Name.t; state : stored }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]
(** A chunk as stored, read only as far as loading needs. *)

type stored_manifest = { format : Output_format.t; chunks : chunk list }
[@@deriving of_yojson] [@@yojson.allow_extra_fields]
(** A manifest read only as far as loading needs. *)

type contents = {
  format : Output_format.t;
  parts : (Schema.Name.t * part) list;
}

let read json =
  match Codec.decode stored_manifest_of_yojson json with
  | Error reason -> Error (`Manifest (Malformed reason))
  | Ok { format; chunks } ->
      let%map parts =
        Base.Result.all
          (List.map
             (fun { table; state } ->
               match state with
               | Done { part } -> Ok (table, part)
               | Pending -> Error (`Manifest (Unfinished table)))
             chunks)
      in
      { format; parts }

let localize json ~format part =
  let malformed what = Error (`Manifest (Malformed what)) in
  let counts = Hashtbl.create 16 in
  let replace key value fields =
    List.map
      (fun (k, v) -> if String.equal k key then (k, value) else (k, v))
      fields
  in
  let entry = function
    | `Assoc fields -> (
        match
          (List.assoc_opt "table" fields, List.assoc_opt "state" fields)
        with
        | Some table, Some (`List [ `String "Done"; `Assoc finished ]) -> (
            match Codec.decode Schema.Name.t_of_yojson table with
            | Error reason -> malformed reason
            | Ok table -> (
                let index =
                  Option.value (Hashtbl.find_opt counts table) ~default:0
                in
                Hashtbl.replace counts table (index + 1);
                match part table index with
                | None -> Error (`Manifest (Unfinished table))
                | Some part ->
                    Ok
                      (`Assoc
                         (replace "state"
                            (`List
                               [
                                 `String "Done";
                                 `Assoc
                                   (replace "part" (yojson_of_part part)
                                      finished);
                               ])
                            fields))))
        | _ -> malformed "a chunk without a finished state")
    | _ -> malformed "a chunk that is not an object"
  in
  match json with
  | `Assoc fields ->
      let%map fields =
        Base.Result.all
          (List.map
             (fun (key, value) ->
               match (key, value) with
               | "format", _ -> Ok (key, Output_format.yojson_of_t format)
               | "chunks", `List chunks ->
                   let%map chunks = Base.Result.all (List.map entry chunks) in
                   (key, `List chunks)
               | _ -> Ok (key, value))
             fields)
      in
      `Assoc fields
  | _ -> malformed "not an object"
