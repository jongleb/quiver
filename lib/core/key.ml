open Base.Result.Let_syntax

type error =
  | Arity of { expected : int; got : int }
  | Mismatch of { field : Ident.t; ltype : Ltype.t }
[@@deriving sexp_of ~portable, equal ~portable]

type value =
  | Signed of int64
  | Unsigned of int64
  | Text of string
  | Binary of string
  | Exact of string
  | Day of Core.Date.t
  | Instant of Moment.t
  | Duration of int64
[@@deriving equal ~portable]

let printed print value =
  let out = Byte_buffer.create () in
  print (borrow_ out) value;
  Byte_buffer.contents (borrow_ out)

let yojson_of_value value =
  let tagged name json = `List [ `String name; json ] in
  match value with
  | Signed v -> tagged "Signed" (`Intlit (Int64.to_string v))
  | Unsigned v -> tagged "Unsigned" (`Intlit (Printf.sprintf "%Lu" v))
  | Text text -> tagged "Text" (`String text)
  | Binary bytes ->
      tagged "Binary" (`String (Hex_encode.to_hex ~case:`Lowercase bytes))
  | Exact text -> tagged "Exact" (`String text)
  | Day day -> tagged "Day" (`String (Core.Date.to_string day))
  | Instant moment -> tagged "Instant" (`String (printed Temporal.moment moment))
  | Duration v -> tagged "Duration" (`Intlit (Int64.to_string v))

let text value =
  let cut text = if String.length text > 32 then String.sub text 0 32 ^ "…" else text in
  match value with
  | Signed v -> Int64.to_string v
  | Unsigned v -> Printf.sprintf "%Lu" v
  | Text text -> Printf.sprintf "%S" (cut text)
  | Binary bytes ->
      "X'" ^ cut (Hex_encode.to_hex ~case:`Uppercase bytes) ^ "'"
  | Exact text -> text
  | Day day -> Core.Date.to_string day
  | Instant moment -> printed Temporal.moment moment
  | Duration micros -> Int64.to_string micros ^ "µs"

let sexp_of_value value = Sexplib0.Sexp.Atom (text value)

type t = value * value list [@@deriving equal ~portable, yojson_of]

let words (first, rest) =
  match rest with
  | [] -> text first
  | _ -> "(" ^ String.concat ", " (List.map text (first :: rest)) ^ ")"

let pp out key = Format.pp_print_string out (words key)
let sexp_of_t key = Sexplib0.Sexp.Atom (words key)

let make (first, rest) values =
  let fits ({ name; ltype; _ } : Schema.Field.t) value =
    match ((ltype : Ltype.t), value) with
    | (Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32), Signed _
    | Uint64, Unsigned _
    | String, Text _
    | Bytes, Binary _
    | Decimal _, Exact _
    | Date, Day _
    | (Datetime _ | Timestamp _), Instant _
    | Time _, Duration _ ->
        Ok value
    | ( ( Bool | Int8 | Int16 | Int32 | Int64 | Uint8 | Uint16 | Uint32 | Uint64
        | Float32 | Float64 | Decimal _ | String | Bytes | Date | Year
        | Unchecked_date | Unchecked_datetime _ | Time _ | Datetime _
        | Timestamp _ | Json | Geometry | Enum _ | Set _ ),
        _ ) ->
        Error (`Key (Mismatch { field = name; ltype }))
  in
  let arity =
    Error (`Key (Arity { expected = 1 + List.length rest; got = List.length values }))
  in
  match values with
  | [] -> arity
  | value :: others -> (
      match Base.List.zip rest others with
      | Unequal_lengths -> arity
      | Ok pairs ->
          let%map value = fits first value
          and others = Base.Result.all (List.map (fun (field, value) -> fits field value) pairs) in
          (value, others))
