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

let equal_value a b =
  match (a, b) with
  | Signed a, Signed b | Unsigned a, Unsigned b | Duration a, Duration b ->
      Int64.equal a b
  | Text a, Text b | Binary a, Binary b | Exact a, Exact b -> String.equal a b
  | Day a, Day b -> Core.Date.equal a b
  | Instant a, Instant b -> Moment.equal a b
  | ( ( Signed _ | Unsigned _ | Text _ | Binary _ | Exact _ | Day _ | Instant _
      | Duration _ ),
      _ ) ->
      false

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
  | Day day -> tagged "Day" (`String (printed Temporal.day day))
  | Instant moment -> tagged "Instant" (`String (printed Temporal.moment moment))
  | Duration v -> tagged "Duration" (`Intlit (Int64.to_string v))

let text value =
  let cut text =
    match String.length text > 32 with
    | true -> String.sub text 0 32 ^ "…"
    | false -> text
  in
  match value with
  | Signed v -> Int64.to_string v
  | Unsigned v -> Printf.sprintf "%Lu" v
  | Text text -> Printf.sprintf "%S" (cut text)
  | Binary bytes ->
      "X'" ^ cut (Hex_encode.to_hex ~case:`Uppercase bytes) ^ "'"
  | Exact text -> text
  | Day day -> printed Temporal.day day
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
  let fields = first :: rest in
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
  match values with
  | value :: others when List.length values = List.length fields ->
      let%bind value = fits first value
      and others =
        Base.Result.all
          (List.map
             (fun (field, value) -> fits field value)
             (List.combine rest others))
      in
      Ok (value, others)
  | _ ->
      Error
        (`Key
           (Arity { expected = List.length fields; got = List.length values }))
