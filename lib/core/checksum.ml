type t = Xxh3_64 of int64

let equal (Xxh3_64 a) (Xxh3_64 b) = Int64.equal a b
let hex (Xxh3_64 digest) = Printf.sprintf "%016Lx" digest

let sexp_of_t checksum =
  Sexplib0.Sexp.List [ Atom "Xxh3_64"; Atom (hex checksum) ]

let yojson_of_t checksum = `List [ `String "Xxh3_64"; `String (hex checksum) ]

let of_hex digits =
  match
    String.length digits = 16 && Base.String.for_all digits ~f:Base.Char.is_hex_digit
  with
  | true -> Option.map (fun digest -> Xxh3_64 digest) (Int64.of_string_opt ("0x" ^ digits))
  | false -> None

let t_of_sexp sexp =
  Base.Option.value_or_thunk
    (match (sexp : Sexplib0.Sexp.t) with
    | List [ Atom "Xxh3_64"; Atom digits ] -> of_hex digits
    | _ -> None)
    ~default:(fun () -> Sexplib0.Sexp_conv.of_sexp_error "Xxh3_64" sexp)

let t_of_yojson json =
  Base.Option.value_or_thunk
    (match json with
    | `List [ `String "Xxh3_64"; `String digits ] -> of_hex digits
    | _ -> None)
    ~default:(fun () ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "Xxh3_64" json)
