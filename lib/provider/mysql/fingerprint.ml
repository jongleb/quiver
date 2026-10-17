type error = string
type t = string

let sexp_of_error text =
  Sexplib0.Sexp.Atom (Printf.sprintf "not a SHA-256 fingerprint: %S" text)

let of_string text =
  let hex =
    Base.String.substr_replace_all (String.trim text) ~pattern:":" ~with_:""
  in
  match
    String.length hex = 64 && Base.String.for_all hex ~f:Base.Char.is_hex_digit
  with
  | true -> Ok (Hex_encode.from_hex hex)
  | false -> Error (`Fingerprint text)

let check t ~digest =
  match String.equal t digest with true -> `Pinned | false -> `Different

let pp out t = Format.pp_print_string out (Hex_encode.to_hex ~case:`Lowercase t)
