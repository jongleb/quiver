type error = string
type t = string

let sexp_of_error text =
  Sexplib0.Sexp.Atom (Printf.sprintf "not a SHA-256 fingerprint: %S" text)

let of_string text =
  let hex =
    Base.String.substr_replace_all (String.trim text) ~pattern:":" ~with_:""
  in
  if String.length hex = 64 && Base.String.for_all hex ~f:Base.Char.is_hex_digit then
    Ok (Hex_encode.from_hex hex)
  else Error (`Fingerprint text)

let check t ~digest =
  if String.equal t digest then `Pinned else `Different

let pp out t = Format.pp_print_string out (Hex_encode.to_hex ~case:`Lowercase t)
