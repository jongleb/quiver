type error = Empty | Contains_nul of string | Invalid_utf8 of string
[@@deriving sexp_of ~portable]

type t = string [@@deriving sexp_of ~portable, equal ~portable, compare ~portable, hash ~portable, yojson_of]

let of_string name =
  if String.equal name "" then Error (`Ident Empty)
  else if String.contains name '\000' then Error (`Ident (Contains_nul name))
  else if not (String.is_valid_utf_8 name) then
    Error (`Ident (Invalid_utf8 name))
  else Ok name

let backquoted name = "`" ^ Base.String.substr_replace_all name ~pattern:"`" ~with_:"``" ^ "`"

let t_of_sexp sexp =
  Codec.checked_of_sexp (fun (`Ident e) -> sexp_of_error e) (of_string (string_of_sexp sexp)) sexp

let t_of_yojson = function
  | `String name as json ->
      Codec.checked_of_yojson (fun (`Ident e) -> sexp_of_error e) (of_string name) json
  | json -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "Ident.t" json
