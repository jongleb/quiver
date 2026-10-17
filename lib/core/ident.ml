type error = Empty | Contains_nul of string | Invalid_utf8 of string
[@@deriving sexp_of ~portable]

type t = string [@@deriving sexp_of ~portable, equal ~portable, compare ~portable, hash ~portable, yojson_of]

let of_string name =
  if String.equal name "" then Error (`Ident Empty)
  else if String.contains name '\000' then Error (`Ident (Contains_nul name))
  else if not (String.is_valid_utf_8 name) then
    Error (`Ident (Invalid_utf8 name))
  else Ok name

let t_of_sexp sexp =
  match of_string (string_of_sexp sexp) with
  | Ok ident -> ident
  | Error (`Ident error) ->
      Sexplib0.Sexp_conv.of_sexp_error
        (Sexplib0.Sexp.to_string (sexp_of_error error))
        sexp

let t_of_yojson = function
  | `String name as json -> (
      match of_string name with
      | Ok ident -> ident
      | Error (`Ident error) ->
          Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
            (Sexplib0.Sexp.to_string (sexp_of_error error))
            json)
  | json -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "Ident.t" json
