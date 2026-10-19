type error =
  | Fsp_out_of_range of int
  | Precision_out_of_range of int
  | Scale_out_of_range of { precision : int; scale : int }
[@@deriving sexp_of ~portable]

module Fsp = struct
  type t = int [@@deriving sexp_of ~portable, equal ~portable, yojson_of]

  let microseconds = 6

  let of_int n =
    if n < 0 || n > microseconds then Error (`Ltype (Fsp_out_of_range n))
    else Ok n

  let t_of_sexp sexp =
    Codec.checked_of_sexp (fun (`Ltype e) -> sexp_of_error e) (of_int (int_of_sexp sexp)) sexp

  let t_of_yojson = function
    | `Int n as json -> Codec.checked_of_yojson (fun (`Ltype e) -> sexp_of_error e) (of_int n) json
    | json -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "Ltype.Fsp" json
end

module Decimal = struct
  type t = { precision : int; scale : int } [@@deriving sexp ~portable, equal ~portable, yojson]

  let make ~precision ~scale =
    if precision < 1 then Error (`Ltype (Precision_out_of_range precision))
    else if scale < 0 || scale > precision then
      Error (`Ltype (Scale_out_of_range { precision; scale }))
    else Ok { precision; scale }

  let t_of_sexp sexp =
    let { precision; scale } = t_of_sexp sexp in
    Codec.checked_of_sexp (fun (`Ltype e) -> sexp_of_error e) (make ~precision ~scale) sexp

  let t_of_yojson json =
    let { precision; scale } = t_of_yojson json in
    Codec.checked_of_yojson (fun (`Ltype e) -> sexp_of_error e) (make ~precision ~scale) json
end

type t =
  | Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Uint8
  | Uint16
  | Uint32
  | Uint64
  | Float32
  | Float64
  | Decimal of Decimal.t
  | String
  | Bytes
  | Date
  | Year
  | Unchecked_date
  | Unchecked_datetime of Fsp.t
  | Time of Fsp.t
  | Datetime of Fsp.t
  | Timestamp of Fsp.t
  | Json
  | Geometry
  | Enum of string list
  | Set of string list
[@@deriving sexp ~portable, equal ~portable, yojson]
