type t = { name : Ident.t; text : string; last : int64 option } [@@deriving sexp ~portable, yojson]
type counter = Unused of { next : int64; increment : int64 } | Above of int64

let make ~name ~text counter =
  let next, step =
    match counter with
    | Unused { next; increment = 0L } -> (next, 1L)
    | Unused { next; increment } -> (next, increment)
    | Above above -> (above, 1L)
  in
  let last = Int64.sub next step in
  let fits = if Int64.compare step 0L > 0 then Int64.compare last next < 0 else Int64.compare last next > 0 in
  { name; text; last = Base.Option.some_if fits last }

let statements { name; text; last } =
  text
  :: Option.fold last ~none:[] ~some:(fun last ->
         [ "DO SETVAL(" ^ Ident.backquoted name ^ ", " ^ Int64.to_string last ^ ")" ])
