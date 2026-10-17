let yojson_of_file file = `String (File_path.Part.to_string file)

let file_of_yojson = function
  | `String text as json -> (
      match File_path.Part.of_string text with
      | file -> file
      | exception failure ->
          Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
            (Printexc.to_string failure)
            json)
  | json -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "a file name" json

let yojson_of_span span =
  `Intlit (Core.Int63.to_string (Core.Time_ns.Span.to_int63_ns span))

let span_of_yojson json =
  Base.Option.value_or_thunk
    (Option.map Core.Time_ns.Span.of_int_ns
       (match json with
       | `Intlit ns | `String ns -> int_of_string_opt ns
       | `Int ns when ns >= 0 -> Some ns
       | _ -> None))
    ~default:(fun () ->
      Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "a span in ns" json)

let field name convert json =
  match json with
  | `Assoc fields ->
      convert
        (Base.Option.value_or_thunk (List.assoc_opt name fields)
           ~default:(fun () ->
             Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error
               ("no field " ^ name) json))
  | _ -> Ppx_yojson_conv_lib.Yojson_conv.of_yojson_error "not an object" json

let decode of_yojson json =
  match of_yojson json with
  | value -> Ok value
  | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (failure, _) ->
      Error
        (match failure with
        | Failure reason -> reason
        | other -> Printexc.to_string other)
