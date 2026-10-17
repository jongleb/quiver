let sql = function
  | '\000' -> Some "\\0"
  | '\'' -> Some "\\'"
  | '"' -> Some "\\\""
  | '\n' -> Some "\\n"
  | '\\' -> Some "\\\\"
  | '\026' -> Some "\\Z"
  | _ -> None

let csv = function '"' -> Some "\"\"" | _ -> None

let column (texts : string list) : string Quiver_core.Column.t =
  match Quiver_core.Column.create String with
  | Any column -> (
      match Quiver_core.Column.repr column with
      | Text ->
          List.iter
            (fun text -> Support.get_ok (Quiver_core.Column.add column text))
            texts;
          (column : string Quiver_core.Column.t)
      | Bool | Int | Int64 | Float -> Alcotest.fail "string column expected")

let texts =
  QCheck2.Gen.(
    list_size (int_range 1 30)
      (string_size
         ~gen:
           (oneof [ char; oneof_list [ '\''; '"'; '\\'; '\000'; '\n'; 'a' ] ])
         (int_range 0 200)))

let rendered f column rows =
  List.init rows (fun row ->
      let out = Quiver_core.Byte_buffer.create () in
      f out column row;
      Quiver_core.Byte_buffer.contents out)

let tests =
  List.map
    (fun (name, replace) ->
      QCheck_alcotest.to_alcotest
        (QCheck2.Test.make ~count:500
           ~name:(name ^ " escaping equals byte-by-byte replacement")
           ~print:QCheck2.Print.(list string)
           texts
           (fun texts ->
             let escapes = Quiver_sink.Escape.make replace in
             List.equal String.equal
               (List.map
                  (fun text ->
                    String.concat ""
                      (List.map
                         (fun c ->
                           Option.value ~default:(String.make 1 c) (replace c))
                         (Base.String.to_list text)))
                  texts)
               (rendered
                  (fun out column row ->
                    Quiver_sink.Escape.add escapes column row out)
                  (column texts) (List.length texts)))))
    [ ("sql", sql); ("csv", csv) ]
  @ [
      QCheck_alcotest.to_alcotest
        (QCheck2.Test.make ~count:500 ~name:"hex equals %02X per byte"
           ~print:QCheck2.Print.(list string)
           texts
           (fun texts ->
             List.equal String.equal
               (List.map
                  (fun text ->
                    String.concat ""
                      (List.map
                         (fun c -> Printf.sprintf "%02X" (Char.code c))
                         (Base.String.to_list text)))
                  texts)
               (rendered Quiver_sink.Render.hex (column texts)
                  (List.length texts))));
    ]
