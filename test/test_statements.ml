let statement =
  QCheck2.Gen.(
    map
      (fun text ->
        Base.String.substr_replace_all text ~pattern:";\n" ~with_:"; ")
      (string_size
         ~gen:(oneof [ oneof_list [ ';'; '\n'; '\''; 'a'; ' ' ]; char ])
         (int_range 1 40)))

let collect text slice =
  let found = ref [] in
  let at = ref 0 in
  let read bytes ~pos ~len =
    let length = Int.min (Int.min len slice) (String.length text - !at) in
    Bytes.blit_string text !at bytes pos length;
    at := !at + length;
    Ok length
  in
  let result =
    Quiver_load.Statements.iter read
      (fun statement ->
        found := statement :: !found;
        Ok ())
  in
  (result, List.rev !found)

let tests =
  [
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:2000
         ~name:"statements come back whole from any slicing"
         ~print:(fun (statements, slice, tail) ->
           Printf.sprintf "%s / slice %d / tail %S"
             (String.concat " | " (List.map (Printf.sprintf "%S") statements))
             slice tail)
         QCheck2.Gen.(
           triple
             (list_size (int_range 0 20) statement)
             (int_range 1 50)
             (oneof_list [ ""; "\n"; "  \n\n" ]))
         (fun (statements, slice, tail) ->
           let text =
             String.concat "" (List.map (fun s -> s ^ ";\n") statements) ^ tail
           in
           match collect text slice with
           | Ok (), found -> List.equal String.equal found statements
           | Error (`Statements _), _ -> false));
    QCheck_alcotest.to_alcotest
      (QCheck2.Test.make ~count:1000
         ~name:"text after the last statement is an error"
         ~print:(fun (statement, rest, slice) ->
           Printf.sprintf "%S %S %d" statement rest slice)
         QCheck2.Gen.(
           triple statement
             (map
                (fun s ->
                  "x"
                  ^ Base.String.substr_replace_all s ~pattern:";\n" ~with_:"; ")
                (string_size ~gen:printable (int_range 0 20)))
             (int_range 1 50))
         (fun (statement, rest, slice) ->
           match collect (statement ^ ";\n" ^ rest) slice with
           | Error (`Statements _), [ first ] -> String.equal first statement
           | (Ok () | Error (`Statements _)), _ -> false));
  ]
