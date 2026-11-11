type error = Unreadable of string [@@deriving sexp_of ~portable]

type t = { text : string; clauses : (int * int * string) list }

let marker = "/*T![placement]"

let read text =
  let length = String.length text in
  let quoted quote pos = Option.fold ~none:length ~some:(fun at -> at + 1) (String.index_from_opt text pos quote) in
  let name inside =
    let body = String.trim inside in
    match Base.String.chop_prefix body ~prefix:"PLACEMENT POLICY=`" with
    | None -> None
    | Some rest ->
        let rec close pos acc =
          if pos >= String.length rest then None
          else if rest.[pos] = '`' && pos + 1 < String.length rest && rest.[pos + 1] = '`' then close (pos + 2) ('`' :: acc)
          else if rest.[pos] = '`' then
            if String.length (String.trim (String.sub rest (pos + 1) (String.length rest - pos - 1))) = 0 then
              Some (Base.String.of_char_list (List.rev acc))
            else None
          else close (pos + 1) (rest.[pos] :: acc)
        in
        close 0 []
  in
  let rec scan pos clauses =
    if pos >= length then Ok (List.rev clauses)
    else
      match text.[pos] with
      | ('\'' | '"' | '`') as quote -> scan (quoted quote (pos + 1)) clauses
      | '/' when pos + 1 < length && text.[pos + 1] = '*' -> (
          let finish = Option.fold ~none:length ~some:(fun at -> at + 2) (Base.String.substr_index text ~pos:(pos + 2) ~pattern:"*/") in
          match Base.String.is_substring_at text ~pos ~substring:marker with
          | false -> scan finish clauses
          | true -> (
              let inside = String.sub text (pos + String.length marker) (Int.max 0 (finish - 2 - pos - String.length marker)) in
              match name inside with
              | Some policy -> scan finish ((pos, finish, policy) :: clauses)
              | None -> Error (`Placement (Unreadable (String.sub text pos (finish - pos))))))
      | _ -> scan (pos + 1) clauses
  in
  Result.map (fun clauses -> { text; clauses }) (scan 0 [])

let policies t =
  List.rev
    (List.fold_left (fun seen (_, _, policy) -> if List.mem policy seen then seen else policy :: seen) [] t.clauses)

let without t =
  let buffer = Buffer.create (String.length t.text) in
  let last =
    List.fold_left
      (fun from (start, finish, _) ->
        let start = if start > from && t.text.[start - 1] = ' ' then start - 1 else start in
        Buffer.add_substring buffer t.text from (start - from);
        finish)
      0 t.clauses
  in
  Buffer.add_substring buffer t.text last (String.length t.text - last);
  Buffer.contents buffer
