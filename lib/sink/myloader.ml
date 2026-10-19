type escapes = Backslash_escapes | No_backslash_escapes

(** Where the lexer is in the text. *)
type state =
  | Code
  | Quoted of char  (** Inside ['...'], ["..."] or [`...`]. *)
  | Line_comment
  | Block_comment

let escapes sql_mode =
  Option.fold sql_mode ~none:Backslash_escapes ~some:(fun mode ->
      if
        List.exists
          (String.equal "NO_BACKSLASH_ESCAPES")
          (String.split_on_char ',' (String.uppercase_ascii mode))
      then No_backslash_escapes
      else Backslash_escapes)

let statement escapes text =
  let out = Buffer.create (String.length text + 16)
  and length = String.length text in
  let at i = if i < length then Some text.[i] else None in
  let rec go i state =
    match (at i, state) with
    | None, _ -> Some (Buffer.contents out)
    | Some ';', (Code | Line_comment | Block_comment)
      when at (i + 1) = Some '\n' ->
        Buffer.add_string out "; ";
        go (i + 1) state
    | Some ';', Quoted _ when at (i + 1) = Some '\n' -> None
    | Some c, Code -> (
        Buffer.add_char out c;
        match (c, at (i + 1), at (i + 2)) with
        | ('\'' | '"' | '`'), _, _ -> go (i + 1) (Quoted c)
        | '#', _, _ -> go (i + 1) Line_comment
        | '-', Some '-', Some (' ' | '\t' | '\n' | '\r') ->
            Buffer.add_char out '-';
            go (i + 2) Line_comment
        | '/', Some '*', _ ->
            Buffer.add_char out '*';
            go (i + 2) Block_comment
        | _ -> go (i + 1) Code)
    | Some c, Quoted quote -> (
        Buffer.add_char out c;
        match (c, at (i + 1), escapes) with
        | '\\', Some next, Backslash_escapes when quote <> '`' ->
            Buffer.add_char out next;
            go (i + 2) state
        | c, Some next, _ when c = quote && next = quote ->
            Buffer.add_char out next;
            go (i + 2) state
        | c, _, _ when c = quote -> go (i + 1) Code
        | _ -> go (i + 1) state)
    | Some c, Line_comment ->
        Buffer.add_char out c;
        let next = if Char.equal c '\n' then Code else Line_comment in
        go (i + 1) next
    | Some c, Block_comment -> (
        Buffer.add_char out c;
        match (c, at (i + 1)) with
        | '*', Some '/' ->
            Buffer.add_char out '/';
            go (i + 2) Code
        | _ -> go (i + 1) Block_comment)
  in
  go 0 Code
