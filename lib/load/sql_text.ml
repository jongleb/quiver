type kind = Word | Backquoted of string
type token = { start : int; length : int; kind : kind }

(** How a backslash reads inside quotes. *)
type escapes = Backslash | Literal

(** Where the reader is in the text. *)
type state =
  | Code
  | Versioned  (** Inside [/*! ... */], which the server runs. *)
  | Quoted of { quote : char; back : state; opened : int }
  | Line
  | Block

let tokens text =
  let length = String.length text in
  let at i = if i < length then Some text.[i] else None in
  let word = function 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' -> true | _ -> false in
  let read escapes =
    let rec go i state found =
      match (at i, state) with
      | None, _ -> List.rev found
      | Some c, (Code | Versioned) when word c ->
          let rec stop j = if j < length && word text.[j] then stop (j + 1) else j in
          let j = stop i in
          go j state ({ start = i; length = j - i; kind = Word } :: found)
      | Some c, (Code | Versioned) -> (
          match (c, at (i + 1), at (i + 2), state) with
          | ('\'' | '"' | '`'), _, _, _ -> go (i + 1) (Quoted { quote = c; back = state; opened = i }) found
          | '*', Some '/', _, Versioned -> go (i + 2) Code found
          | '/', Some '*', Some '!', Code -> go (i + 3) Versioned found
          | '/', Some '*', _, Code -> go (i + 2) Block found
          | '#', _, _, _ -> go (i + 1) Line found
          | '-', Some '-', Some (' ' | '\t' | '\n' | '\r'), _ -> go (i + 2) Line found
          | _ -> go (i + 1) state found)
      | Some c, Quoted { quote; back; opened } -> (
          match (c, at (i + 1), escapes) with
          | '\\', Some _, Backslash when quote <> '`' -> go (i + 2) state found
          | '`', Some '`', _ when quote = '`' -> go (i + 2) state found
          | '`', _, _ when quote = '`' ->
              let name =
                Base.String.substr_replace_all (String.sub text (opened + 1) (i - opened - 1)) ~pattern:"``"
                  ~with_:"`"
              in
              go (i + 1) back ({ start = opened; length = i + 1 - opened; kind = Backquoted name } :: found)
          | c, _, _ when c = quote -> go (i + 1) back found
          | _ -> go (i + 1) state found)
      | Some '\n', Line -> go (i + 1) Code found
      | Some _, Line -> go (i + 1) Line found
      | Some c, Block -> (
          match (c, at (i + 1)) with '*', Some '/' -> go (i + 2) Code found | _ -> go (i + 1) Block found)
    in
    go 0 Code []
  in
  let rec both escaped literal =
    match (escaped, literal) with
    | [], _ | _, [] -> []
    | a :: rest, b :: _ when a.start < b.start -> both rest literal
    | a :: _, b :: rest when a.start > b.start -> both escaped rest
    | a :: rest, b :: others -> if Stdlib.( = ) a b then a :: both rest others else both rest others
  in
  both (read Backslash) (read Literal)

let created statement =
  let word ({ start; length; _ } : token) = String.uppercase_ascii (String.sub statement start length) in
  let rec first = function
    | { kind = Backquoted name; _ } :: _ -> Some name
    | ({ kind = Word; _ } as token) :: rest when List.mem (word token) [ "IF"; "NOT"; "EXISTS" ] -> first rest
    | _ :: _ | [] -> None
  in
  let rec named = function
    | ({ kind = Word; _ } as token) :: rest
      when List.mem (word token) [ "TABLE"; "VIEW"; "SEQUENCE"; "PROCEDURE"; "FUNCTION"; "EVENT"; "TRIGGER" ] ->
        first rest
    | _ :: rest -> named rest
    | [] -> None
  in
  named (tokens statement)
