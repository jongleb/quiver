type t = Literal of string | Glob of string [@@deriving sexp_of]

type error =
  | Empty_name of string
  | Names of { pattern : string; expected : int }
  | Unclosed_quote of string
[@@deriving sexp_of]

type table = { database : t; table : t }
type column = { database : t; table : t; column : t }
type fit = Fits | Misses

let names ~expected pattern =
  let length = String.length pattern in
  let rec quoted pos acc =
    if pos >= length then Error (`Pattern (Unclosed_quote pattern))
    else if pattern.[pos] <> '`' then quoted (pos + 1) (acc ^ String.make 1 pattern.[pos])
    else if pos + 1 < length && pattern.[pos + 1] = '`' then quoted (pos + 2) (acc ^ "`")
    else Ok (pos + 1, acc)
  in
  let rec part pos acc =
    if pos >= length || pattern.[pos] = '.' then Ok (pos, Glob acc)
    else part (pos + 1) (acc ^ String.make 1 pattern.[pos])
  in
  let rec split pos acc =
    let parsed =
      if pos < length && pattern.[pos] = '`' then
        Result.map (fun (next, name) -> (next, Literal name)) (quoted (pos + 1) "")
      else part pos ""
    in
    match parsed with
    | Error _ as error -> error
    | Ok (_, (Literal "" | Glob "")) -> Error (`Pattern (Empty_name pattern))
    | Ok (next, name) ->
        if next >= length then Ok (List.rev (name :: acc))
        else if pattern.[next] = '.' then split (next + 1) (name :: acc)
        else Error (`Pattern (Names { pattern; expected }))
  in
  match split 0 [] with
  | Error _ as error -> error
  | Ok names when List.length names = expected -> Ok names
  | Ok _ -> Error (`Pattern (Names { pattern; expected }))

let table pattern =
  match names ~expected:2 pattern with
  | Ok [ database; table ] -> Ok { database; table }
  | Ok _ -> Error (`Pattern (Names { pattern; expected = 2 }))
  | Error _ as error -> error

let column pattern =
  match names ~expected:3 pattern with
  | Ok [ database; table; column ] -> Ok { database; table; column }
  | Ok _ -> Error (`Pattern (Names { pattern; expected = 3 }))
  | Error _ as error -> error

let fit name pattern ~fold =
  let name = fold name in
  match pattern with
  | Literal literal -> if String.equal (fold literal) name then Fits else Misses
  | Glob glob ->
      let glob = fold glob in
      let n = String.length name and g = String.length glob in
      let ended i = if i = n then Fits else Misses in
      let rec go i j =
        if j = g then ended i
        else
          match glob.[j] with
          | '*' -> (
              match go i (j + 1) with
              | Fits -> Fits
              | Misses -> if i < n then go (i + 1) j else Misses)
          | '?' -> if i < n then go (i + 1) (j + 1) else Misses
          | c -> if i < n && Char.equal name.[i] c then go (i + 1) (j + 1) else Misses
      in
      go 0 0

let fits_table ({ database; table } : table) ({ database = d; table = t } : Schema.Name.t) =
  match fit (d :> string) database ~fold:Fun.id with
  | Misses -> Misses
  | Fits -> fit (t :> string) table ~fold:Fun.id

let fits_column ({ database; table; column } : column) name (c : Ident.t) =
  match fits_table { database; table } name with
  | Misses -> Misses
  | Fits -> fit (c :> string) column ~fold:String.lowercase_ascii

let assignment text =
  let length = String.length text in
  let rec equals pos quoted =
    if pos >= length then None
    else
      match (text.[pos], quoted) with
      | '`', `Quoted -> equals (pos + 1) `Bare
      | '`', `Bare -> equals (pos + 1) `Quoted
      | '=', `Bare -> Some (String.sub text 0 pos, String.sub text (pos + 1) (length - pos - 1))
      | _, (`Quoted | `Bare) -> equals (pos + 1) quoted
  in
  equals 0 `Bare

let to_string = function
  | Glob glob -> glob
  | Literal literal ->
      "`" ^ String.concat "``" (String.split_on_char '`' literal) ^ "`"
