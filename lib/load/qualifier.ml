type t = { from : string; into : string; quoted : string; names : string list }
type error = Ambiguous of Quiver_core.Ident.t [@@deriving sexp_of ~portable]

let make ~(from : Quiver_core.Ident.t) ~(into : Quiver_core.Ident.t) ~names =
  if List.exists (Quiver_core.Ident.equal from) names then Error (`Qualifier (Ambiguous from))
  else
    Ok { from = (from :> string); into = (into :> string); quoted = Quiver_core.Ident.backquoted into; names = List.map (fun (name : Quiver_core.Ident.t) -> (name :> string)) names }

let rewrite t text =
  let name ({ start; length; kind } : Sql_text.token) =
    match kind with Word -> String.sub text start length | Backquoted name -> name
  in
  let rec blank i = if i < String.length text && Base.Char.is_whitespace text.[i] then blank (i + 1) else i in
  let dotted after =
    let at = blank after in
    if at < String.length text && text.[at] = '.' then Some (blank (at + 1)) else None
  in
  let out = Buffer.create (String.length text) in
  let rec emit pos = function
    | ({ start; length; _ } as qualifier : Sql_text.token) :: (({ start = next; _ } as named) :: _ as rest)
      when Option.equal Int.equal (dotted (start + length)) (Some next)
           && String.equal (name qualifier) t.from
           && List.mem (name named) t.names ->
        Buffer.add_substring out text pos (start - pos);
        Buffer.add_string out t.quoted;
        emit (start + length) rest
    | _ :: rest -> emit pos rest
    | [] -> Buffer.add_substring out text pos (String.length text - pos)
  in
  if String.equal t.from t.into then text
  else (
    emit 0 (Sql_text.tokens text);
    Buffer.contents out)
