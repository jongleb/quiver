type t = (string * string) list

type error =
  | Malformed of string
  | Repeated of string
  | Other_charset of { from : string; into : string }
[@@deriving sexp_of ~portable]

let of_rules rules =
  let named name =
    String.length name > 0
    && String.for_all (function 'a' .. 'z' | '0' .. '9' | '_' -> true | _ -> false) name
    && String.contains name '_'
  in
  let charset name = Base.String.prefix name (Option.value (String.index_opt name '_') ~default:0) in
  Base.List.fold_result rules ~init:[] ~f:(fun mapped rule ->
      match String.split_on_char '=' (String.lowercase_ascii rule) with
      | [ from; into ] when named from && named into ->
          if List.mem_assoc from mapped then Error (`Collations (Repeated from))
          else if not (String.equal (charset from) (charset into)) then
            Error (`Collations (Other_charset { from; into }))
          else Ok ((from, into) :: mapped)
      | _ -> Error (`Collations (Malformed rule)))

let rewrite t text =
  match t with
  | [] -> text
  | _ :: _ ->
      let out = Buffer.create (String.length text) in
      let rec emit pos = function
        | [] -> Buffer.add_substring out text pos (String.length text - pos)
        | ({ kind = Backquoted _; _ } : Sql_text.token) :: rest -> emit pos rest
        | { start; length; kind = Word } :: rest -> (
            match List.assoc_opt (String.lowercase_ascii (String.sub text start length)) t with
            | None -> emit pos rest
            | Some into ->
                Buffer.add_substring out text pos (start - pos);
                Buffer.add_string out into;
                emit (start + length) rest)
      in
      emit 0 (Sql_text.tokens text);
      Buffer.contents out

let definition t (definition : Quiver_core.Definition.t) =
  let collation =
    Option.fold
      (List.assoc_opt (String.lowercase_ascii (definition.context.collation :> string)) t)
      ~none:definition.context.collation
      ~some:(fun into -> Result.value (Quiver_core.Ident.of_string into) ~default:definition.context.collation)
  in
  { definition with text = rewrite t definition.text; context = { definition.context with collation } }
