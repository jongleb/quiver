open Quiver_core
open Sexplib0.Sexp_conv

let key = Keyed.of_secret "a secret"

type cell = Null | Text of string | Int of int | Big of int64 [@@deriving sexp_of]

let equal_cell (a : cell) b = a = b

let hex text = Hex_encode.to_hex ~case:`Lowercase text

let ok what = function Ok value -> value | Error _ -> Alcotest.fail what

let hmac_oracle () =
  List.iter
    (fun (key, prefix, text, expected) ->
      Alcotest.(check string)
        (Printf.sprintf "HMAC of %S" text)
        expected
        (hex (Quiver_openssl.Sha.hmac_sha256 ~key ~prefix text)))
    [
      ("k", 0L, "", "eb4854a13279f14d76736c73ced7543c65fbfebb32c4edbaf43d79572489a792");
      ( "secret key",
        4294967299L,
        "Привет, мир",
        "19694aa74c7994972e0466563d0cc7b4f34228d0074478559d7c59927c33cee8" );
      ( String.make 64 '\000',
        Int64.max_int,
        String.make 200 'x',
        "1ac178a4606537d34c5b0b77cde9c114db65fabb17b98a86984e3d7a591e7e62" );
    ]

let rec glob_model pattern name =
  match (pattern, name) with
  | [], [] -> true
  | '*' :: rest, _ ->
      let skipped = match name with [] -> false | _ :: tail -> glob_model pattern tail in
      glob_model rest name || skipped
  | '?' :: rest, _ :: tail -> glob_model rest tail
  | c :: rest, n :: tail -> Char.equal c n && glob_model rest tail
  | _ :: _, [] | [], _ :: _ -> false

let chars text = List.init (String.length text) (String.get text)

let glob_matches_model =
  QCheck2.Test.make ~count:2000 ~name:"a pattern matches a name as a plain glob matcher says"
    ~print:(fun (d, t, c, (nd, nt, nc)) -> String.concat " | " [ d; t; c; nd; nt; nc ])
    QCheck2.Gen.(
      let part alphabet = string_size ~gen:(oneof_list alphabet) (int_range 1 5) in
      let glob = part [ 'a'; 'b'; 'A'; '*'; '?' ] and name = part [ 'a'; 'b'; 'A' ] in
      tup4 glob glob glob (tup3 name name name))
    (fun (d, t, c, (nd, nt, nc)) ->
      let pattern = ok "pattern" (Pattern.column (String.concat "." [ d; t; c ])) in
      let name = { Schema.Name.database = Support.ident nd; table = Support.ident nt } in
      let expected =
        glob_model (chars d) (chars nd)
        && glob_model (chars t) (chars nt)
        && glob_model (chars (String.lowercase_ascii c)) (chars (String.lowercase_ascii nc))
      in
      match Pattern.fits_column pattern name (Support.ident nc) with
      | Fits -> expected
      | Misses -> not expected)

let quoted_names_match_exactly =
  QCheck2.Test.make ~count:1000 ~name:"a backquoted name matches only itself"
    ~print:(fun (a, b) -> a ^ " / " ^ b)
    QCheck2.Gen.(
      let name = string_size ~gen:(oneof_list [ 'a'; '.'; '`'; '*'; '?'; '='; 'x' ]) (int_range 1 6) in
      pair name name)
    (fun (name, other) ->
      let quoted = "`" ^ String.concat "``" (String.split_on_char '`' name) ^ "`" in
      match Pattern.table (quoted ^ "." ^ quoted) with
      | Error _ -> false
      | Ok pattern ->
          let fits text =
            match
              Pattern.fits_table pattern
                { database = Support.ident text; table = Support.ident text }
            with
            | Fits -> true
            | Misses -> false
          in
          fits name && (String.equal name other || not (fits other)))

let pattern_errors () =
  let error text =
    match Pattern.column text with
    | Ok _ -> "fits"
    | Error (`Pattern error) -> Sexplib0.Sexp.to_string (Pattern.sexp_of_error error)
  in
  Alcotest.(check (list string))
    "refused patterns"
    [
      "(Names(pattern a.b)(expected 3))";
      "(Empty_name a..b)";
      "(Unclosed_quote `a.b.c)";
      "(Names(pattern a.b.c.d)(expected 3))";
      "fits";
    ]
    (List.map error [ "a.b"; "a..b"; "`a.b.c"; "a.b.c.d"; "`a.b`.c.d" ])

let rule_errors () =
  let outcome text =
    match Mask.rule text with
    | Ok rule -> Sexplib0.Sexp.to_string (Mask.sexp_of_rule rule)
    | Error (`Mask error) -> Sexplib0.Sexp.to_string (Mask.sexp_of_error error)
    | Error (`Pattern error) -> Sexplib0.Sexp.to_string (Pattern.sexp_of_error error)
    | Error (`Mask_script error) -> Mask_script.describe error
  in
  Alcotest.(check (list string))
    "rules"
    [
      "a.b.c=null";
      "a.b.c=text:x=y";
      "\"`a=b`.t.c=sql:CONCAT('a', id)\"";
      "(Malformed a.b.c)";
      "(Unknown_action nul)";
      "(Malformed a.b.c=sql:)";
      "(Names(pattern a.b)(expected 3))";
    ]
    (List.map outcome
       [ "a.b.c=null"; "a.b.c=text:x=y"; "`a=b`.t.c=sql:CONCAT('a', id)"; "a.b.c"; "a.b.c=nul"; "a.b.c=sql:"; "a.b=hash" ])

(** A character's kind, as [shape] must keep it. *)
type kind = Digit | Upper | Lower | Kept of int | Cyrillic_upper | Cyrillic_lower | Other

let kind code =
  if code >= Char.code '0' && code <= Char.code '9' then Digit
  else if code >= Char.code 'A' && code <= Char.code 'Z' then Upper
  else if code >= Char.code 'a' && code <= Char.code 'z' then Lower
  else if code < 0x80 then Kept code
  else if (code >= 0x410 && code <= 0x42F) || code = 0x401 then Cyrillic_upper
  else if (code >= 0x430 && code <= 0x44F) || code = 0x451 then Cyrillic_lower
  else Other

let decoded text =
  let rec go pos acc =
    if pos >= String.length text then Some (List.rev acc)
    else
      let decode = String.get_utf_8_uchar text pos in
      if Uchar.utf_decode_is_valid decode then
        go (pos + Uchar.utf_decode_length decode) (Uchar.to_int (Uchar.utf_decode_uchar decode) :: acc)
      else None
  in
  go 0 []

let shaped_like original masked =
  match (decoded original, decoded masked) with
  | Some before, Some after when List.length before = List.length after ->
      String.length masked <= String.length original
      && List.for_all2
           (fun before after ->
             match (kind before, kind after) with
             | Other, Lower -> true
             | Other, _ -> false
             | expected, actual -> expected = actual)
           before after
  | _ -> false

let letters text =
  Option.fold ~none:0
    ~some:(fun codes -> List.length (List.filter (fun code -> match kind code with Kept _ -> false | _ -> true) codes))
    (decoded text)

let text_gen =
  QCheck2.Gen.(
    let piece =
      oneof_list
        [ "a"; "Z"; "q"; "7"; "0"; " "; "-"; "."; "+"; "("; "Ж"; "ё"; "Ё"; "я"; "é"; "中"; "😀"; "α"; "@"; "_" ]
    in
    map (String.concat "") (list_size (int_range 0 24) piece))

let with_batch ltypes rows f =
  let batch = Batch.create ltypes in
  let rec cells (columns : Column.any Iarray.t @ local) j = function
    | [] -> ()
    | cell :: rest ->
        (let (Column.Any column) = Iarray.get columns j in
         match (Column.repr column, cell) with
         | _, Null -> Column.add_null column
         | Text, Text s -> Support.get_ok (Column.add column s)
         | Int, Int n -> Support.get_ok (Column.add column n)
         | Int64, Big n -> Support.get_ok (Column.add column n)
         | _ -> Alcotest.fail "cell does not fit its column");
        cells columns (j + 1) rest
  in
  let rec fill (batch : Batch.t @ local) = function
    | [] -> ()
    | row :: rest ->
        cells (Batch.columns batch) 0 row;
        Batch.commit_row batch;
        fill batch rest
  in
  fill (borrow_ batch) rows;
  match Batch.seal batch with Ok sealed -> f sealed | Error _ -> Alcotest.fail "seal"

let cells_of (batch : Batch.sealed @ local) =
  let columns = Batch.sealed_columns batch in
  let width = Iarray.length columns in
  let rec row i j acc =
    if j < 0 then acc
    else
      let (Column.Any column) = Iarray.get columns j in
      let cell =
        match (Column.presence column i, Column.repr column) with
        | Null, _ -> Null
        | Present, Text -> Text (Column.get column i)
        | Present, Int -> Int (Column.get column i)
        | Present, Int64 -> Big (Column.get column i)
        | Present, (Bool | Float) -> Null
      in
      row i (j - 1) (cell :: acc)
  in
  let rec rows i acc = if i < 0 then acc else rows (i - 1) (row i (width - 1) [] :: acc) in
  rows (Batch.sealed_rows batch - 1) [] [@nontail]

let columns =
  [
    ("id", Ltype.Int64, Schema.Field.Not_null);
    ("s", String, Nullable);
    ("b", Bytes, Nullable);
    ("i", Int32, Nullable);
    ("u", Uint16, Not_null);
    ("n", String, Not_null);
    ("e", Enum [ "x"; "y" ], Nullable);
  ]

(** What a test asks of a column. *)
type asked = Keep | Mask_null | Constant of string | Shape | Email | Hash | Source
[@@deriving sexp_of]

let action_of = function
  | Keep -> None
  | Mask_null -> Some "null"
  | Constant value -> Some ("text:" ^ value)
  | Shape -> Some "shape"
  | Email -> Some "email"
  | Hash -> Some "hash"
  | Source -> Some "sql:UPPER(n)"

let apply_scenario =
  QCheck2.Gen.(
    let asked choices = oneof_list choices in
    let* s = asked [ Keep; Mask_null; Constant "c"; Shape; Email; Hash; Source ]
    and* b = asked [ Keep; Mask_null; Constant "\001\002"; Hash; Source ]
    and* i = asked [ Keep; Mask_null; Hash; Source ]
    and* u = asked [ Keep; Hash; Source ]
    and* n = asked [ Keep; Constant ""; Shape; Email; Hash; Source ]
    and* e = asked [ Keep; Mask_null; Constant "y" ] in
    let text = oneof [ map (fun t -> Text t) text_gen; pure Null ] in
    let* rows =
      list_size (int_range 0 30)
        (let* id = int_range 0 1000
         and* s = oneof [ text; map (fun t -> Text (t ^ "@example.com")) text_gen ]
         and* b = oneof [ map (fun t -> Text t) (string_size (int_range 0 20)); pure Null ]
         and* i = oneof [ map (fun n -> Int n) (int_range (-1000) 1000); pure Null ]
         and* u = map (fun n -> Int n) (int_range 0 65535)
         and* n = oneof [ map (fun t -> Text t) text_gen; pure Null ]
         and* e = oneof_list [ Text "x"; Text "y"; Null ] in
         pure [ Big (Int64.of_int id); s; b; i; u; n; e ])
    in
    let* repeated = bool in
    let rows = if repeated then rows @ rows else rows in
    pure ([ s; b; i; u; n; e ], rows))

let masked_by asks rows =
  let table = Support.table (List.map (fun (name, ltype, nullability) -> (name, ltype, nullability)) columns) in
  let rules =
    List.filter_map Fun.id
      (List.map2
         (fun (name, _, _) asked ->
           Option.map
             (fun action -> ok action (Mask.rule ("db.t." ^ name ^ "=" ^ action)))
             (action_of asked))
         (List.tl columns) asks)
  in
  match Mask.resolve (Mask.make key ~script:Mask_script.empty ~strict:[] rules) [ table ] with
  | Error (`Mask error) -> Error error
  | Ok [ mask ] ->
      with_batch
        (List.map (fun (_, ltype, _) -> ltype) columns)
        rows
        (fun sealed ->
          match Mask.Table.apply mask (borrow_ sealed) with
          | Ok () -> Ok (cells_of sealed)
          | Error (`Mask error) -> Error error)
  | Ok _ -> Alcotest.fail "one table"

let apply_keeps_promises =
  QCheck2.Test.make ~count:400 ~name:"each action keeps its promise on every row, NULL stays NULL"
    ~print:(fun (asks, rows) ->
      Sexplib0.Sexp.to_string_hum ([%sexp_of: asked list * cell list list] (asks, rows)))
    apply_scenario
    (fun (asks, rows) ->
      let not_null = [ false; false; false; true; true; false ] in
      let nulls_under_source =
        List.exists2
          (fun asked (required, j) ->
            match asked with
            | Source -> required && List.exists (fun row -> equal_cell (List.nth row j) Null) rows
            | Keep | Mask_null | Constant _ | Shape | Email | Hash -> false)
          asks
          (List.combine not_null [ 1; 2; 3; 4; 5; 6 ])
      in
      let unfit_null =
        List.exists2
          (fun asked required -> match asked with Mask_null -> required | _ -> false)
          asks not_null
      in
      match masked_by asks rows with
      | Error (Not_nullable _) -> unfit_null
      | Error (Null_from_source _) -> nulls_under_source && not unfit_null
      | Error _ -> false
      | Ok masked ->
          (not nulls_under_source) && (not unfit_null)
          && List.length masked = List.length rows
          && List.for_all2
               (fun before after ->
                 equal_cell (List.hd before) (List.hd after)
                 && List.for_all2
                      (fun asked (j, ltype) ->
                        let before = List.nth before j and after = List.nth after j in
                        match (asked, before, after) with
                        | Keep, _, _ | Source, _, _ -> equal_cell before after
                        | Mask_null, _, Null -> true
                        | Mask_null, _, _ -> false
                        | _, Null, Null -> true
                        | _, Null, _ | _, _, Null -> false
                        | Constant value, _, Text text -> String.equal value text
                        | Shape, Text before, Text after -> shaped_like before after
                        | Email, Text before, Text after -> (
                            match String.rindex_opt before '@' with
                            | None -> shaped_like before after
                            | Some at ->
                                let domain = String.sub before at (String.length before - at) in
                                Base.String.is_suffix after ~suffix:domain
                                && shaped_like
                                     (String.sub before 0 at)
                                     (String.sub after 0 (String.length after - String.length domain)))
                        | Hash, Text before, Text after -> (
                            match ltype with
                            | Ltype.Bytes -> String.length before = String.length after
                            | _ ->
                                Option.equal Int.equal
                                  (Option.map List.length (decoded before))
                                  (Some (String.length after))
                                && String.for_all
                                     (fun c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f'))
                                     after)
                        | Hash, Int _, Int after -> (
                            match ltype with
                            | Int32 -> after >= -0x8000_0000 && after <= 0x7FFF_FFFF
                            | Uint16 -> after >= 0 && after <= 65535
                            | _ -> false)
                        | _ -> false)
                      asks
                      (List.map (fun j -> (j, (fun (_, ltype, _) -> ltype) (List.nth columns j))) [ 1; 2; 3; 4; 5; 6 ]))
               rows masked)

let same_values_mask_alike =
  QCheck2.Test.make ~count:200
    ~name:"equal values mask alike within a run, a secret masks the same each time, another one differently"
    ~print:QCheck2.Print.(list string)
    QCheck2.Gen.(list_size (int_range 1 10) text_gen)
    (fun texts ->
      let masked secret action =
        let table = Support.table [ ("id", Int64, Not_null); ("s", String, Nullable) ] in
        let rule = ok action (Mask.rule ("db.t.s=" ^ action)) in
        match Mask.resolve (Mask.make (Keyed.of_secret secret) ~script:Mask_script.empty ~strict:[] [ rule ]) [ table ] with
        | Ok [ mask ] ->
            with_batch [ Int64; String ]
              (List.mapi (fun id text -> [ Big (Int64.of_int id); Text text ]) (texts @ texts))
              (fun sealed ->
                match Mask.Table.apply mask (borrow_ sealed) with
                | Ok () -> List.map (fun row -> List.nth row 1) (cells_of sealed)
                | Error _ -> Alcotest.fail "apply")
        | _ -> Alcotest.fail "resolve"
      in
      List.for_all
        (fun action ->
          let once = masked "one" action and again = masked "one" action and other = masked "two" action in
          let n = List.length texts in
          List.equal equal_cell once again
          && List.equal equal_cell (Base.List.take once n) (Base.List.drop once n)
          && List.for_all2
               (fun text (mine, theirs) ->
                 let masked_part =
                   match (action, String.rindex_opt text '@') with
                   | "email", Some at -> String.sub text 0 at
                   | _ -> text
                 in
                 letters masked_part < 8
                 || ((not (equal_cell mine theirs)) && not (equal_cell mine (Text text))))
               texts
               (List.combine (Base.List.take once n) (Base.List.take other n)))
        [ "shape"; "hash"; "email" ])

(** A table of a resolve scenario. *)
type table_case = {
  name : string * string;
  fields : (string * Ltype.t * Schema.Field.nullability) list;
  key : [ `Primary | `Unique | `Indexed | `None ];
  generated : bool;
}
[@@deriving sexp_of]

(** A rule of a resolve scenario. *)
type rule_case = { database : string; table : string; column : string; action : string }
[@@deriving sexp_of]

let resolve_scenario =
  QCheck2.Gen.(
    let ltype =
      oneof_list
        Ltype.[ Int32; Int64; Uint8; String; Bytes; Json; Date; Enum [ "x"; "y" ]; Set [ "x"; "y" ]; Float64 ]
    in
    let field name =
      let* ltype = ltype and* nullability = oneof_list [ Schema.Field.Nullable; Not_null ] in
      pure (name, ltype, nullability)
    in
    let table_case name =
      let* count = int_range 1 4 in
      let* fields = flatten_list (List.map field (Base.List.take [ "a"; "b"; "c"; "d" ] count)) in
      let* key = oneof_list [ `Primary; `Unique; `Indexed; `None ] and* generated = bool in
      let fields =
        match (key, fields) with
        | `None, _ | _, [] -> fields
        | (`Primary | `Unique | `Indexed), (name, ltype, _) :: rest -> (name, ltype, Schema.Field.Not_null) :: rest
      in
      pure { name; fields; key; generated }
    in
    let* tables =
      let* count = int_range 1 3 in
      flatten_list
        (List.map table_case (Base.List.take [ ("d1", "t1"); ("d1", "t2"); ("d2", "t1") ] count))
    in
    let* rules =
      list_size (int_range 1 4)
        (let* database = oneof_list [ "d1"; "d2"; "*"; "d?" ]
         and* table = oneof_list [ "t1"; "t2"; "*"; "x" ]
         and* column = oneof_list [ "a"; "B"; "c"; "g"; "*"; "?" ]
         and* action = oneof_list [ "null"; "text:x"; "text:x,y"; "text:z"; "shape"; "email"; "hash"; "sql:1"; "keep" ] in
         pure { database; table; column; action })
    in
    let* strict =
      oneof_weighted
        [ (4, pure []); (1, list_size (int_range 1 2) (oneof_list [ "d1.t1"; "d1.*"; "*.*"; "d9.*" ])) ]
    in
    pure (tables, rules, strict))

let schema_table { name = database, table; fields; key; generated } =
  let fields = List.map (fun (name, ltype, nullability) -> { Schema.Field.name = Support.ident name; ltype; nullability }) fields in
  let first = (List.hd fields).name in
  let generated =
    if generated then [ { Schema.Field.name = Support.ident "g"; ltype = String; nullability = Nullable } ] else []
  in
  let key : Ident.t Schema.Table.key =
    match key with
    | `Primary -> Primary (first, [])
    | `Unique -> Unique { index = Support.ident "u"; fields = (first, []) }
    | `Indexed -> Indexed { index = Support.ident "i"; field = first }
    | `None -> No_key
  in
  Support.get_ok
    (Schema.Table.make
       ~name:{ database = Support.ident database; table = Support.ident table }
       ~fields ~generated ~key ~estimate:{ rows = 0; bytes = 0 })

let resolve_is_sound_and_complete =
  QCheck2.Test.make ~count:3000
    ~name:"resolving rules fails only for a reason the tables and rules show, and masks the first rule's columns otherwise"
    ~print:(fun case -> Sexplib0.Sexp.to_string_hum ([%sexp_of: table_case list * rule_case list * string list] case))
    resolve_scenario
    (fun (tables, rules, strict) ->
      let names text name =
        glob_model (chars text) (chars name)
      in
      let first (database, table) column =
        Base.List.findi rules ~f:(fun _ rule ->
            names rule.database database && names rule.table table
            && names (String.lowercase_ascii rule.column) (String.lowercase_ascii column))
      in
      let columns case =
        List.map (fun (name, ltype, nullability) -> (name, ltype, nullability, `Field)) case.fields
        @ if case.generated then [ ("g", Ltype.String, Schema.Field.Nullable, `Generated) ] else []
      in
      let named =
        List.concat_map
          (fun case ->
            List.filter_map
              (fun ((column, _, _, _) as entry) ->
                Option.map (fun (i, rule) -> (case, entry, i, rule)) (first case.name column))
              (columns case))
          tables
      in
      let fits action ltype =
        match (action, ltype) with
        | "null", _ | "sql:1", _ | "keep", _ -> true
        | ("text:x" | "text:z"), Ltype.(String | Bytes | Json) -> true
        | "text:x", (Enum _ | Set _) -> true
        | "text:x,y", Ltype.(String | Bytes | Json | Set _) -> true
        | ("shape" | "email"), String -> true
        | "hash", Ltype.(String | Bytes | Int32 | Int64 | Uint8) -> true
        | _ -> false
      in
      let named_in case = List.filter (fun (c, _, _, _) -> c == case) named in
      let violations =
        List.concat_map
          (fun case ->
            let mine = named_in case in
            let fields = List.filter (fun (_, (_, _, _, place), _, _) -> place = `Field) mine in
            List.concat_map
              (fun (_, (column, ltype, nullability, place), _, (rule : rule_case)) ->
                let key_column = let c, _, _ = List.hd case.fields in c in
                let generated =
                  match place with `Generated -> [ `Generated_column (case.name, column) ] | `Field -> []
                and keyed =
                  match (place, case.key) with
                  | `Field, (`Primary | `Unique) when String.equal column key_column && rule.action <> "keep" ->
                      [ `Key_column (case.name, column) ]
                  | _ -> []
                and nulled =
                  match (place, rule.action, nullability) with
                  | `Field, "null", Schema.Field.Not_null -> [ `Not_nullable (case.name, column) ]
                  | _ -> []
                and typed =
                  match (place, rule.action, ltype) with
                  | `Generated, _, _ -> []
                  | `Field, ("text:z" | "text:x,y"), Ltype.Enum _ -> [ `Not_member (case.name, column) ]
                  | `Field, "text:z", Ltype.Set _ -> [ `Not_member (case.name, column) ]
                  | `Field, action, ltype when not (fits action ltype) -> [ `Unfit (case.name, column) ]
                  | `Field, _, _ -> []
                in
                generated @ keyed @ nulled @ typed)
              mine
            @ (let masked = List.filter (fun (_, _, _, (rule : rule_case)) -> rule.action <> "keep") fields in
               if masked <> [] && case.generated then [ `Generated_values case.name ] else [])
            @
            let watched =
              List.exists
                (fun pattern ->
                  match String.split_on_char '.' pattern with
                  | [ d; t ] -> names d (fst case.name) && names t (snd case.name)
                  | _ -> false)
                strict
            in
            let undecided =
              List.filter
                (fun (column, _, _) -> not (List.exists (fun (_, (c, _, _, _), _, _) -> String.equal c column) fields))
                case.fields
            in
            if watched && undecided <> [] then [ `Unhandled case.name ] else [])
          tables
        @ List.filter_map
            (fun pattern ->
              let fitting =
                List.exists
                  (fun case ->
                    match String.split_on_char '.' pattern with
                    | [ d; t ] -> names d (fst case.name) && names t (snd case.name)
                    | _ -> false)
                  tables
              in
              if fitting then None else Some (`Unused ("strict " ^ pattern)))
            strict
        @ List.filter_map
            (fun (i, (rule : rule_case)) ->
              if List.exists (fun (_, _, j, _) -> j = i) named then None
              else Some (`Unused (String.concat "." [ rule.database; rule.table; rule.column ] ^ "=" ^ rule.action)))
            (List.mapi (fun i rule -> (i, rule)) rules)
      in
      let parsed =
        List.map
          (fun { database; table; column; action } ->
            match Mask.rule (String.concat "." [ database; table; column ] ^ "=" ^ action) with
            | Ok rule -> rule
            | Error _ -> Alcotest.fail "rule")
          rules
      in
      let pair (name : Schema.Name.t) column = (((name.database :> string), (name.table :> string)), (column : Ident.t :> string)) in
      let strict =
        List.map
          (fun pattern ->
            match Pattern.table pattern with Ok table -> ("strict " ^ pattern, table) | Error _ -> Alcotest.fail pattern)
          strict
      in
      match Mask.resolve (Mask.make key ~script:Mask_script.empty ~strict parsed) (List.map schema_table tables) with
      | Ok masks ->
          violations = []
          && List.for_all2
               (fun case mask ->
                 List.equal
                   (fun (c, a) (c', a') -> String.equal c c' && String.equal a a')
                   (List.map (fun (column, action) -> ((column : Ident.t :> string), action)) (Mask.Table.masked mask))
                   (List.filter_map
                      (fun (_, (column, _, _, _), _, (rule : rule_case)) ->
                        if String.equal rule.action "keep" then None
                        else
                          Some
                            ( column,
                              match String.index_opt rule.action ':' with
                              | Some at -> String.sub rule.action 0 at
                              | None -> rule.action ))
                      (named_in case)))
               tables masks
      | Error (`Mask error) -> (
          let has v = List.mem v violations in
          match error with
          | Generated_column { table; column } ->
              let t, c = pair table column in
              has (`Generated_column (t, c))
          | Key_column { table; column } ->
              let t, c = pair table column in
              has (`Key_column (t, c))
          | Not_nullable { table; column } ->
              let t, c = pair table column in
              has (`Not_nullable (t, c))
          | Unfit { table; column; _ } ->
              let t, c = pair table column in
              has (`Unfit (t, c))
          | Not_member { table; column; _ } ->
              let t, c = pair table column in
              has (`Not_member (t, c))
          | Generated_values table -> has (`Generated_values ((table.database :> string), (table.table :> string)))
          | Unused written -> has (`Unused written)
          | Unhandled { table; _ } -> has (`Unhandled ((table.database :> string), (table.table :> string)))
          | Malformed _ | Unknown_action _ | Scripted _ | Null_from_source _ | Null_from_expression _
          | Mismatched_column _ | Rewritten _ | Finer_than_column _ ->
              false))

let table_refusals () =
  let table =
    Support.table
      [ ("id", Int64, Not_null); ("e", Enum [ "x"; "y" ], Nullable); ("s", Set [ "x"; "y" ], Nullable); ("n", String, Not_null); ("d", Date, Nullable) ]
  and generated = Support.table ~generated:[ ("g", String, Nullable) ] [ ("id", Int64, Not_null); ("v", String, Nullable) ] in
  let told ?(table = table) rule =
    match Mask.rule rule with
    | Error _ -> "parse"
    | Ok rule -> (
        match Mask.resolve (Mask.make key ~script:Mask_script.empty ~strict:[] [ rule ]) [ table ] with
        | Ok _ -> "fine"
        | Error (`Mask error) -> Sexplib0.Sexp.to_string (Mask.sexp_of_error error))
  in
  Alcotest.(check (list string))
    "a table with generated columns"
    [ "(Generated_column(table(db t))(column g))"; "(Generated_values(db t))"; "fine" ]
    (List.map (told ~table:generated) [ "db.t.g=null"; "db.t.v=null"; "db.t.v=keep" ]);
  Alcotest.(check (list string))
    "each refusal"
    [
      "fine";
      "(Not_member(table(db t))(column e)(value z))";
      "(Not_member(table(db t))(column e)(value x,y))";
      "fine";
      "(Not_member(table(db t))(column s)(value x,z))";
      "(Not_nullable(table(db t))(column n))";
      "(Unfit(table(db t))(column d)(action shape)(ltype Date))";
      "(Key_column(table(db t))(column id))";
      "fine";
      "(Unused db.t.zzz=null)";
    ]
    (List.map told
       [
         "db.t.e=text:x"; "db.t.e=text:z"; "db.t.e=text:x,y"; "db.t.s=text:x,y"; "db.t.s=text:x,z"; "db.t.n=null";
         "db.t.d=shape"; "db.t.id=hash"; "db.t.id=keep"; "db.t.zzz=null";
       ])

let source_query () =
  let table =
    Support.table
      [
        ("id", Int64, Not_null);
        ("v", Int32, Nullable);
        ("s", String, Nullable);
        ("d", Datetime (Support.get_ok (Ltype.Fsp.of_int 3)), Nullable);
        ("e", Enum [ "x" ], Nullable);
        ("k", Decimal (Support.get_ok (Ltype.Decimal.make ~precision:10 ~scale:2)), Nullable);
        ("b", Bytes, Nullable);
      ]
  in
  Alcotest.(check string)
    "each source field is its expression cast to the field's type, under its own name"
    "SELECT `id`, CAST((id * 2) AS SIGNED) AS `v`, CAST((CONCAT('a', id)) AS CHAR CHARACTER SET \
     utf8mb4) AS `s`, CAST((NOW()) AS DATETIME(3)) AS `d`, ('x') AS `e`, CAST((0) AS DECIMAL(10, \
     2)) AS `k`, CAST((UNHEX('00')) AS BINARY) AS `b` FROM `db`.`t` ORDER BY `id`"
    (Quiver_mysql.Query.sql
       (Chunk
          {
            chunk = Chunk.whole table;
            sources =
              List.map
                (fun (name, expression) -> (Support.ident name, expression))
                [
                  ("v", "id * 2");
                  ("s", "CONCAT('a', id)");
                  ("d", "NOW()");
                  ("e", "'x'");
                  ("k", "0");
                  ("b", "UNHEX('00')");
                ];
          }))

let after_load () =
  let rule text =
    match Quiver_load.After_load.rule text with
    | Ok rule -> rule
    | Error _ -> Alcotest.fail text
  in
  let name database table = { Schema.Name.database = Support.ident database; table = Support.ident table } in
  let tables = [ name "shop" "users"; name "shop" "orders"; name "logs" "users" ] in
  let into (table : Schema.Name.t) = { table with database = Support.ident ("new_" ^ (table.database :> string)) } in
  let shown result =
    match result with
    | Ok statements ->
        List.map
          (fun ((table : Schema.Name.t), (first, rest)) ->
            (table.database :> string) ^ "." ^ (table.table :> string) ^ ": " ^ String.concat " ; " (first :: rest))
          statements
    | Error (`After_load error) -> [ Sexplib0.Sexp.to_string (Quiver_load.After_load.sexp_of_error error) ]
  in
  Alcotest.(check (list string))
    "statements by table, in rule order, with the target's names"
    [
      "new_shop.users: UPDATE `new_shop`.`users` SET a = 1 ; DELETE FROM users WHERE b";
      "new_shop.orders: UPDATE `new_shop`.`orders` SET a = 1";
    ]
    (shown
       (Quiver_load.After_load.statements
          [ rule "shop.*=UPDATE {table} SET a = 1"; rule "*.users= DELETE FROM users WHERE b " ]
          (Base.List.take tables 2) ~into));
  Alcotest.(check (list string))
    "a rule naming no table"
    [ "(Unused nope.*=x)" ]
    (shown (Quiver_load.After_load.statements [ rule "shop.users=x"; rule "nope.*=x" ] tables ~into));
  Alcotest.(check bool)
    "a rule with no statement" true
    (Result.is_error (Quiver_load.After_load.rule "shop.users=  "))

(** The rows a dump through the model provider and sink delivered. *)
type delivered = { sources : (string * string) list; rows : cell list list; masked : (string * string) list }

let dump_through rules ~null_source =
  let table =
    Support.table
      [ ("id", Int64, Not_null); ("note", String, Nullable); ("code", Int32, Not_null); ("src", String, Not_null) ]
  in
  let seen = Portable.Atomic.make [] in
  let module Provider = struct
    type conn_config = unit
    type position = unit
    type session = int
    type error = Never

    let sexp_of_error Never = Sexplib0.Sexp.Atom "never"

    type snapshot_mode = unit
    type 'lvl supports = unit
    type connections = int

    let supports () _ = Ok ()
    let connect () (_ : Provider_intf.sessions) = Ok 1
    let opened n = n
    let disconnect _ = ()
    let introspect _ _ = Schema.make [ table ]
    let plan _ _ _ = Ok [ Chunk.whole table ]

    let snapshot () n =
      Ok (Array.init n (fun session -> Await.Capsule.Await.With_mutex.create (fun () -> session)), ())

    let ddl _ _ = Ok ""
    let key_span _ _ = Ok None

    let read_chunk _ _ mask writer =
      let sources = Mask.Table.sources mask in
      Portable.Atomic.set seen (List.map (fun (name, expression) -> ((name : Ident.t :> string), expression)) sources);
      let pending = ref (List.init 40 Fun.id) in
      Batch_writer.fill writer (fun (columns : Column.any Iarray.t @ local) ->
          match !pending with
          | [] -> Ok Batch_writer.End
          | id :: rest ->
              pending := rest;
              let (Column.Any ids) = Iarray.get columns 0 in
              let () =
                match Column.repr ids with
                | Int64 -> ignore (Column.add ids (Int64.of_int id))
                | _ -> ()
              in
              let (Column.Any notes) = Iarray.get columns 1 in
              let () =
                match Column.repr notes with
                | Text when id mod 5 = 0 -> Column.add_null notes
                | Text -> ignore (Column.add notes (Printf.sprintf "Note %d, Ёлка" id))
                | _ -> ()
              in
              let (Column.Any codes) = Iarray.get columns 2 in
              let () =
                match Column.repr codes with Int -> ignore (Column.add codes id) | _ -> ()
              in
              let (Column.Any computed) = Iarray.get columns 3 in
              let () =
                match Column.repr computed with
                | Text when null_source && id = 17 -> Column.add_null computed
                | Text ->
                    ignore
                      (Column.add computed
                         (Option.fold ~none:"raw" ~some:(fun expression -> "server: " ^ expression)
                            (List.find_map
                               (fun ((name : Ident.t), expression) ->
                                 if String.equal (name :> string) "src" then Some expression else None)
                               sources)))
                | _ -> ()
              in
              Ok Batch_writer.Row)

    let objects _ _ = Ok []
    let definition _ _ _ = Error (`Provider Never)
    let close _ = ()
    let sexp_of_position () = Sexplib0.Sexp.Atom "nowhere"
    let source () = ""
    let yojson_of_position () = `Null
    let position_of_yojson _ = ()
  end in
  let module Sink = struct
    type t = { rows : cell list list list Portable.Atomic.t }
    type part = { owner : t; mutable kept : cell list list }
    type renderer = unit
    type rendered = { cells : cell list list @@ aliased many } [@@unboxed]
    type error = Never [@@warning "-37"]

    let sexp_of_error Never = Sexplib0.Sexp.Atom "never"
    let format _ = Output_format.Sql
    let database _ _ ~ddl:_ = Ok ()
    let table _ _ ~ddl:_ ~parts:_ = Ok ()
    let add_part _ _ = Ok ()
    let objects _ _ _ = Ok ()
    let part owner _ ~index:_ = Ok { owner; kept = [] }
    let renderer _ = ()
    let render () (batch : Batch.sealed @ local) = Ok { cells = cells_of batch }

    let append part { cells } =
      part.kept <- part.kept @ cells;
      Ok ()

    let close part answer =
      let kept = part.kept in
      Portable.Atomic.update part.owner.rows ~pure_f:(fun rows -> kept :: rows);
      answer
        (Ok
           {
             Manifest.file = File_path.Part.of_string "part";
             rows = List.length kept;
             bytes = 0;
             stored_bytes = 0;
             checksum = Xxh3_64 0L;
           })

    let discard _ = ()
    let finish _ ~source:_ ~tables:_ ~manifest:_ = Ok ()
  end in
  let module Dump = Dump.Make (Provider) (Sink) in
  let sink = { Sink.rows = Portable.Atomic.make [] } in
  let opts =
    Support.get_ok
      (Dump_opts.make
         ~plan:(Support.get_ok (Plan_opts.make ~chunk_bytes:1))
         ~readers:(Readers 1) ~renderers:(Renderers 2) ~cores:2 ~batch_bytes:64
         ~order:Largest_chunks ~declaring:Before_its_rows)
  in
  let rules =
    List.map
      (fun text -> match Mask.rule text with Ok rule -> rule | Error _ -> Alcotest.fail text)
      rules
  in
  match
    Support.within 10. (fun () ->
        Dump.run
          {
            conn = ();
            mode = ();
            level = Best_effort;
            filter = Everything;
            rows = All_rows;
            objects = Without_objects;
            masking = Mask.make key ~script:Mask_script.empty ~strict:[] rules;
            opts;
          }
          sink)
  with
  | Error `Timeout -> Alcotest.fail "the dump hung"
  | Ok outcome ->
  Result.map_error (function `Mask error -> `Mask error | _ -> `Other) @@ Result.map
    (fun (manifest : Manifest.t) ->
      {
        sources = Portable.Atomic.get seen;
        rows = List.concat (Portable.Atomic.get sink.rows);
        masked = List.map (fun ({ column; action; _ } : Manifest.masked) -> ((column :> string), action)) manifest.masked;
      })
    outcome

let through_a_dump () =
  match dump_through [ "db.t.note=shape"; "db.t.code=hash"; "db.t.src=sql:UPPER(x)" ] ~null_source:false with
  | Error _ -> Alcotest.fail "the dump failed"
  | Ok { sources; rows; masked } ->
      Alcotest.(check (list (pair string string))) "the source evaluates its field" [ ("src", "UPPER(x)") ] sources;
      Alcotest.(check (list (pair string string)))
        "the manifest names every masked column"
        [ ("note", "shape"); ("code", "hash"); ("src", "sql") ]
        masked;
      Alcotest.(check int) "every row" 40 (List.length rows);
      List.iter
        (fun row ->
          match row with
          | [ Big id; note; Int code; Text src ] ->
              let id = Int64.to_int id in
              let original = Printf.sprintf "Note %d, Ёлка" id in
              let shaped =
                match note with
                | Null -> id mod 5 = 0
                | Text text -> id mod 5 <> 0 && shaped_like original text && text <> original
                | Int _ | Big _ -> false
              in
              Alcotest.(check bool) "a shaped note" true shaped;
              Alcotest.(check bool) "a hashed code in range" true (code >= -0x8000_0000 && code <= 0x7FFF_FFFF);
              Alcotest.(check string) "the source's value" "server: UPPER(x)" src
          | _ -> Alcotest.fail "a row of another shape")
        rows;
      Alcotest.(check bool)
        "codes are masked" true
        (List.exists (function [ Big id; _; Int code; _ ] -> Int64.to_int id <> code | _ -> false) rows)

let null_from_source () =
  match dump_through [ "db.t.src=sql:UPPER(x)" ] ~null_source:true with
  | Error (`Mask (Mask.Null_from_source { column; _ })) ->
      Alcotest.(check string) "the column" "src" (column :> string)
  | Error _ -> Alcotest.fail "another failure"
  | Ok _ -> Alcotest.fail "a NULL in a NOT NULL column went through"

let unmasked_key () =
  match dump_through [ "db.t.id=hash" ] ~null_source:false with
  | Error (`Mask (Mask.Key_column _)) -> ()
  | Error _ | Ok _ -> Alcotest.fail "a key column was masked"

let tests =
  [
    Alcotest.test_case "HMAC-SHA256 matches Python's hmac" `Quick hmac_oracle;
    QCheck_alcotest.to_alcotest glob_matches_model;
    QCheck_alcotest.to_alcotest quoted_names_match_exactly;
    Alcotest.test_case "malformed patterns are refused" `Quick pattern_errors;
    Alcotest.test_case "rules parse, malformed ones are refused" `Quick rule_errors;
    QCheck_alcotest.to_alcotest apply_keeps_promises;
    QCheck_alcotest.to_alcotest same_values_mask_alike;
    QCheck_alcotest.to_alcotest resolve_is_sound_and_complete;
    Alcotest.test_case "each rule a table cannot take is refused" `Quick table_refusals;
    Alcotest.test_case "the source evaluates expressions cast to each field's type" `Quick source_query;
    Alcotest.test_case "after-load statements go to the tables their rules name" `Quick after_load;
    Alcotest.test_case "a dump masks what reaches the sink and the manifest says so" `Quick through_a_dump;
    Alcotest.test_case "a NULL from the source into a NOT NULL column fails the dump" `Quick null_from_source;
    Alcotest.test_case "a key column cannot be masked" `Quick unmasked_key;
  ]
