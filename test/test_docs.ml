open Quiver_core

let unescaped text =
  let entities = [ ("&amp;", "&"); ("&lt;", "<"); ("&gt;", ">"); ("&quot;", "\""); ("&#39;", "'") ] in
  let out = Buffer.create (String.length text) in
  let rec go pos =
    if pos < String.length text then
      match
        List.find_opt
          (fun (entity, _) -> pos + String.length entity <= String.length text && String.sub text pos (String.length entity) = entity)
          entities
      with
      | Some (entity, plain) ->
          Buffer.add_string out plain;
          go (pos + String.length entity)
      | None ->
          Buffer.add_char out text.[pos];
          go (pos + 1)
  in
  go 0;
  Buffer.contents out

let any_text =
  QCheck2.Gen.(
    map (String.concat "")
      (list_size (int_range 0 30) (oneof_list [ "a"; "<"; ">"; "&"; "\""; "'"; "&amp;"; " "; "\n"; "Ж"; "😀"; "</pre>"; "-"; "_"; "$" ])))

let escaping =
  QCheck2.Test.make ~count:1000 ~name:"escaped text has no markup in it and reads back as it was"
    ~print:(fun text -> text) any_text (fun text ->
      let escaped = Quiver_docs.Docs.escape text in
      String.for_all (fun c -> c <> '<' && c <> '>' && c <> '"' && c <> '\'') escaped && unescaped escaped = text)

let unslug slug =
  let rec go pos acc =
    if pos >= String.length slug then Some (String.concat "" (List.rev acc))
    else if slug.[pos] = '_' && pos + 2 < String.length slug then
      Option.bind (int_of_string_opt ("0x" ^ String.sub slug (pos + 1) 2)) (fun code ->
          go (pos + 3) (String.make 1 (Char.chr code) :: acc))
    else go (pos + 1) (String.make 1 slug.[pos] :: acc)
  in
  go 0 []

let slugs =
  QCheck2.Test.make ~count:1000 ~name:"a slug is a plain HTML id, and two names never share one"
    ~print:(fun text -> text) any_text (fun name ->
      let slug = Quiver_docs.Docs.slug name in
      String.for_all (fun c -> (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c = '-' || c = '_') slug
      && unslug slug = Some name)

let balanced page =
  let void = [ "meta"; "link"; "br"; "!doctype" ] in
  let rec go pos stack =
    match String.index_from_opt page pos '<' with
    | None -> stack = []
    | Some at -> (
        match String.index_from_opt page at '>' with
        | None -> false
        | Some stop ->
            let inside = String.sub page (at + 1) (stop - at - 1) in
            let name =
              String.lowercase_ascii
                (List.hd (String.split_on_char ' ' (Base.String.chop_prefix_if_exists inside ~prefix:"/")))
            in
            if List.mem name void then go (stop + 1) stack
            else if String.length inside > 0 && inside.[0] = '/' then
              match stack with top :: rest when top = name -> go (stop + 1) rest | _ -> false
            else go (stop + 1) (name :: stack))
  in
  go 0 []

let commands =
  QCheck2.Gen.(
    list_size (int_range 0 5)
      (let* name = string_size ~gen:(oneof_list [ 'a'; 'z'; '-'; '7' ]) (int_range 1 8)
       and* summary = any_text
       and* body = any_text in
       let help = "NAME\n       quiver-" ^ name ^ " - " ^ String.concat " " (String.split_on_char '\n' summary) ^ "\n\nOPTIONS\n" ^ body in
       pure (Quiver_docs.Docs.command ~name ~help)))

let pages_are_whole =
  QCheck2.Test.make ~count:300 ~name:"every page is balanced HTML, and holds every command, function and form"
    ~print:(fun commands ->
      String.concat "\n---\n" (List.map (fun (c : Quiver_docs.Docs.command) -> c.name ^ ": " ^ c.help) commands))
    commands
    (fun commands ->
      let site = Quiver_docs.Docs.site ~version:"1.<2>&3" ~commands in
      let page path = List.assoc path site in
      let has page part = Base.String.is_substring page ~substring:part in
      List.for_all (fun (path, content) -> Filename.extension path <> ".html" || balanced content) site
      && List.for_all
           (fun ({ name; help; _ } : Quiver_docs.Docs.command) ->
             has (page "cli.html") (Printf.sprintf "id=\"%s\"" (Quiver_docs.Docs.slug name))
             && has (page "cli.html") (Quiver_docs.Docs.escape help)
             && has (page "index.html") (Printf.sprintf "cli.html#%s" (Quiver_docs.Docs.slug name)))
           commands
      && List.for_all
           (fun ({ entries; _ } : Mask_script.section) ->
             List.for_all
               (fun (entry : Mask_script.entry) ->
                 has (page "masking.html") (Printf.sprintf "id=\"fn-%s\"" (Quiver_docs.Docs.slug entry.name))
                 && has (page "masking.html") (Quiver_docs.Docs.escape (Quiver_docs.Docs.signature entry))
                 && List.for_all
                      (fun ({ expression; gives; _ } : Mask_script.example) ->
                        has (page "masking.html") (Quiver_docs.Docs.escape expression)
                        && has (page "masking.html") (Quiver_docs.Docs.escape gives))
                      entry.examples)
               entries)
           Mask_script.reference
      && List.for_all (fun form -> has (page "masking.html") (Quiver_docs.Docs.escape form))
           [ "(row COLUMN)"; "(mask SELECTOR BODY)"; "(keep SELECTOR)"; "(strict \"DB.TABLE\" …)"; "(words NAME \"FILE\")" ])

let summaries =
  QCheck2.Test.make ~count:500 ~name:"a command's summary is what its help names it"
    ~print:(fun (name, summary) -> name ^ " / " ^ summary)
    QCheck2.Gen.(
      pair
        (string_size ~gen:(oneof_list [ 'a'; 'b'; '-' ]) (int_range 1 6))
        (map
           (fun parts -> String.trim (String.concat "" parts))
           (list_size (int_range 0 20) (oneof_list [ "x"; " "; "-"; "Ж" ]))))
    (fun (name, summary) ->
      let help = "NAME\n       quiver-" ^ name ^ " - " ^ summary ^ "\n\nSYNOPSIS\n       quiver " ^ name ^ "\n" in
      String.equal (Quiver_docs.Docs.command ~name ~help).summary summary)

let undocumented =
  QCheck2.Test.make ~count:500 ~name:"options with no description are found, and only they"
    ~print:(fun (options, gapless, ended) ->
      Printf.sprintf "%s; gapless %b; ended %b"
        (String.concat "; " (List.map (fun (name, doc) -> name ^ "=" ^ Option.value doc ~default:"-") options))
        gapless ended)
    QCheck2.Gen.(
      triple
        (list_size (int_range 0 8)
           (pair
              (map (fun s -> "--" ^ s ^ "=X (absent QUIVER_X env)") (string_size ~gen:(oneof_list [ 'a'; 'b'; '-' ]) (int_range 1 5)))
              (option (string_size ~gen:(oneof_list [ 'w'; ' '; '.' ]) (int_range 1 20)))))
        bool bool)
    (fun (options, gapless, ended) ->
      let gap = if gapless then "" else "\n" in
      let body =
        String.concat ""
          (List.map
             (fun (name, doc) ->
               "       " ^ name ^ "\n" ^ Option.fold doc ~none:"" ~some:(fun doc -> "           " ^ String.trim doc ^ "w\n") ^ gap)
             options)
      in
      let rest = if ended then "" else "ENVIRONMENT\n       QUIVER_X\n           Like --x.\n" in
      let help = "NAME\n       quiver-x - X\n\nOPTIONS\n" ^ body ^ rest in
      let help = if ended && gapless then Base.String.chop_suffix_if_exists help ~suffix:"\n" else help in
      List.equal String.equal
        (Quiver_docs.Docs.undocumented (Quiver_docs.Docs.command ~name:"x" ~help))
        (List.filter_map (fun (name, doc) -> match doc with None -> Some name | Some _ -> None) options))

let options =
  QCheck2.Test.make ~count:500 ~name:"the long names of every option of a help are found, and only they"
    ~print:QCheck2.Print.(list (pair string (option string)))
    QCheck2.Gen.(
      list_size (int_range 0 8)
        (pair
           (string_size ~gen:(oneof_list [ 'a'; 'b'; '-' ]) (int_range 1 5))
           (option (oneof_list [ "-x, "; "-y DIR, " ]))))
    (fun options ->
      let body =
        String.concat ""
          (List.mapi
             (fun i (name, short) ->
               let value = if i mod 2 = 0 then "=X (absent QUIVER_X env)" else "" in
               "       " ^ Option.value short ~default:"" ^ "--" ^ name ^ value ^ "\n           Like --zzz, with -q.\n\n")
             options)
      in
      let help =
        "NAME\n       quiver-x - X\n\nOPTIONS\n" ^ body ^ "COMMON OPTIONS\n       --help[=FMT] (default=auto)\n           Show.\n\n"
        ^ "ENVIRONMENT\n       QUIVER_X\n           Like --x.\n"
      in
      List.equal String.equal
        (Quiver_docs.Docs.options (Quiver_docs.Docs.command ~name:"x" ~help))
        (List.map fst options @ [ "help" ]))

let kinds : Mask_script.kind list = [ Text; Int; Bool; Date; Datetime ]

let literal (kind : Mask_script.kind) =
  match kind with
  | Text -> "\"a\""
  | Int -> "7"
  | Bool -> "true"
  | Date -> "(make-date 2000 1 2)"
  | Datetime -> "(make-datetime (make-date 2000 1 2) 5)"

let signatures_hold =
  QCheck2.Test.make ~count:2000
    ~name:"every documented signature types as documented, and refuses another kind or count of arguments"
    ~print:(fun (section, entry, kind, extra, wrong) -> Printf.sprintf "%d/%d same=%d extra=%d wrong=%d" section entry kind extra wrong)
    QCheck2.Gen.(
      let* section = int_range 0 (List.length Mask_script.reference - 1) in
      let entries = (List.nth Mask_script.reference section).entries in
      let* entry = int_range 0 (List.length entries - 1)
      and* kind = int_range 0 4
      and* extra = int_range 0 3
      and* wrong = int_range 0 10 in
      pure (section, entry, kind, extra, wrong))
    (fun (section, entry, kind, extra, wrong) ->
      let ({ name; parameters; arity; result; _ } : Mask_script.entry) =
        List.nth (List.nth Mask_script.reference section).entries entry
      in
      let same = List.nth kinds kind in
      let kind_of (shape : Mask_script.shape) = match shape with Of kind -> Some kind | Same -> Some same | Words -> None in
      let argument (parameter : Mask_script.parameter) =
        match parameter.shape with Words -> "names" | Of _ | Same -> literal (Option.get (kind_of parameter.shape))
      in
      let count = List.length parameters in
      let arguments =
        match arity with
        | Exactly | Optional_last -> List.map argument parameters
        | Repeated_last least ->
            List.map argument (Base.List.take parameters (count - 1))
            @ List.init (least + extra) (fun _ -> argument (List.nth parameters (count - 1)))
      in
      let script =
        match Mask_script.parse ~source:"s" ~load:(fun _ -> Ok "Анна\nБорис\n") "(words names \"names.txt\")" with
        | Ok script -> script
        | Error _ -> Alcotest.fail "words"
      in
      let compiled arguments ~result =
        Result.bind
          (Mask_script.expression ~source:"e" ("(" ^ String.concat " " (name :: arguments) ^ ")"))
          (fun expression -> Mask_script.compile script expression ~value:Text ~columns:(fun _ -> None) ~result)
      in
      let expected = Option.get (kind_of result) in
      let fits = Result.is_ok (compiled arguments ~result:expected) in
      let other = List.find (fun kind -> kind <> expected) kinds in
      let refuses_result = Result.is_error (compiled arguments ~result:other) in
      let mistyped =
        match
          List.filter_map
            (fun i ->
              let parameter = List.nth parameters (Int.min i (count - 1)) in
              match parameter.shape with
              | Of kind -> Some (i, List.find (fun other -> other <> kind) kinds)
              | Same | Words -> None)
            (List.init (List.length arguments) Fun.id)
        with
        | [] -> true
        | typed -> (
            let i, other = List.nth typed (wrong mod List.length typed) in
            match compiled (List.mapi (fun j argument -> if j = i then literal other else argument) arguments) ~result:expected with
            | Error (`Mask_script (Mistyped _)) -> true
            | Ok _ | Error _ -> false)
      in
      let least =
        match arity with Exactly -> count | Optional_last -> count - 1 | Repeated_last least -> count - 1 + least
      in
      let too_few =
        least = 0
        ||
        match compiled (Base.List.take arguments (least - 1)) ~result:expected with
        | Error (`Mask_script (Arity _)) -> true
        | Ok _ | Error _ -> false
      in
      let too_many =
        match arity with
        | Repeated_last _ -> true
        | Exactly | Optional_last -> (
            match compiled (arguments @ [ List.hd arguments ]) ~result:expected with
            | Error (`Mask_script (Arity _)) -> true
            | Ok _ | Error _ -> false)
      in
      if fits && refuses_result && mistyped && too_few && too_many then true
      else
        QCheck2.Test.fail_reportf "%s: fits %b, refuses another result %b, refuses a wrong kind %b, too few %b, too many %b"
          name fits refuses_result mistyped too_few too_many)

let examples_hold () =
  let key = Keyed.of_secret Mask_script.example_secret in
  List.iter
    (fun ({ entries; _ } : Mask_script.section) ->
      List.iter
        (fun ({ name; examples; _ } : Mask_script.entry) ->
          if examples = [] && name <> "pick-word" then Alcotest.failf "%s has no example" name;
          List.iter
            (fun ({ expression; given; gives } : Mask_script.example) ->
              let got =
                match Mask_script.evaluate key expression ~given with
                | Ok shown -> shown
                | Error (`Mask_script error) -> Mask_script.describe error
              in
              Alcotest.(check string) (name ^ ": " ^ expression) gives got)
            examples)
        entries)
    Mask_script.reference

let signatures_read () =
  let entry name =
    List.find
      (fun (entry : Mask_script.entry) -> String.equal entry.name name)
      (List.concat_map (fun (section : Mask_script.section) -> section.entries) Mask_script.reference)
  in
  Alcotest.(check (list string))
    "signatures"
    [
      "(concat TEXT …) → text";
      "(- A [B]) → integer";
      "(format TEMPLATE [TEXT …]) → text";
      "(pick SEED CHOICE …) → any";
      "(make-datetime DATE SECONDS) → time";
      "(pick-word SEED WORDS) → text";
      "(< A B) → truth";
    ]
    (List.map
       (fun name -> Quiver_docs.Docs.signature (entry name))
       [ "concat"; "-"; "format"; "pick"; "make-datetime"; "pick-word"; "<" ])

let tests =
  [
    Alcotest.test_case "signatures read as written by hand" `Quick signatures_read;
    QCheck_alcotest.to_alcotest escaping;
    QCheck_alcotest.to_alcotest slugs;
    QCheck_alcotest.to_alcotest pages_are_whole;
    QCheck_alcotest.to_alcotest summaries;
    QCheck_alcotest.to_alcotest undocumented;
    QCheck_alcotest.to_alcotest options;
    QCheck_alcotest.to_alcotest signatures_hold;
    Alcotest.test_case "every documented example gives what it says" `Quick examples_hold;
  ]
