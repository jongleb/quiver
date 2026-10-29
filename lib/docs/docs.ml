type command = { name : string; summary : string; help : string }

let escape text =
  let out = Buffer.create (String.length text) in
  String.iter
    (function
      | '&' -> Buffer.add_string out "&amp;"
      | '<' -> Buffer.add_string out "&lt;"
      | '>' -> Buffer.add_string out "&gt;"
      | '"' -> Buffer.add_string out "&quot;"
      | '\'' -> Buffer.add_string out "&#39;"
      | c -> Buffer.add_char out c)
    text;
  Buffer.contents out

let slug name =
  String.concat ""
    (List.map
       (fun c ->
         match c with
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> String.make 1 c
         | c -> Printf.sprintf "_%02X" (Char.code c))
       (List.of_seq (String.to_seq name)))

let kind_label (kind : Quiver_core.Mask_script.kind) =
  match kind with Text -> "text" | Int -> "integer" | Bool -> "truth" | Date -> "date" | Datetime -> "time"

let shape_label (shape : Quiver_core.Mask_script.shape) =
  match shape with Of kind -> kind_label kind | Same -> "any" | Words -> "words"

let signature ({ name; parameters; arity; result; _ } : Quiver_core.Mask_script.entry) =
  let count = List.length parameters in
  let shown =
    List.mapi
      (fun i ({ name; _ } : Quiver_core.Mask_script.parameter) ->
        let upper = String.uppercase_ascii name in
        match arity with
        | Optional_last when i = count - 1 -> "[" ^ upper ^ "]"
        | Repeated_last 0 when i = count - 1 -> "[" ^ upper ^ " …]"
        | Repeated_last _ when i = count - 1 -> upper ^ " …"
        | Exactly | Optional_last | Repeated_last _ -> upper)
      parameters
  in
  Printf.sprintf "(%s) → %s" (String.concat " " (name :: shown)) (shape_label result)

let command ~name ~help =
  let lines = String.split_on_char '\n' help in
  let rec after_name = function
    | heading :: line :: rest ->
        if String.equal (String.trim heading) "NAME" then
          Option.fold (Base.String.substr_index line ~pattern:" - ") ~none:(String.trim line) ~some:(fun at ->
              String.trim (String.sub line (at + 3) (String.length line - at - 3)))
        else after_name (line :: rest)
    | [] | [ _ ] -> ""
  in
  { name; summary = after_name lines; help }

let options { help; _ } =
  let heading line = String.length line > 0 && line.[0] <> ' ' in
  let named piece =
    Option.map
      (fun rest ->
        let ends =
          Base.String.lfindi rest ~f:(fun _ c -> not (Base.Char.is_alphanum c || Char.equal c '-'))
        in
        String.sub rest 0 (Option.value ends ~default:(String.length rest)))
      (Base.String.chop_prefix piece ~prefix:"--")
  in
  let rec scan within acc = function
    | [] -> List.rev acc
    | line :: rest when heading line -> scan (Base.String.is_suffix line ~suffix:"OPTIONS") acc rest
    | line :: rest when within && String.starts_with ~prefix:"       -" line ->
        let pieces = Base.String.split_on_chars (String.trim line) ~on:[ ',' ] in
        scan within (List.rev_append (List.filter_map (fun piece -> named (String.trim piece)) pieces) acc) rest
    | _ :: rest -> scan within acc rest
  in
  scan false [] (String.split_on_char '\n' help)

let undocumented { help; _ } =
  let option line = String.starts_with ~prefix:"       -" line in
  let heading line = String.length line > 0 && line.[0] <> ' ' in
  let rec scan within acc = function
    | [] -> List.rev acc
    | line :: rest -> (
        match (heading line, within, rest) with
        | true, _, _ -> scan (Base.String.is_suffix line ~suffix:"OPTIONS") acc rest
        | false, true, next :: _ when option line && (String.trim next = "" || option next || heading next) ->
            scan within (String.trim line :: acc) rest
        | false, true, [] when option line -> List.rev (String.trim line :: acc)
        | false, _, _ -> scan within acc rest)
  in
  scan false [] (String.split_on_char '\n' help)

let style =
  {|:root {
  --ink: #1d232a; --muted: #5b6672; --line: #dde3e9; --paper: #ffffff; --tint: #f4f7fa;
  --accent: #2463c4; --code: #f2f4f7;
}
@media (prefers-color-scheme: dark) {
  :root { --ink: #e3e8ee; --muted: #9aa6b2; --line: #2c343d; --paper: #12161b; --tint: #181e25;
    --accent: #7fb0ff; --code: #1c232b; }
}
* { box-sizing: border-box; }
body { margin: 0; background: var(--paper); color: var(--ink);
  font: 16px/1.55 -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, "Helvetica Neue", Arial, sans-serif; }
header { border-bottom: 1px solid var(--line); background: var(--tint); }
header nav, main { max-width: 980px; margin: 0 auto; padding: 0 20px; }
header nav { display: flex; gap: 22px; align-items: baseline; flex-wrap: wrap; padding-top: 14px; padding-bottom: 14px; }
header nav .brand { font-weight: 700; font-size: 18px; margin-right: auto; color: var(--ink); }
header nav .version { color: var(--muted); font-weight: 400; font-size: 14px; margin-left: 6px; }
a { color: var(--accent); text-decoration: none; }
a:hover { text-decoration: underline; }
main { padding-top: 12px; padding-bottom: 60px; }
h1 { font-size: 30px; margin: 28px 0 8px; }
h2 { font-size: 22px; margin: 36px 0 8px; padding-top: 8px; border-top: 1px solid var(--line); }
h3 { font-size: 17px; margin: 26px 0 4px; }
p.lead { color: var(--muted); font-size: 18px; margin-top: 0; }
code, pre { font-family: ui-monospace, SFMono-Regular, Menlo, Consolas, monospace; font-size: 14px; }
code { background: var(--code); padding: 1px 5px; border-radius: 4px; }
pre { background: var(--code); padding: 14px 16px; border-radius: 8px; overflow-x: auto; line-height: 1.45; }
pre code { background: none; padding: 0; }
table { border-collapse: collapse; width: 100%; margin: 8px 0 4px; font-size: 14px; }
th, td { text-align: left; padding: 6px 10px; border-bottom: 1px solid var(--line); vertical-align: top; }
th { color: var(--muted); font-weight: 600; }
.toc { columns: 3 180px; padding-left: 18px; }
.toc li { break-inside: avoid; }
.signature { font-size: 15px; }
.cards { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 14px; }
.card { border: 1px solid var(--line); border-radius: 10px; padding: 14px 16px; background: var(--tint); }
.card h3 { margin-top: 0; }
footer { color: var(--muted); font-size: 13px; text-align: center; padding: 30px 0; border-top: 1px solid var(--line); }
|}

let page ~version ~title body =
  String.concat ""
    [
      "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n";
      "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n";
      "<title>";
      escape title;
      " — quiver</title>\n<link rel=\"stylesheet\" href=\"style.css\">\n</head>\n<body>\n<header><nav>";
      "<a class=\"brand\" href=\"index.html\">quiver<span class=\"version\">";
      escape version;
      "</span></a><a href=\"cli.html\">Commands</a><a href=\"masking.html\">Masking language</a>";
      "<a href=\"https://github.com/jongleb/quiver#readme\">README</a></nav></header>\n<main>\n";
      body;
      "\n</main>\n<footer>Generated by <code>quiver docs</code> from quiver ";
      escape version;
      " itself.</footer>\n</body>\n</html>\n";
    ]

let code text = "<code>" ^ escape text ^ "</code>"
let pre text = "<pre><code>" ^ escape text ^ "</code></pre>"

let index ~version ~commands =
  String.concat "\n"
    [
      "<h1>quiver</h1>";
      "<p class=\"lead\">A parallel, consistent logical copier for MySQL and MariaDB: dump, load, copy server to \
       server, across the network, masking personal data as it is read.</p>";
      "<h2>Install</h2>";
      pre
        "curl -fsSL https://raw.githubusercontent.com/jongleb/quiver/main/install.sh | sh\n\
         brew install jongleb/tap/quiver";
      "<h2>Commands</h2>";
      "<div class=\"cards\">"
      ^ String.concat ""
          (List.map
             (fun { name; summary; _ } ->
               Printf.sprintf "<div class=\"card\"><h3><a href=\"cli.html#%s\">quiver %s</a></h3><p>%s</p></div>"
                 (slug name) (escape name) (escape summary))
             commands)
      ^ "</div>";
      "<h2>Masking</h2>";
      "<p>Columns are masked as rows are read, by rules on the command line, in the quiverfile, or in a policy \
       written in a small language of S-expressions: selectors by name and type, kept and strict columns, \
       keyed fake values that still join, dates, word lists. See <a href=\"masking.html\">the masking \
       language</a>.</p>";
      pre
        "(strict \"shop.users\")\n\
         (keep \"shop.users.id\")\n\
         (mask \"shop.users.phone\" (concat \"+7 (\" (str (keyed-int value 900 999)) \") \" (digits value 3) \"-\" (right value 5)))\n\
         (mask (where (table \"shop.*\") (column \"*email*\") (type text)) (email value))";
      Printf.sprintf "<p>This is the documentation of quiver %s.</p>" (escape version);
    ]
  |> page ~version ~title:"quiver"

let cli ~version ~commands =
  String.concat "\n"
    ([
       "<h1>Commands</h1>";
       "<p class=\"lead\">Every option of every command, as <code>quiver COMMAND --help</code> prints it. Each \
        option also reads the environment variable it names, and a quiverfile.</p>";
       "<ul class=\"toc\">"
       ^ String.concat ""
           (List.map
              (fun { name; _ } -> Printf.sprintf "<li><a href=\"#%s\">%s</a></li>" (slug name) (escape name))
              commands)
       ^ "</ul>";
     ]
    @ List.map
        (fun { name; summary; help } ->
          Printf.sprintf "<h2 id=\"%s\">quiver %s</h2>\n<p>%s</p>\n%s" (slug name) (escape name) (escape summary)
            (pre help))
        commands)
  |> page ~version ~title:"Commands"

let forms =
  [
    ("value", "The column's value in this row.");
    ("(row COLUMN)", "Another column of the row, as read, before any masking.");
    ("\"text\"  42  true  false  null", "Constants.");
    ("(if CONDITION THEN ELSE)", "THEN when CONDITION is true, ELSE when it is false or NULL.");
    ("(cond (CONDITION VALUE) … (else VALUE))", "The first VALUE whose CONDITION is true.");
    ("(let ((NAME VALUE) …) BODY)", "Names, each seeing those before it.");
    ("(define (NAME PARAMETER …) BODY)", "A function, calling only those defined above it: nothing recurses.");
    ( "(mask SELECTOR BODY)",
      "The columns SELECTOR takes get BODY's value; a column takes the first rule naming it, from --mask \
       first, then the scripts in order." );
    ("(keep SELECTOR)", "Its columns are copied as they are, and count as decided.");
    ( "(permute SELECTOR [LOW HIGH])",
      "A keyed bijection of its columns, as --mask DB.TABLE.COLUMN=permute: keys stay keys, and every column \
       a foreign key ties to one of them follows unless a rule names it. LOW and HIGH bound the integers." );
    ( "(references \"DB.TABLE.COLUMN\" \"DB.TABLE.COLUMN\")",
      "The first column refers to the second though no foreign key says so: a permute of either spreads to \
       the other, and masking them differently is an error." );
    ( "(strict \"DB.TABLE\" …)",
      "Every column of these tables must be masked or kept by a rule; otherwise nothing is read, and the \
       message names the columns." );
    ("(words NAME \"FILE\")", "The lines of FILE, beside the script, for pick-word; empty lines and lines starting with # are left out.");
    ("(after-load \"DB.TABLE\" \"STATEMENT\")", "Run on the target by copy once the rows and keys are in, {table} standing for the table there.");
  ]

let masking ~version =
  let entry (entry : Quiver_core.Mask_script.entry) =
    let examples =
      match entry.examples with
      | [] -> ""
      | examples ->
          "<table><tr><th>Expression</th><th>value</th><th>Gives</th></tr>"
          ^ String.concat ""
              (List.map
                 (fun ({ expression; given; gives } : Quiver_core.Mask_script.example) ->
                   Printf.sprintf "<tr><td>%s</td><td>%s</td><td>%s</td></tr>" (code expression)
                     (Option.fold given ~none:"NULL" ~some:(fun ((kind : Quiver_core.Mask_script.kind), text) ->
                          code text ^ " <small>" ^ kind_label kind ^ "</small>"))
                     (code gives))
                 examples)
          ^ "</table>"
    in
    Printf.sprintf "<h3 id=\"fn-%s\" class=\"signature\">%s</h3>\n<p>%s</p>\n%s" (slug entry.name)
      (code (signature entry)) (escape entry.doc) examples
  in
  String.concat "\n"
    ([
       "<h1>The masking language</h1>";
       "<p class=\"lead\">A policy for masking columns as rows are read, in S-expressions. quiver types it \
        against every column before anything is read, compiles it and runs it on its render threads.</p>";
       pre
         "quiver copy … --mask-script policy.scm --mask-secret \"$MASK_SECRET\"\n\
          quiver copy … --mask 'shop.users.inn=expr:(hash-int value)'\n\
          quiver mask --mask-script policy.scm '(phone value)' '+7 (912) 345-67-89'";
       "<h2 id=\"values\">Values</h2>";
       "<p>A column of text, binary, JSON, ENUM, SET or DECIMAL is <em>text</em>; one of integers an \
        <em>integer</em>; DATE a <em>date</em>; DATETIME and TIMESTAMP a <em>time</em>; other types take \
        <code>sql:</code> instead. Every kind has NULL, which goes through functions as in SQL. A value \
        given to a column of another kind, NULL in a NOT NULL column, or a time finer than its column \
        stops the copy.</p>";
       "<h2 id=\"forms\">Forms</h2>";
       "<table><tr><th>Form</th><th>Meaning</th></tr>"
       ^ String.concat ""
           (List.map (fun (form, meaning) -> Printf.sprintf "<tr><td>%s</td><td>%s</td></tr>" (code form) (escape meaning)) forms)
       ^ "</table>";
       "<p>A SELECTOR is <code>\"DB.TABLE.COLUMN\"</code> with <code>*</code> and <code>?</code>, names in \
        backquotes taken as written, or <code>(where (table \"DB.TABLE\") (column \"NAME\") (type TYPE …))</code> \
        with any clause left out; TYPE is one of text, binary, json, integer, decimal, float, date, \
        datetime, timestamp, time, year, enum, set, geometry.</p>";
       "<h2 id=\"functions\">Functions</h2>";
       "<ul class=\"toc\">"
       ^ String.concat ""
           (List.concat_map
              (fun ({ entries; _ } : Quiver_core.Mask_script.section) ->
                List.map
                  (fun ({ name; _ } : Quiver_core.Mask_script.entry) ->
                    Printf.sprintf "<li><a href=\"#fn-%s\">%s</a></li>" (slug name) (code name))
                  entries)
              Quiver_core.Mask_script.reference)
       ^ "</ul>";
     ]
    @ List.map
        (fun ({ title; doc; entries } : Quiver_core.Mask_script.section) ->
          Printf.sprintf "<h2 id=\"%s\">%s</h2>\n<p>%s</p>\n%s" (slug title) (escape title) (escape doc)
            (String.concat "\n" (List.map entry entries)))
        Quiver_core.Mask_script.reference)
  |> page ~version ~title:"The masking language"

let site ~version ~commands =
  [
    ("index.html", index ~version ~commands);
    ("cli.html", cli ~version ~commands);
    ("masking.html", masking ~version);
    ("style.css", style);
  ]
