(** Convert a parser event list to the yaml-test-suite tree notation. The tree
    notation is a human-readable, line-by-line representation of the event
    stream, used by the test suite to specify expected output. *)

(*
    Notation summary
    ~~~~~~~~~~~~~~~~
    +STR          stream start
    -STR          stream end
    +DOC [---]    document start (with or without explicit marker)
    -DOC [...]    document end   (with or without explicit marker)
    +MAP [{}]     mapping start  (flow=true; optionally &anchor and tag URI)
    -MAP          mapping end
    +SEQ [[]]     sequence start (flow=true; optionally &anchor and tag URI)
    -SEQ          sequence end
    =VAL [&anchor] [<tag>] STYLE VALUE
                  where STYLE is one of:
                    :   plain
                    '   single-quoted
                    '"' double-quoted
                    |   literal block
                    >   folded block
    =ALI *name    alias

    The VALUE in =VAL lines has special characters escaped:
      actual newline    -> \n
      actual tab        -> \t
      actual backslash  -> \\

    The tree lines in the test suite have leading spaces for visual nesting;
    we strip those when comparing (see [normalize_tree]).
*)

open Types

(* ------------------------------------------------------------------ *)
(* Value escaping                                                        *)
(* ------------------------------------------------------------------ *)

(** Escape a scalar value string for display in the tree format. Newlines, tabs,
    backslashes, and carriage returns are made visible. Trailing spaces are
    replaced with U+2423 OPEN BOX (␣) per the yaml-test-suite convention, so
    they are not invisible in the output. *)
let escape_value (s : string) : string =
  let buf = Buffer.create (String.length s) in
  String.iter
    (fun c ->
      match c with
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | '\r' -> Buffer.add_string buf "\\r"
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\x08' -> Buffer.add_string buf "\\b"
      | _ -> Buffer.add_char buf c)
    s;
  (* Replace trailing ASCII spaces with U+2423 (OPEN BOX = ␣) *)
  let escaped = Buffer.contents buf in
  let n = String.length escaped in
  let i = ref n in
  while !i > 0 && escaped.[!i - 1] = ' ' do
    decr i
  done;
  if !i = n then escaped
  else begin
    let buf2 = Buffer.create (n + ((!i - n) * 2)) in
    Buffer.add_string buf2 (String.sub escaped 0 !i);
    for _ = !i + 1 to n do
      Buffer.add_string buf2 "\xE2\x90\xA3" (* U+2423 ␣ *)
    done;
    Buffer.contents buf2
  end

(* ------------------------------------------------------------------ *)
(* Style prefix                                                          *)
(* ------------------------------------------------------------------ *)

let style_prefix = function
  | Plain -> ":"
  | Single_quoted -> "'"
  | Double_quoted -> "\""
  | Literal -> "|"
  | Folded -> ">"

(* ------------------------------------------------------------------ *)
(* Anchor and tag formatting                                             *)
(* ------------------------------------------------------------------ *)

let format_anchor = function
  | None -> ""
  | Some a -> " &" ^ a

let format_tag = function
  | None -> ""
  | Some t -> " <" ^ t ^ ">"

(* ------------------------------------------------------------------ *)
(* Event to tree line(s)                                                 *)
(* ------------------------------------------------------------------ *)

(** Render a single event as a tree-format line. Returns one string per event.
*)
let event_to_line (ev : event) : string =
  match ev.kind with
  | Stream_start -> "+STR"
  | Stream_end -> "-STR"
  | Document_start { explicit; _ } -> if explicit then "+DOC ---" else "+DOC"
  | Document_end { explicit } -> if explicit then "-DOC ..." else "-DOC"
  | Mapping_start { anchor; tag; flow; _ } ->
      let flow_marker = if flow then " {}" else "" in
      "+MAP" ^ flow_marker ^ format_anchor anchor ^ format_tag tag
  | Mapping_end -> "-MAP"
  | Sequence_start { anchor; tag; flow; _ } ->
      let flow_marker = if flow then " []" else "" in
      "+SEQ" ^ flow_marker ^ format_anchor anchor ^ format_tag tag
  | Sequence_end -> "-SEQ"
  | Scalar { anchor; tag; value; style } ->
      "=VAL" ^ format_anchor anchor ^ format_tag tag ^ " " ^ style_prefix style
      ^ escape_value value
  | Alias name -> "=ALI *" ^ name

(** Convert an event list to the multi-line tree string. Each event becomes one
    line; lines are separated by newlines. The result ends with a final newline.
*)
let to_tree (events : event list) : string =
  let lines = List_ext.map event_to_line events in
  String.concat "\n" lines ^ "\n"

(* ------------------------------------------------------------------ *)
(* Comparison helpers                                                    *)
(* ------------------------------------------------------------------ *)

(** Split [s] into trimmed, non-empty lines for comparison. Leading whitespace
    (the visual indentation in the test suite's tree format) is stripped since
    it carries no semantic meaning. *)
let normalize_tree (s : string) : string list =
  String.split_on_char '\n' s
  |> List_ext.map String.trim
  |> List_ext.filter (fun l -> l <> "")

(** Return a human-readable diff between [expected] and [actual] tree strings.
    Only lists the first differing line. *)
let diff_trees ~expected ~actual : string option =
  let exp_lines = normalize_tree expected in
  let act_lines = normalize_tree actual in
  let rec go i = function
    | [], [] -> None
    | e :: es, a :: ax ->
        if e = a then go (i + 1) (es, ax)
        else Some (Printf.sprintf "line %d: expected %S, got %S" (i + 1) e a)
    | e :: _, [] ->
        Some (Printf.sprintf "line %d: expected %S, got <end>" (i + 1) e)
    | [], a :: _ ->
        Some (Printf.sprintf "line %d: expected <end>, got %S" (i + 1) a)
  in
  go 1 (exp_lines, act_lines)
