(** YAMLx — pure-OCaml YAML 1.2 parser. *)

open Printf

(* ------------------------------------------------------------------ *)
(* Type sharing with Types                                               *)
(* ------------------------------------------------------------------ *)

(* These declarations make the types defined here identical to the ones
   in Types, so values flow through the pipeline without any conversion.
   The sharing syntax 'type t = Types.t = ...' satisfies both the
   compiler (same physical type) and the mli (no mention of Types). *)

type pos = Types.pos = {
  line : int;
  column : int;
  column_bytes : int;
  offset : int;
  offset_bytes : int;
}

type loc = Types.loc = { start_pos : pos; end_pos : pos }

type yaml_error = Types.yaml_error = { msg : string; loc : loc }

type scalar_style = Types.scalar_style =
  | Plain
  | Single_quoted
  | Double_quoted
  | Literal
  | Folded

type event_kind = Types.event_kind =
  | Stream_start
  | Stream_end
  | Document_start of {
      explicit : bool;
      version : (int * int) option;
      tag_directives : (string * string) list;
    }
  | Document_end of { explicit : bool }
  | Mapping_start of {
      anchor : string option;
      tag : string option;
      implicit : bool;
      flow : bool;
    }
  | Mapping_end
  | Sequence_start of {
      anchor : string option;
      tag : string option;
      implicit : bool;
      flow : bool;
    }
  | Sequence_end
  | Scalar of {
      anchor : string option;
      tag : string option;
      value : string;
      style : scalar_style;
    }
  | Alias of string

type event = Types.event = { kind : event_kind; start_pos : pos; end_pos : pos }

type node = Types.node =
  | Scalar_node of {
      anchor : string option;
      tag : string option;
      value : string;
      style : scalar_style;
      loc : loc;
      height : int;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Sequence_node of {
      anchor : string option;
      tag : string option;
      items : node list;
      flow : bool;
      loc : loc;
      height : int;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Mapping_node of {
      anchor : string option;
      tag : string option;
      pairs : (node * node) list;
      flow : bool;
      loc : loc;
      height : int;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Alias_node of {
      name : string;
      resolved : node Lazy.t; [@opaque]
      loc : loc;
      height : int;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }

type value = Types.value =
  | Null of loc
  | Bool of loc * bool
  | Int of loc * int64
  | Float of loc * float
  | String of loc * string
  | Seq of loc * value list
  | Map of loc * (loc * value * value) list

(** Extract the precomputed height from any node variant. *)
let node_height : node -> int = function
  | Scalar_node r -> r.height
  | Sequence_node r -> r.height
  | Mapping_node r -> r.height
  | Alias_node r -> r.height

let equal_value = Types.equal_value

(** Compute the height of a value tree (maximum depth from root to leaf). Leaf
    values (Null, Bool, Int, Float, String) have height 1. *)
let rec value_height : value -> int = function
  | Null _
  | Bool _
  | Int _
  | Float _
  | String _ ->
      1
  | Seq (_, items) ->
      1 + List.fold_left (fun acc v -> max acc (value_height v)) 0 items
  | Map (_, pairs) ->
      1
      + List.fold_left
          (fun acc (_, k, v) -> max acc (max (value_height k) (value_height v)))
          0 pairs

(* ------------------------------------------------------------------ *)
(* Internal pipeline wiring                                              *)
(* ------------------------------------------------------------------ *)

let make_pipeline (input : string) =
  let reader = Reader.of_string input in
  let scanner = Scanner.create reader in
  let parser_ = Parser.create scanner in
  parser_

(* ------------------------------------------------------------------ *)
(* Public API — Events                                                   *)
(* ------------------------------------------------------------------ *)

let parse_events (input : string) : event list =
  let parser_ = make_pipeline input in
  Parser.to_event_list parser_

(* ------------------------------------------------------------------ *)
(* Public API — Nodes                                                    *)
(* ------------------------------------------------------------------ *)

let parse_nodes_versioned ?(max_depth = Types.default_max_depth)
    (input : string) : ((int * int) option * node) list =
  let parser_ = make_pipeline input in
  let composer = Composer.create ~max_depth parser_ in
  let versioned, doc_start_lines =
    Composer.compose_stream_with_starts composer
  in
  let raw_comments = Scanner.drain_comments (Parser.get_scanner parser_) in
  let plain_nodes = List.map snd versioned in
  let attached =
    Comment_attacher.attach ~doc_start_lines plain_nodes raw_comments
  in
  (* Re-pair version info with the comment-attached nodes (same order). *)
  List.map2 (fun (ver, _) node -> (ver, node)) versioned attached

let parse_nodes ?(max_depth = Types.default_max_depth) (input : string) :
    node list =
  List.map snd (parse_nodes_versioned ~max_depth input)

(* ------------------------------------------------------------------ *)
(* Public API — exceptions and defaults                                  *)
(* ------------------------------------------------------------------ *)

type schema = Types.schema = Yaml_1_2 | Yaml_1_1

type error = Types.error =
  | Scan_error of yaml_error
  | Parse_error of yaml_error
  | Expansion_limit_exceeded of int
  | Depth_limit_exceeded of int
  | Printer_error of string
  | Document_count_error of string
  | Schema_error of yaml_error
  | Simplicity_error of yaml_error
  | Duplicate_key_error of yaml_error
  | Cycle_error of yaml_error

exception Error = Types.Error

let default_expansion_limit = Types.default_expansion_limit
let default_max_depth = Types.default_max_depth

(* ------------------------------------------------------------------ *)
(* Value location                                                        *)
(* ------------------------------------------------------------------ *)

let value_loc : value -> loc = function
  | Null loc
  | Bool (loc, _)
  | Int (loc, _)
  | Float (loc, _)
  | String (loc, _)
  | Seq (loc, _)
  | Map (loc, _) ->
      loc

(* Error formatting                                                      *)
(* ------------------------------------------------------------------ *)

let format_loc ?file (loc : loc) : string =
  let ({ start_pos; end_pos } : loc) = loc in
  let loc_str =
    if start_pos.line = end_pos.line then
      if start_pos.column = end_pos.column then
        sprintf "line %d, column %d" start_pos.line start_pos.column
      else
        sprintf "line %d, columns %d-%d" start_pos.line start_pos.column
          end_pos.column
    else
      sprintf "lines %d-%d, columns %d-%d" start_pos.line end_pos.line
        start_pos.column end_pos.column
  in
  match file with
  | None -> loc_str
  | Some f -> "file " ^ f ^ ", " ^ loc_str

let default_format_loc = format_loc

let show_yaml_error ?(format_loc = format_loc) (e : yaml_error) : string =
  format_loc e.loc ^ ": " ^ e.msg

let read_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let n = in_channel_length ic in
      let s = Bytes.create n in
      really_input ic s 0 n;
      Bytes.to_string s)

let catch_errors ?file ?(format_loc = default_format_loc) f =
  let pos_error kind e =
    let loc_str = format_loc ?file e.loc in
    match file with
    | None -> kind ^ ": " ^ loc_str ^ ": " ^ e.msg
    | Some _ -> loc_str ^ ": " ^ e.msg
  in
  let other_error msg =
    match file with
    | None -> msg
    | Some path -> "file " ^ path ^ ": " ^ msg
  in
  try Ok (f ()) with
  | Error (Scan_error e) -> Result.Error (pos_error "scan error" e)
  | Error (Parse_error e) -> Result.Error (pos_error "parse error" e)
  | Error (Expansion_limit_exceeded n) ->
      Result.Error
        (other_error (sprintf "expansion limit exceeded (%d nodes)" n))
  | Error (Depth_limit_exceeded n) ->
      Result.Error (other_error (sprintf "depth limit exceeded (%d levels)" n))
  | Error (Printer_error msg) ->
      Result.Error (other_error ("printer error: " ^ msg))
  | Error (Document_count_error msg) ->
      Result.Error (other_error ("document count error: " ^ msg))
  | Error (Schema_error e) -> Result.Error (pos_error "schema error" e)
  | Error (Simplicity_error e) -> Result.Error (pos_error "simplicity error" e)
  | Error (Duplicate_key_error e) ->
      Result.Error (pos_error "duplicate key error" e)
  | Error (Cycle_error e) -> Result.Error (pos_error "cycle error" e)

let register_exception_printers ?(format_loc = default_format_loc) () =
  Printexc.register_printer (function
    | Error (Scan_error e) ->
        Some ("YAMLx.Error (Scan_error): " ^ format_loc e.loc ^ ": " ^ e.msg)
    | Error (Parse_error e) ->
        Some ("YAMLx.Error (Parse_error): " ^ format_loc e.loc ^ ": " ^ e.msg)
    | Error (Expansion_limit_exceeded n) ->
        Some (sprintf "YAMLx.Error (Expansion_limit_exceeded %d)" n)
    | Error (Depth_limit_exceeded n) ->
        Some (sprintf "YAMLx.Error (Depth_limit_exceeded %d)" n)
    | Error (Printer_error msg) -> Some ("YAMLx.Error (Printer_error): " ^ msg)
    | Error (Document_count_error msg) ->
        Some ("YAMLx.Error (Document_count_error): " ^ msg)
    | Error (Schema_error e) ->
        Some ("YAMLx.Error (Schema_error): " ^ format_loc e.loc ^ ": " ^ e.msg)
    | Error (Simplicity_error e) ->
        Some
          ("YAMLx.Error (Simplicity_error): " ^ format_loc e.loc ^ ": " ^ e.msg)
    | Error (Duplicate_key_error e) ->
        Some
          ("YAMLx.Error (Duplicate_key_error): " ^ format_loc e.loc ^ ": "
         ^ e.msg)
    | Error (Cycle_error e) ->
        Some ("YAMLx.Error (Cycle_error): " ^ format_loc e.loc ^ ": " ^ e.msg)
    | _ -> None)

(* ------------------------------------------------------------------ *)
(* Public submodules                                                     *)
(* ------------------------------------------------------------------ *)

let write_file path content =
  let oc = open_out path in
  Fun.protect
    ~finally:(fun () -> close_out oc)
    (fun () -> output_string oc content)

module Node = struct
  type t = node =
    | Scalar_node of {
        anchor : string option;
        tag : string option;
        value : string;
        style : scalar_style;
        loc : loc;
        height : int;
        head_comments : string list;
        line_comment : string option;
        foot_comments : string list;
      }
    | Sequence_node of {
        anchor : string option;
        tag : string option;
        items : t list;
        flow : bool;
        loc : loc;
        height : int;
        head_comments : string list;
        line_comment : string option;
        foot_comments : string list;
      }
    | Mapping_node of {
        anchor : string option;
        tag : string option;
        pairs : (t * t) list;
        flow : bool;
        loc : loc;
        height : int;
        head_comments : string list;
        line_comment : string option;
        foot_comments : string list;
      }
    | Alias_node of {
        name : string;
        resolved : t Lazy.t; [@opaque]
        loc : loc;
        height : int;
        head_comments : string list;
        line_comment : string option;
        foot_comments : string list;
      }

  let rec has_comments = function
    | Scalar_node n -> n.head_comments <> [] || n.line_comment <> None
    | Alias_node n -> n.head_comments <> [] || n.line_comment <> None
    | Sequence_node n ->
        n.head_comments <> [] || n.line_comment <> None || n.foot_comments <> []
        || List.exists has_comments n.items
    | Mapping_node n ->
        n.head_comments <> [] || n.line_comment <> None || n.foot_comments <> []
        || List.exists (fun (k, v) -> has_comments k || has_comments v) n.pairs

end

module Nodes = struct
  type t = node list

  let of_yaml_exn = parse_nodes

  let of_yaml ?file ?max_depth input =
    catch_errors ?file (fun () -> of_yaml_exn ?max_depth input)

  let of_yaml_file ?max_depth path =
    match
      try Ok (read_file path) with
      | Sys_error msg -> Result.Error msg
    with
    | Result.Error msg -> Result.Error ("file " ^ path ^ ": " ^ msg)
    | Ok input -> of_yaml ~file:path ?max_depth input

  let to_yaml = Printer.to_yaml
  let to_yaml_file path nodes = write_file path (to_yaml nodes)

  let to_plain_yaml_exn ?strict ?expansion_limit docs =
    Printer.to_plain_yaml ?strict ?expansion_limit docs

  let to_plain_yaml_file ?strict ?expansion_limit path nodes =
    match
      catch_errors (fun () -> to_plain_yaml_exn ?strict ?expansion_limit nodes)
    with
    | Error _ as e -> e
    | Ok yaml ->
        write_file path yaml;
        Ok ()

  let has_comments = List.exists Node.has_comments
end

(* ------------------------------------------------------------------ *)
(* Value ↔ Node conversion                                              *)
(* ------------------------------------------------------------------ *)

(* ------------------------------------------------------------------ *)
(* String block-style helpers                                           *)
(* ------------------------------------------------------------------ *)

let block_style_threshold = 70

(** True when [s] contains no C0 control characters except TAB (0x09) and LF
    (0x0A), and no DEL (0x7F). These are the characters safe to embed in a YAML
    literal block scalar. *)
let is_safe_for_literal s =
  let ok = ref true in
  String.iter
    (fun c ->
      let b = Char.code c in
      if (b < 0x09 || (b > 0x0A && b < 0x20)) || b = 0x7F then ok := false)
    s;
  !ok

(** True when [s] contains only printable characters: no C0 controls (including
    no LF or TAB), no DEL (0x7F). Safe to embed in a YAML folded block scalar
    before inserting synthetic line breaks. *)
let is_safe_for_folded s =
  let ok = ref true in
  String.iter
    (fun c ->
      let b = Char.code c in
      if b < 0x20 || b = 0x7F then ok := false)
    s;
  !ok

(** True when [s] has at least one LF character that is not trailing. Used to
    decide whether a literal block style is appropriate. *)
let has_internal_lf s =
  let n = String.length s in
  let i = ref (n - 1) in
  while !i >= 0 && s.[!i] = '\n' do
    decr i
  done;
  let trail_start = !i + 1 in
  let found = ref false in
  let j = ref 0 in
  while !j < trail_start && not !found do
    if s.[!j] = '\n' then found := true;
    incr j
  done;
  !found

(** Return [s] with all trailing ['\n'] characters removed. *)
let strip_trailing_lf s =
  let n = String.length s in
  let i = ref (n - 1) in
  while !i >= 0 && s.[!i] = '\n' do
    decr i
  done;
  if !i = n - 1 then s else String.sub s 0 (!i + 1)

(** Decide the scalar style for a string value so that it round-trips correctly
    through a YAML parser using the JSON schema. Double-quoted style is used
    whenever the plain representation would be misread as a non-string type or
    is otherwise unsafe as a plain scalar. *)
let string_scalar_style (s : string) : scalar_style =
  let n = String.length s in
  if n = 0 then Double_quoted
  else
    (* Patterns resolved to non-String types by the JSON schema *)
    let is_null = s = "null" || s = "Null" || s = "NULL" || s = "~" in
    let is_bool =
      s = "true" || s = "True" || s = "TRUE" || s = "false" || s = "False"
      || s = "FALSE"
    in
    let looks_numeric =
      (* Decimal / hex / octal integers *)
      (let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
       start < n
       && String.to_seq (String.sub s start (n - start))
          |> Seq.for_all (fun c -> c >= '0' && c <= '9'))
      || (n > 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X'))
      || (n > 2 && s.[0] = '0' && (s.[1] = 'o' || s.[1] = 'O'))
      (* Floats *)
      || s = ".inf"
      || s = ".Inf" || s = ".INF" || s = "+.inf" || s = "+.Inf" || s = "+.INF"
      || s = "-.inf" || s = "-.Inf" || s = "-.INF" || s = ".nan" || s = ".NaN"
      || s = ".NAN"
      ||
      match float_of_string_opt s with
      | Some _ ->
          String.contains s '.' || String.contains s 'e'
          || String.contains s 'E'
      | None -> false
    in
    if is_null || is_bool || looks_numeric then Double_quoted
    else if not (Char_class.can_start_plain_block (Char.code s.[0])) then
      Double_quoted
    else if s.[n - 1] = ' ' then Double_quoted
    else if String.contains s '\n' || String.contains s '\r' then Double_quoted
    else
      (* ': ' or ':\n' → mapping-value indicator inside the scalar *)
      let colon_unsafe =
        try
          let i = String.index s ':' in
          i + 1 < n && (s.[i + 1] = ' ' || s.[i + 1] = '\n')
        with
        | Not_found -> false
      in
      (* ' #' → comment indicator inside the scalar *)
      let hash_unsafe =
        try
          let i = String.index s '#' in
          i > 0 && s.[i - 1] = ' '
        with
        | Not_found -> false
      in
      if colon_unsafe || hash_unsafe then Double_quoted else Plain

(** Return the scalar style and content for a [String] value. For long strings
    with safe characters, may select [Literal] or [Folded] block style and
    return word-wrapped content. Falls back to {!string_scalar_style} otherwise.

    - Strings longer than {!block_style_threshold} with internal LF and only
      safe characters → [Literal] (newlines preserved exactly).
    - Strings longer than {!block_style_threshold} with no internal LF, at least
      one space, and only printable characters → [Folded] with lines wrapped at
      ~{!block_style_threshold} characters. A trailing LF is permitted (it only
      affects the chomp indicator). *)
let string_node_content (s : string) : scalar_style * string =
  let n = String.length s in
  if n > block_style_threshold then
    if has_internal_lf s && is_safe_for_literal s then (Literal, s)
    else if not (has_internal_lf s) then
      (* Candidate for Folded: strip trailing LF(s) and check the content. *)
      let main = strip_trailing_lf s in
      let mn = String.length main in
      if
        mn > 0 && is_safe_for_folded main && String.contains main ' '
        && main.[0] <> ' '
        && main.[mn - 1] <> ' '
      then (Folded, s) (* word wrapping happens in the printer *)
      else (string_scalar_style s, s)
    else (string_scalar_style s, s)
  else (string_scalar_style s, s)

(*
   This function is ~8x faster than sprintf but the format string
   must be valid (otherwise we may get an Out_of_memory exception or crash).
   It should not be exposed to users as is.
*)
external caml_format_float : string -> float -> string = "caml_format_float"

(** Format a float as a YAML-safe plain scalar. *)
let format_float (f : float) : string =
  if Float.is_nan f then ".nan"
  else if Float.is_infinite f then if f > 0.0 then ".inf" else "-.inf"
  else
    let s =
      (*
         Avoid getting a long tail of unnecessary decimals such as
         "3.1400000000000001" when using 'sprintf "%.17g" 3.14'.
         "%.17g" guarantees roundtripping but often "%.16g" is sufficient
         and produces more compact, reader-friendly output.
      *)
      let s16 = caml_format_float "%.16g" f in
      if float_of_string s16 = f then s16 else caml_format_float "%.17g" f
    in
    (* Ensure the string won't be re-read as an integer by requiring a
       decimal point or exponent marker. *)
    if String.contains s '.' || String.contains s 'e' || String.contains s 'E'
    then s
    else s ^ ".0"

let zero_pos = Types.zero_pos
let zero_loc = { Types.start_pos = zero_pos; end_pos = zero_pos }
let no_loc = zero_loc

let make_scalar ?(style = Types.Plain) value : node =
  Scalar_node
    {
      anchor = None;
      tag = None;
      value;
      style;
      loc = no_loc;
      height = 1;
      head_comments = [];
      line_comment = None;
      foot_comments = [];
    }

let rec value_to_node : value -> node = function
  | Null _ -> make_scalar "null"
  | Bool (_, b) -> make_scalar (if b then "true" else "false")
  | Int (_, n) -> make_scalar (Int64.to_string n)
  | Float (_, f) -> make_scalar (format_float f)
  | String (_, s) ->
      let style, content = string_node_content s in
      make_scalar ~style content
  | Seq (_, items) ->
      let ns = List.map value_to_node items in
      let h =
        1 + List.fold_left (fun acc nd -> max acc (node_height nd)) 0 ns
      in
      Sequence_node
        {
          anchor = None;
          tag = None;
          items = ns;
          flow = false;
          loc = no_loc;
          height = h;
          head_comments = [];
          line_comment = None;
          foot_comments = [];
        }
  | Map (_, pairs) ->
      let ns =
        List.map (fun (_, k, v) -> (value_to_node k, value_to_node v)) pairs
      in
      let h =
        1
        + List.fold_left
            (fun acc (k, v) -> max acc (max (node_height k) (node_height v)))
            0 ns
      in
      Mapping_node
        {
          anchor = None;
          tag = None;
          pairs = ns;
          flow = false;
          loc = no_loc;
          height = h;
          head_comments = [];
          line_comment = None;
          foot_comments = [];
        }

let check_strict_keys_value values =
  let check_unique pairs =
    let seen = ref Types.Value_set.empty in
    List.iter
      (fun (pair_loc, k, _) ->
        if Types.Value_set.mem k !seen then
          raise
            (Error
               (Duplicate_key_error
                  { msg = "duplicate mapping key"; loc = pair_loc }));
        seen := Types.Value_set.add k !seen)
      pairs
  in
  let rec go = function
    | Null _
    | Bool _
    | Int _
    | Float _
    | String _ ->
        ()
    | Seq (_, items) -> List.iter go items
    | Map (_, pairs) ->
        check_unique pairs;
        List.iter
          (fun (_, k, v) ->
            go k;
            go v)
          pairs
  in
  List.iter go values

module Values = struct
  type t = value list

  let of_yaml_exn ?(max_depth = Types.default_max_depth)
      ?(expansion_limit = Types.default_expansion_limit) ?schema ?strict_schema
      ?reject_ambiguous ?plain ?strict_keys (input : string) : value list =
    let versioned = parse_nodes_versioned ~max_depth input in
    Resolver.resolve_documents ~expansion_limit ?schema ?strict_schema
      ?reject_ambiguous ?plain ?strict_keys versioned

  let of_yaml ?file ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous ?plain ?strict_keys input =
    catch_errors ?file (fun () ->
        of_yaml_exn ?max_depth ?expansion_limit ?schema ?strict_schema
          ?reject_ambiguous ?plain ?strict_keys input)

  let of_yaml_file ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous ?plain ?strict_keys path =
    match
      try Ok (read_file path) with
      | Sys_error msg -> Result.Error msg
    with
    | Result.Error msg -> Result.Error ("file " ^ path ^ ": " ^ msg)
    | Ok input ->
        of_yaml ~file:path ?max_depth ?expansion_limit ?schema ?strict_schema
          ?reject_ambiguous ?plain ?strict_keys input

  let of_nodes_exn ?(expansion_limit = Types.default_expansion_limit) ?schema
      ?strict_schema ?reject_ambiguous ?plain ?strict_keys nodes =
    (* Nodes without version info: pass None for each document. *)
    let versioned = List.map (fun n -> (None, n)) nodes in
    Resolver.resolve_documents ~expansion_limit ?schema ?strict_schema
      ?reject_ambiguous ?plain ?strict_keys versioned

  let of_nodes ?expansion_limit ?schema ?strict_schema ?reject_ambiguous ?plain
      ?strict_keys nodes =
    catch_errors (fun () ->
        of_nodes_exn ?expansion_limit ?schema ?strict_schema ?reject_ambiguous
          ?plain ?strict_keys nodes)

  let one_of_yaml_exn ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous ?plain ?strict_keys input =
    match
      of_yaml_exn ?max_depth ?expansion_limit ?schema ?strict_schema
        ?reject_ambiguous ?plain ?strict_keys input
    with
    | [] -> raise (Error (Document_count_error "no document in input"))
    | [ v ] -> v
    | _ :: _ :: _ ->
        raise (Error (Document_count_error "multiple documents in input"))

  let one_of_yaml ?file ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous ?plain ?strict_keys input =
    catch_errors ?file (fun () ->
        one_of_yaml_exn ?max_depth ?expansion_limit ?schema ?strict_schema
          ?reject_ambiguous ?plain ?strict_keys input)

  let one_of_yaml_file ?max_depth ?expansion_limit ?schema ?strict_schema
      ?reject_ambiguous ?plain ?strict_keys path =
    match
      try Ok (read_file path) with
      | Sys_error msg -> Result.Error msg
    with
    | Result.Error msg -> Result.Error ("file " ^ path ^ ": " ^ msg)
    | Ok input ->
        one_of_yaml ~file:path ?max_depth ?expansion_limit ?schema
          ?strict_schema ?reject_ambiguous ?plain ?strict_keys input

  let to_nodes ?(strict_keys = true) values =
    if strict_keys then check_strict_keys_value values;
    List.map value_to_node values

  let to_yaml ?strict_keys values = Nodes.to_yaml (to_nodes ?strict_keys values)

  let to_yaml_file ?strict_keys path values =
    write_file path (to_yaml ?strict_keys values)
end

module Value = struct
  type t = value =
    | Null of loc
    | Bool of loc * bool
    | Int of loc * int64
    | Float of loc * float
    | String of loc * string
    | Seq of loc * t list
    | Map of loc * (loc * t * t) list

  let of_yaml_exn = Values.one_of_yaml_exn
  let of_yaml = Values.one_of_yaml
  let of_yaml_file = Values.one_of_yaml_file
  let to_yaml ?(strict_keys = true) v = Values.to_yaml ~strict_keys [ v ]

  let to_yaml_file ?(strict_keys = true) path v =
    Values.to_yaml_file ~strict_keys path [ v ]

  let equal = equal_value
  let compare = Types.compare_value
  let loc = value_loc

  module Build = struct
    let null = Null zero_loc
    let bool b = Bool (zero_loc, b)
    let int i = Int (zero_loc, Int64.of_int i)
    let int64 i = Int (zero_loc, i)
    let float f = Float (zero_loc, f)
    let string s = String (zero_loc, s)
    let seq items = Seq (zero_loc, items)

    let map pairs =
      Map
        ( zero_loc,
          List.map (fun (k, v) -> (zero_loc, String (zero_loc, k), v)) pairs )
  end
end

(* ------------------------------------------------------------------ *)
(* Event printing — internal helpers for tests and the CLI tool         *)
(* ------------------------------------------------------------------ *)

let events_to_tree = Event_printer.to_tree
let diff_event_trees = Event_printer.diff_trees
