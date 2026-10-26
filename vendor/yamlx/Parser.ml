(** YAML 1.2 parser. Transforms the token stream from the Scanner into an event
    stream. The parser implements a recursive-descent grammar matching the YAML
    1.2.2 specification. *)

(*
    Grammar summary (tokens → events)
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

             stream ::= STREAM_START doc* STREAM_END
                doc ::= DIRECTIVE* DOCUMENT_START node? DOCUMENT_END?
               node ::= ALIAS | properties? (block_content | flow_content)
         properties ::= TAG ANCHOR? | ANCHOR TAG?
      block_content ::= block_collection | scalar
       flow_content ::= flow_collection | scalar
   block_collection ::= block_sequence | block_mapping
    flow_collection ::= flow_sequence | flow_mapping
     block_sequence ::= BLOCK_SEQ_START (BLOCK_ENTRY node?)* BLOCK_END
      block_mapping ::= BLOCK_MAP_START ((KEY node?)? (VALUE node?)?)* BLOCK_END
      flow_sequence ::= FLOW_SEQ_START (flow_seq_entry FLOW_ENTRY?)* FLOW_SEQ_END
       flow_mapping ::= FLOW_MAP_START (flow_map_entry FLOW_ENTRY?)* FLOW_MAP_END

    The parser is implemented as a state machine. [next_event] drives it one
    step at a time, returning one event per call.
*)

open Types

(* ------------------------------------------------------------------ *)
(* Directive table                                                       *)
(* ------------------------------------------------------------------ *)

type directives = {
  mutable version : (int * int) option;
  mutable tags : (string * string) list;  (** (handle, prefix) pairs *)
}

let default_directives () =
  { version = None; tags = [ ("!", "!"); ("!!", "tag:yaml.org,2002:") ] }

(* ------------------------------------------------------------------ *)
(* Parser state                                                          *)
(* ------------------------------------------------------------------ *)

(** The parser is a pushdown automaton. [state] encodes the current production
    being processed and [states] is the return stack. *)
type parse_state =
  | Parse_stream_start
  | Parse_document_start_implicit  (** first document without '---' *)
  | Parse_document_start  (** subsequent or explicit documents *)
  | Parse_document_end
  | Parse_document_content
  | Parse_block_node
  | Parse_block_node_or_indentless_sequence
  | Parse_flow_node
  | Parse_block_sequence_first_entry
  | Parse_block_sequence_entry
  | Parse_indentless_sequence_entry
  | Parse_block_mapping_first_key
  | Parse_block_mapping_key
  | Parse_block_mapping_value
  | Parse_flow_sequence_first_entry
  | Parse_flow_sequence_entry
  | Parse_flow_sequence_need_separator
      (** after an entry: requires [','] or ['\]'] *)
  | Parse_flow_sequence_entry_mapping_key
  | Parse_flow_sequence_entry_mapping_value
  | Parse_flow_sequence_entry_mapping_end
  | Parse_flow_mapping_first_key
  | Parse_flow_mapping_key
  | Parse_flow_mapping_need_separator
      (** after a key-value pair: requires [','] or ['\}'] *)
  | Parse_flow_mapping_value
  | Parse_end

type t = {
  scanner : Scanner.state;
  mutable state : parse_state;
  mutable states : parse_state list;  (** return stack *)
  mutable directives : directives;
  mutable events : event list;  (** buffered events *)
}

let create (scanner : Scanner.state) : t =
  {
    scanner;
    state = Parse_stream_start;
    states = [];
    directives = default_directives ();
    events = [];
  }

(* ------------------------------------------------------------------ *)
(* Helpers                                                               *)
(* ------------------------------------------------------------------ *)

let mk_event kind sp ep = { kind; start_pos = sp; end_pos = ep }

(** Push a continuation state onto the return stack. *)
let push_state p state = p.states <- state :: p.states

(** Pop a continuation state. *)
let pop_state p =
  match p.states with
  | s :: rest ->
      p.states <- rest;
      s
  | [] -> Parse_end

(** True if the scanner's next token has the given kind. *)
let check p kinds = Scanner.check_token p.scanner kinds

let peek_kind p = Scanner.peek_kind p.scanner
let get_tok p = Scanner.get_token p.scanner
let peek_tok p = Scanner.peek_token p.scanner

(** Percent-decode a tag suffix (e.g. [%21] → [!]). *)
let pct_decode s =
  let n = String.length s in
  let buf = Buffer.create n in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '%' && !i + 2 < n then begin
      let hi = s.[!i + 1] in
      let lo = s.[!i + 2] in
      let hex_val c =
        if c >= '0' && c <= '9' then Char.code c - Char.code '0'
        else if c >= 'a' && c <= 'f' then Char.code c - Char.code 'a' + 10
        else if c >= 'A' && c <= 'F' then Char.code c - Char.code 'A' + 10
        else -1
      in
      let h = hex_val hi and l = hex_val lo in
      if h >= 0 && l >= 0 then begin
        Buffer.add_char buf (Char.chr ((h lsl 4) lor l));
        i := !i + 3
      end
      else begin
        Buffer.add_char buf s.[!i];
        incr i
      end
    end
    else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

(** Resolve a tag using the directive table. [(handle, suffix)] → full URI
    string. *)
let resolve_tag directives pos handle suffix =
  let suffix = pct_decode suffix in
  match handle with
  | "" -> suffix (* verbatim tag *)
  | _ -> (
      match List.assoc_opt handle directives.tags with
      | Some prefix -> prefix ^ suffix
      | None ->
          Types.parse_error pos
            "tag handle '%s' is not defined in this document" handle)

(* ------------------------------------------------------------------ *)
(* Anchor / tag / alias collection                                       *)
(* ------------------------------------------------------------------ *)

(** Collect consecutive ANCHOR and TAG tokens (in any order) and return them
    together with the position of the first. *)
let collect_node_properties p =
  let anchor = ref None in
  let tag = ref None in
  let start = ref None in
  let collecting = ref true in
  while !collecting do
    match peek_kind p with
    | Anchor name ->
        let tok_pos = (peek_tok p).tok_start_pos in
        if !start = None then start := Some tok_pos;
        if !anchor <> None then
          Types.parse_error tok_pos "a node cannot have two anchors";
        anchor := Some name;
        ignore (get_tok p)
    | Tag (handle, suffix) ->
        let tok = get_tok p in
        if !start = None then start := Some tok.tok_start_pos;
        tag := Some (resolve_tag p.directives tok.tok_start_pos handle suffix)
    | _ -> collecting := false
  done;
  (!anchor, !tag, !start)

(* ------------------------------------------------------------------ *)
(* Directive processing                                                  *)
(* ------------------------------------------------------------------ *)

let process_directives p =
  (* Reset directives to defaults for each new document *)
  p.directives <- default_directives ();
  let had_directives = ref false in
  let collecting = ref true in
  while !collecting do
    match peek_kind p with
    | Directive (name, value) -> (
        let tok = get_tok p in
        had_directives := true;
        match name with
        | "YAML" -> (
            if p.directives.version <> None then
              Types.parse_error tok.tok_start_pos
                "duplicate YAML directive (only one allowed per document)";
            let parts = String.split_on_char '.' value in
            match parts with
            | [ maj; min ] -> (
                try
                  p.directives.version <-
                    Some (int_of_string maj, int_of_string min)
                with
                | Failure _ -> ())
            | _ -> ())
        | handle when String.length handle >= 1 && handle.[0] = '!' ->
            (* TAG directive: name = handle, value = prefix *)
            p.directives.tags <-
              (handle, value)
              :: List_ext.filter (fun (h, _) -> h <> handle) p.directives.tags
        | _ -> ())
    | _ -> collecting := false
  done;
  !had_directives

(* ------------------------------------------------------------------ *)
(* Empty scalar helper                                                   *)
(* ------------------------------------------------------------------ *)

let empty_scalar pos =
  mk_event
    (Scalar { anchor = None; tag = None; value = ""; style = Plain })
    pos pos

(* ------------------------------------------------------------------ *)
(* Core state machine                                                    *)
(* ------------------------------------------------------------------ *)

(** Produce one event from the current state. Updates [p.state] to the next
    state. *)
let rec produce p =
  match p.state with
  (* ---- Stream ---- *)
  | Parse_stream_start ->
      let tok = get_tok p in
      (* STREAM_START *)
      p.state <- Parse_document_start_implicit;
      mk_event Stream_start tok.tok_start_pos tok.tok_end_pos
  | Parse_end ->
      let tok = peek_tok p in
      mk_event Stream_end tok.tok_start_pos tok.tok_end_pos
  (* ---- Document start ---- *)
  | Parse_document_start_implicit -> (
      let had_dir = process_directives p in
      let sp = (peek_tok p).tok_start_pos in
      match peek_kind p with
      | Directive _ -> assert false (* already processed *)
      | Document_start ->
          (* Explicit document start *)
          ignore (get_tok p);
          let ep = (peek_tok p).tok_start_pos in
          push_state p Parse_document_end;
          p.state <- Parse_document_content;
          mk_event
            (Document_start
               {
                 explicit = true;
                 version = p.directives.version;
                 tag_directives = p.directives.tags;
               })
            sp ep
      | Stream_end ->
          if had_dir then
            Types.parse_error sp "directive(s) not followed by a document";
          ignore (get_tok p);
          p.state <- Parse_end;
          mk_event Stream_end sp sp
      | Document_end ->
          if had_dir then
            Types.parse_error sp
              "directive(s) not followed by a document (unexpected '...')";
          (* Bare '...' at start of stream (no document open): skip it *)
          ignore (get_tok p);
          p.state <- Parse_document_start_implicit;
          produce p
      | _ ->
          (* Implicit document *)
          push_state p Parse_document_end;
          p.state <- Parse_block_node;
          mk_event
            (Document_start
               {
                 explicit = false;
                 version = None;
                 tag_directives = p.directives.tags;
               })
            sp sp)
  | Parse_document_start -> (
      let had_dir = process_directives p in
      let sp = (peek_tok p).tok_start_pos in
      match peek_kind p with
      | Stream_end ->
          if had_dir then
            Types.parse_error sp "directive(s) not followed by a document";
          ignore (get_tok p);
          p.state <- Parse_end;
          mk_event Stream_end sp sp
      | Document_end ->
          (* Bare '...' between documents (no new doc started yet): skip it *)
          ignore (get_tok p);
          p.state <- Parse_document_start;
          produce p
      | Document_start ->
          let tok = get_tok p in
          push_state p Parse_document_end;
          p.state <- Parse_document_content;
          mk_event
            (Document_start
               {
                 explicit = true;
                 version = p.directives.version;
                 tag_directives = p.directives.tags;
               })
            tok.tok_start_pos tok.tok_end_pos
      | _ ->
          (* Implicit document (content without '---') *)
          push_state p Parse_document_end;
          p.state <- Parse_block_node;
          mk_event
            (Document_start
               {
                 explicit = false;
                 version = p.directives.version;
                 tag_directives = p.directives.tags;
               })
            sp sp)
  (* ---- Document end ---- *)
  | Parse_document_end -> (
      match peek_kind p with
      | Document_end ->
          let tok = get_tok p in
          p.state <- Parse_document_start;
          mk_event
            (Document_end { explicit = true })
            tok.tok_start_pos tok.tok_end_pos
      | Directive _ ->
          (* A directive after a document requires an explicit '...' marker first *)
          let tok = peek_tok p in
          Types.parse_error tok.tok_start_pos
            "a directive must be preceded by a document-end marker ('...')"
      | Document_start
      | Stream_end ->
          let sp = (peek_tok p).tok_start_pos in
          p.state <- Parse_document_start;
          mk_event (Document_end { explicit = false }) sp sp
      | _ ->
          (* Anything else is unexpected extra content after the document's root node *)
          let tok = peek_tok p in
          Types.parse_error tok.tok_start_pos
            "unexpected content after document root node (missing '---' or \
             '...'?)")
  (* ---- Document content ---- *)
  | Parse_document_content -> (
      match peek_kind p with
      | Document_end
      | Stream_end ->
          p.state <- pop_state p;
          let sp = (peek_tok p).tok_start_pos in
          (* Empty document: emit an empty plain scalar as the document's node *)
          empty_scalar sp
      | _ ->
          p.state <- Parse_block_node;
          produce p)
  (* ---- Nodes ---- *)
  | Parse_block_node
  | Parse_block_node_or_indentless_sequence
  | Parse_flow_node ->
      (* NOTE: do NOT pop the state here.  parse_node pops it internally once
       it knows what the node is (scalar, sequence, mapping).  Popping early
       would discard the continuation state (e.g. Parse_document_end) before
       parse_node has a chance to use it. *)
      let allow_indentless =
        p.state = Parse_block_node_or_indentless_sequence
      in
      let in_flow = p.state = Parse_flow_node in
      parse_node p ~allow_indentless ~in_flow
  (* ---- Block sequences ---- *)
  | Parse_block_sequence_first_entry ->
      ignore (get_tok p);
      (* BLOCK_SEQUENCE_START *)
      p.state <- Parse_block_sequence_entry;
      produce p
  | Parse_block_sequence_entry -> (
      match peek_kind p with
      | Block_entry -> (
          let tok = get_tok p in
          (* BLOCK_ENTRY *)
          match peek_kind p with
          | Block_entry
          | Block_end ->
              p.state <- Parse_block_sequence_entry;
              (* Empty item *)
              empty_scalar tok.tok_end_pos
          | _ ->
              push_state p Parse_block_sequence_entry;
              p.state <- Parse_block_node;
              produce p)
      | Block_end ->
          let tok = get_tok p in
          p.state <- pop_state p;
          mk_event Sequence_end tok.tok_start_pos tok.tok_end_pos
      | kind ->
          Types.parse_error (peek_tok p).tok_start_pos
            "expected block sequence entry or BLOCK_END, got %s"
            (show_kind kind))
  (* ---- Indentless sequence (mapping value is an implicit sequence) ---- *)
  | Parse_indentless_sequence_entry -> (
      match peek_kind p with
      | Block_entry -> (
          let tok = get_tok p in
          match peek_kind p with
          | Block_entry
          | Key
          | Value
          | Block_end ->
              p.state <- Parse_indentless_sequence_entry;
              empty_scalar tok.tok_end_pos
          | _ ->
              push_state p Parse_indentless_sequence_entry;
              p.state <- Parse_block_node;
              produce p)
      | _ ->
          let sp = (peek_tok p).tok_start_pos in
          p.state <- pop_state p;
          mk_event Sequence_end sp sp)
  (* ---- Block mappings ---- *)
  | Parse_block_mapping_first_key ->
      ignore (get_tok p);
      (* BLOCK_MAPPING_START *)
      p.state <- Parse_block_mapping_key;
      produce p
  | Parse_block_mapping_key -> (
      match peek_kind p with
      | Key -> (
          let tok = get_tok p in
          match peek_kind p with
          | Key
          | Value
          | Block_end ->
              p.state <- Parse_block_mapping_value;
              empty_scalar tok.tok_end_pos
          | _ ->
              push_state p Parse_block_mapping_value;
              p.state <- Parse_block_node_or_indentless_sequence;
              produce p)
      | Block_end ->
          let tok = get_tok p in
          p.state <- pop_state p;
          mk_event Mapping_end tok.tok_start_pos tok.tok_end_pos
      | Value ->
          (* Implicit empty key: ': value' without a preceding '?' *)
          p.state <- Parse_block_mapping_value;
          let sp = (peek_tok p).tok_start_pos in
          empty_scalar sp
      | kind ->
          Types.parse_error (peek_tok p).tok_start_pos
            "expected block mapping key or BLOCK_END, got %s" (show_kind kind))
  | Parse_block_mapping_value -> (
      match peek_kind p with
      | Value -> (
          let tok = get_tok p in
          match peek_kind p with
          | Key
          | Value
          | Block_end ->
              p.state <- Parse_block_mapping_key;
              empty_scalar tok.tok_end_pos
          | Block_entry ->
              push_state p Parse_block_mapping_key;
              p.state <- Parse_indentless_sequence_entry;
              let sp = tok.tok_end_pos in
              mk_event
                (Sequence_start
                   { anchor = None; tag = None; implicit = true; flow = false })
                sp sp
          | _ ->
              push_state p Parse_block_mapping_key;
              p.state <- Parse_block_node_or_indentless_sequence;
              produce p)
      | _ ->
          (* Missing value: emit empty scalar *)
          p.state <- Parse_block_mapping_key;
          let sp = (peek_tok p).tok_start_pos in
          empty_scalar sp)
  (* ---- Flow sequences ---- *)
  | Parse_flow_sequence_first_entry ->
      let fs_tok = get_tok p in
      (* FLOW_SEQUENCE_START *)
      (* Leading comma: a comma immediately after '[' is invalid in YAML 1.2 *)
      (match peek_kind p with
      | Flow_entry ->
          Types.parse_error fs_tok.tok_end_pos
            "empty entry in flow sequence (unexpected leading comma)"
      | _ -> ());
      p.state <- Parse_flow_sequence_entry;
      produce p
  | Parse_flow_sequence_entry -> (
      (* Parses one entry; this state is entered at start or after consuming a comma.
       After the entry, Parse_flow_sequence_need_separator requires the next comma. *)
      match peek_kind p with
      | Flow_sequence_end ->
          let tok = get_tok p in
          p.state <- pop_state p;
          mk_event Sequence_end tok.tok_start_pos tok.tok_end_pos
      | Key ->
          (* Inline mapping inside a flow sequence: [key: val] style.
         Set state to need_separator so that after the mapping, a comma is required. *)
          let tok = get_tok p in
          p.state <- Parse_flow_sequence_entry_mapping_key;
          mk_event
            (Mapping_start
               { anchor = None; tag = None; implicit = true; flow = true })
            tok.tok_start_pos tok.tok_end_pos
      | Value ->
          (* Bare ':' with no preceding key → empty implicit key in flow sequence *)
          let sp = (peek_tok p).tok_start_pos in
          p.state <- Parse_flow_sequence_entry_mapping_key;
          mk_event
            (Mapping_start
               { anchor = None; tag = None; implicit = true; flow = true })
            sp sp
      | _ ->
          (* Parse one node; after it, a comma (or ']') is required *)
          push_state p Parse_flow_sequence_need_separator;
          p.state <- Parse_flow_node;
          produce p)
  | Parse_flow_sequence_need_separator -> (
      (* After an entry, require ',' or ']'; anything else is a missing-comma error. *)
      match peek_kind p with
      | Flow_sequence_end ->
          let tok = get_tok p in
          p.state <- pop_state p;
          mk_event Sequence_end tok.tok_start_pos tok.tok_end_pos
      | Flow_entry ->
          let comma_tok = get_tok p in
          (match peek_kind p with
          | Flow_entry ->
              Types.parse_error comma_tok.tok_end_pos
                "empty entry in flow sequence (unexpected consecutive comma)"
          | _ -> ());
          p.state <- Parse_flow_sequence_entry;
          produce p
      | _ ->
          let tok = peek_tok p in
          Types.parse_error tok.tok_start_pos
            "missing comma between flow sequence entries")
  | Parse_flow_sequence_entry_mapping_key -> (
      match peek_kind p with
      | Value
      | Flow_entry
      | Flow_sequence_end ->
          p.state <- Parse_flow_sequence_entry_mapping_value;
          let sp = (peek_tok p).tok_start_pos in
          empty_scalar sp
      | _ ->
          push_state p Parse_flow_sequence_entry_mapping_value;
          p.state <- Parse_flow_node;
          produce p)
  | Parse_flow_sequence_entry_mapping_value -> (
      match peek_kind p with
      | Value -> (
          ignore (get_tok p);
          match peek_kind p with
          | Flow_entry
          | Flow_sequence_end ->
              p.state <- Parse_flow_sequence_entry_mapping_end;
              let sp = (peek_tok p).tok_start_pos in
              empty_scalar sp
          | _ ->
              push_state p Parse_flow_sequence_entry_mapping_end;
              p.state <- Parse_flow_node;
              produce p)
      | _ ->
          p.state <- Parse_flow_sequence_entry_mapping_end;
          let sp = (peek_tok p).tok_start_pos in
          empty_scalar sp)
  | Parse_flow_sequence_entry_mapping_end ->
      p.state <- Parse_flow_sequence_need_separator;
      let sp = (peek_tok p).tok_start_pos in
      mk_event Mapping_end sp sp
  (* ---- Flow mappings ---- *)
  | Parse_flow_mapping_first_key ->
      ignore (get_tok p);
      (* FLOW_MAPPING_START *)
      p.state <- Parse_flow_mapping_key;
      produce p
  | Parse_flow_mapping_key -> (
      (* Parses one key; entered after '{' (first key) or after consuming a comma.
       After the key-value pair, Parse_flow_mapping_need_separator requires a comma. *)
      match peek_kind p with
      | Flow_mapping_end ->
          let tok = get_tok p in
          p.state <- pop_state p;
          mk_event Mapping_end tok.tok_start_pos tok.tok_end_pos
      | Key -> (
          let tok = get_tok p in
          match peek_kind p with
          | Value
          | Flow_entry
          | Flow_mapping_end ->
              p.state <- Parse_flow_mapping_value;
              empty_scalar tok.tok_end_pos
          | _ ->
              push_state p Parse_flow_mapping_value;
              p.state <- Parse_flow_node;
              produce p)
      | _ ->
          (* Implicit key in flow mapping (e.g., {foo: bar}) *)
          push_state p Parse_flow_mapping_value;
          p.state <- Parse_flow_node;
          produce p)
  | Parse_flow_mapping_need_separator -> (
      (* After a key-value pair: require ',' or '}'. *)
      match peek_kind p with
      | Flow_mapping_end ->
          let tok = get_tok p in
          p.state <- pop_state p;
          mk_event Mapping_end tok.tok_start_pos tok.tok_end_pos
      | Flow_entry ->
          ignore (get_tok p);
          p.state <- Parse_flow_mapping_key;
          produce p
      | _ ->
          let tok = peek_tok p in
          Types.parse_error tok.tok_start_pos
            "missing comma between flow mapping entries")
  | Parse_flow_mapping_value -> (
      match peek_kind p with
      | Value -> (
          ignore (get_tok p);
          match peek_kind p with
          | Flow_entry
          | Flow_mapping_end ->
              p.state <- Parse_flow_mapping_need_separator;
              let sp = (peek_tok p).tok_start_pos in
              empty_scalar sp
          | _ ->
              push_state p Parse_flow_mapping_need_separator;
              p.state <- Parse_flow_node;
              produce p)
      | _ ->
          p.state <- Parse_flow_mapping_need_separator;
          let sp = (peek_tok p).tok_start_pos in
          empty_scalar sp)

(** Parse a node (scalar, collection, or alias). Handles properties (anchor /
    tag) and dispatches to the right collection or scalar production. *)
and parse_node p ~allow_indentless ~in_flow:_ =
  let sp = (peek_tok p).tok_start_pos in
  (* Handle alias *)
  if
    check p [ Alias "" ] |> fun _ ->
    match peek_kind p with
    | Alias _ -> true
    | _ -> false
  then begin
    let tok = get_tok p in
    let name =
      match tok.tok_kind with
      | Alias n -> n
      | _ -> assert false
    in
    p.state <- pop_state p;
    mk_event (Alias name) tok.tok_start_pos tok.tok_end_pos
  end
  else begin
    let anchor, tag, prop_start = collect_node_properties p in
    let node_start =
      match prop_start with
      | Some s -> s
      | None -> sp
    in
    match peek_kind p with
    | Block_sequence_start ->
        p.state <- Parse_block_sequence_first_entry;
        let ev_sp = (peek_tok p).tok_start_pos in
        mk_event
          (Sequence_start { anchor; tag; implicit = tag = None; flow = false })
          node_start ev_sp
    | Block_mapping_start ->
        p.state <- Parse_block_mapping_first_key;
        let ev_sp = (peek_tok p).tok_start_pos in
        mk_event
          (Mapping_start { anchor; tag; implicit = tag = None; flow = false })
          node_start ev_sp
    | Flow_sequence_start ->
        let ev_sp = (peek_tok p).tok_start_pos in
        p.state <- Parse_flow_sequence_first_entry;
        mk_event
          (Sequence_start { anchor; tag; implicit = tag = None; flow = true })
          node_start ev_sp
    | Flow_mapping_start ->
        let ev_sp = (peek_tok p).tok_start_pos in
        p.state <- Parse_flow_mapping_first_key;
        mk_event
          (Mapping_start { anchor; tag; implicit = tag = None; flow = true })
          node_start ev_sp
    | Block_entry when allow_indentless ->
        (* Indentless sequence: a sequence that starts at the current indent.
         The continuation (e.g. Parse_block_mapping_key) is already on the
         state stack from the caller; do NOT push again. *)
        let ev_sp = (peek_tok p).tok_start_pos in
        p.state <- Parse_indentless_sequence_entry;
        mk_event
          (Sequence_start { anchor; tag; implicit = tag = None; flow = false })
          node_start ev_sp
    | Scalar (value, style) ->
        let tok = get_tok p in
        p.state <- pop_state p;
        mk_event
          (Scalar { anchor; tag; value; style })
          tok.tok_start_pos tok.tok_end_pos
    | _ -> (
        (* Empty node: emit empty plain scalar *)
        p.state <- pop_state p;
        match (anchor, tag) with
        | None, None -> empty_scalar node_start
        | _ ->
            (* Anchor/tag with no value: empty scalar *)
            mk_event
              (Scalar { anchor; tag; value = ""; style = Plain })
              node_start node_start)
  end

and show_kind = function
  | Stream_start -> "STREAM_START"
  | Stream_end -> "STREAM_END"
  | Directive _ -> "DIRECTIVE"
  | Document_start -> "DOCUMENT_START"
  | Document_end -> "DOCUMENT_END"
  | Block_sequence_start -> "BLOCK_SEQUENCE_START"
  | Block_mapping_start -> "BLOCK_MAPPING_START"
  | Block_end -> "BLOCK_END"
  | Flow_sequence_start -> "FLOW_SEQUENCE_START"
  | Flow_sequence_end -> "FLOW_SEQUENCE_END"
  | Flow_mapping_start -> "FLOW_MAPPING_START"
  | Flow_mapping_end -> "FLOW_MAPPING_END"
  | Block_entry -> "BLOCK_ENTRY"
  | Flow_entry -> "FLOW_ENTRY"
  | Key -> "KEY"
  | Value -> "VALUE"
  | Alias _ -> "ALIAS"
  | Anchor _ -> "ANCHOR"
  | Tag _ -> "TAG"
  | Scalar _ -> "SCALAR"

(* ------------------------------------------------------------------ *)
(* Public interface                                                      *)
(* ------------------------------------------------------------------ *)

(** Return (without consuming) the next event. Produces a new event if the
    buffer is empty. *)
let peek_event (p : t) : event =
  match p.events with
  | ev :: _ -> ev
  | [] ->
      let ev = produce p in
      p.events <- [ ev ];
      ev

(** Consume and return the next event. *)
let get_event (p : t) : event =
  match p.events with
  | ev :: rest ->
      p.events <- rest;
      ev
  | [] -> produce p

(** Check whether the next event's kind is in the list. Uses structural equality
    on the constructors only (ignores fields). *)
let check_event (p : t) (kinds : event_kind list) : bool =
  let ev = peek_event p in
  List.exists
    (fun k ->
      match (ev.kind, k) with
      | Stream_start, Stream_start -> true
      | Stream_end, Stream_end -> true
      | Document_start _, Document_start _ -> true
      | Document_end _, Document_end _ -> true
      | Mapping_start _, Mapping_start _ -> true
      | Mapping_end, Mapping_end -> true
      | Sequence_start _, Sequence_start _ -> true
      | Sequence_end, Sequence_end -> true
      | Scalar _, Scalar _ -> true
      | Alias _, Alias _ -> true
      | _ -> false)
    kinds

(** Expose the underlying scanner, e.g. to drain accumulated comments after
    parsing is complete. *)
let get_scanner (p : t) : Scanner.state = p.scanner

(** Collect all events into a list. This is a convenience for tests. *)
let to_event_list (p : t) : event list =
  let result = ref [] in
  let stop = ref false in
  while not !stop do
    let ev = get_event p in
    result := ev :: !result;
    match ev.kind with
    | Stream_end -> stop := true
    | _ -> ()
  done;
  List.rev !result
