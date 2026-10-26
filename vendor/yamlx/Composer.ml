(** YAML Composer. Builds an in-memory AST (Types.node) from the Parser's event
    stream. The Composer resolves anchor/alias references: when an alias is
    encountered the already-composed node for that anchor is substituted in
    place.

    Note that the YAML specification allows forward aliases (an alias to a node
    declared later), but this is extremely rare and hard to support without
    breaking the single-pass model. We require anchors to be declared before
    they are used, which is what all common YAML documents do. *)

open Types

(* ------------------------------------------------------------------ *)
(* Composer state                                                        *)
(* ------------------------------------------------------------------ *)

type t = {
  parser_ : Parser.t;
  anchors : (string, node Lazy.t ref) Hashtbl.t;
  max_depth : int;
}

let create ?(max_depth = Types.default_max_depth) (parser_ : Parser.t) : t =
  { parser_; anchors = Hashtbl.create 16; max_depth }

(* ------------------------------------------------------------------ *)
(* Helpers                                                               *)
(* ------------------------------------------------------------------ *)

let get_ev c = Parser.get_event c.parser_

(** Pre-register an anchor cell before the node is fully composed, so that
    aliases inside the node can resolve to it. Returns the cell so it can be
    filled after composition. *)
let prereq_anchor c anchor =
  match anchor with
  | None -> None
  | Some name ->
      let cell = ref (lazy (assert false)) in
      Hashtbl.replace c.anchors name cell;
      Some cell

(** Fill an anchor cell with the completed node. *)
let fill_anchor cell_opt node =
  Option.iter (fun cell -> cell := Lazy.from_val node) cell_opt

(* ------------------------------------------------------------------ *)
(* Node composition                                                      *)
(* ------------------------------------------------------------------ *)

(** Extract the height stored in any node variant. *)
let node_height : node -> int = function
  | Scalar_node r -> r.height
  | Sequence_node r -> r.height
  | Mapping_node r -> r.height
  | Alias_node r -> r.height

(** Compose a single node from the next event(s) in the stream. [depth] is the
    current nesting depth (1 at document level). Raises
    {!Types.Depth_limit_exceeded} when [depth] exceeds [c.max_depth].
    Precondition: the next event is the *start* of a node (i.e. not a *_end or
    document event). *)
let rec compose_node (c : t) ~(depth : int) : node =
  let ev = get_ev c in
  if depth > c.max_depth then
    raise (Types.Error (Types.Depth_limit_exceeded c.max_depth));
  match ev.kind with
  | Alias name -> (
      match Hashtbl.find_opt c.anchors name with
      | Some cell ->
          Alias_node
            {
              name;
              resolved = Lazy.from_fun (fun () -> Lazy.force !cell);
              loc = { start_pos = ev.start_pos; end_pos = ev.end_pos };
              height = 1;
              head_comments = [];
              line_comment = None;
              foot_comments = [];
            }
      | None -> Types.parse_error ev.start_pos "undefined alias '*%s'" name)
  | Scalar { anchor; tag; value; style } ->
      let cell = prereq_anchor c anchor in
      let node =
        Scalar_node
          {
            anchor;
            tag;
            value;
            style;
            loc = { start_pos = ev.start_pos; end_pos = ev.end_pos };
            height = 1;
            head_comments = [];
            line_comment = None;
            foot_comments = [];
          }
      in
      fill_anchor cell node;
      node
  | Sequence_start { anchor; tag; flow; _ } ->
      let cell = prereq_anchor c anchor in
      let start_pos = ev.start_pos in
      let end_pos = ref ev.end_pos in
      let items = ref [] in
      let stop = ref false in
      while not !stop do
        let next = Parser.peek_event c.parser_ in
        match next.kind with
        | Sequence_end ->
            let end_ev = get_ev c in
            end_pos := end_ev.end_pos;
            stop := true
        | _ -> items := compose_node c ~depth:(depth + 1) :: !items
      done;
      let items_list = List.rev !items in
      let h =
        1 + List.fold_left (fun acc n -> max acc (node_height n)) 0 items_list
      in
      let node =
        Sequence_node
          {
            anchor;
            tag;
            items = items_list;
            flow;
            loc = { start_pos; end_pos = !end_pos };
            height = h;
            head_comments = [];
            line_comment = None;
            foot_comments = [];
          }
      in
      fill_anchor cell node;
      node
  | Mapping_start { anchor; tag; flow; _ } ->
      let cell = prereq_anchor c anchor in
      let start_pos = ev.start_pos in
      let end_pos = ref ev.end_pos in
      let pairs = ref [] in
      let stop = ref false in
      while not !stop do
        let next = Parser.peek_event c.parser_ in
        match next.kind with
        | Mapping_end ->
            let end_ev = get_ev c in
            end_pos := end_ev.end_pos;
            stop := true
        | _ ->
            let key = compose_node c ~depth:(depth + 1) in
            let value = compose_node c ~depth:(depth + 1) in
            pairs := (key, value) :: !pairs
      done;
      let pairs_list = List.rev !pairs in
      let h =
        1
        + List.fold_left
            (fun acc (k, v) -> max acc (max (node_height k) (node_height v)))
            0 pairs_list
      in
      let node =
        Mapping_node
          {
            anchor;
            tag;
            pairs = pairs_list;
            flow;
            loc = { start_pos; end_pos = !end_pos };
            height = h;
            head_comments = [];
            line_comment = None;
            foot_comments = [];
          }
      in
      fill_anchor cell node;
      node
  | _ ->
      Types.parse_error ev.start_pos
        "compose_node: unexpected event kind at this position"

(** Compose a complete YAML document. Returns the document's [%YAML] version
    directive (if any) paired with the composed node. *)
let compose_document (c : t) : (int * int) option * node =
  (* Anchors are document-local: clear any anchors from previous documents. *)
  Hashtbl.clear c.anchors;
  (* Consume DOCUMENT_START and capture the version directive. *)
  let start_ev = get_ev c in
  let version =
    match start_ev.kind with
    | Document_start { version; _ } -> version
    | _ -> Types.parse_error start_ev.start_pos "expected DOCUMENT_START"
  in
  let node = compose_node c ~depth:1 in
  (* Consume DOCUMENT_END *)
  let end_ev = get_ev c in
  (match end_ev.kind with
  | Document_end _ -> ()
  | _ -> Types.parse_error end_ev.start_pos "expected DOCUMENT_END");
  (version, node)

(** Compose all documents in a YAML stream. Returns one [(version, node)] pair
    per document, where [version] is the value of the [%YAML] directive if
    present. *)
let compose_stream_with_starts (c : t) :
    ((int * int) option * node) list * int list =
  (* Consume STREAM_START *)
  let start_ev = get_ev c in
  (match start_ev.kind with
  | Stream_start -> ()
  | _ -> Types.parse_error start_ev.start_pos "expected STREAM_START");
  let docs = ref [] in
  let doc_start_lines = ref [] in
  let stop = ref false in
  while not !stop do
    let next = Parser.peek_event c.parser_ in
    match next.kind with
    | Stream_end ->
        ignore (get_ev c);
        stop := true
    | Document_start _ ->
        doc_start_lines := next.start_pos.line :: !doc_start_lines;
        docs := compose_document c :: !docs
    | _ ->
        Types.parse_error next.start_pos "expected DOCUMENT_START or STREAM_END"
  done;
  (List.rev !docs, List.rev !doc_start_lines)

(** Compose all documents in a YAML stream. Returns one [(version, node)] pair
    per document, where [version] is the value of the [%YAML] directive if
    present. *)
let compose_stream (c : t) : ((int * int) option * node) list =
  fst (compose_stream_with_starts c)
