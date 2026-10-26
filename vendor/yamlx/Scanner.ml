(** YAML 1.2 scanner (tokenizer). Transforms a character stream (Reader) into a
    token stream consumed by the Parser. This is by far the most complex module
    in the library. *)

(*
    Architecture
    ~~~~~~~~~~~~
    The scanner maintains a *token queue* of buffered tokens not yet delivered
    to the parser.  Tokens are produced lazily: [get_token] calls
    [fetch_more_tokens] as needed, which dispatches on the current character
    and appends one or more tokens to the queue.

    Indentation
    ~~~~~~~~~~~
    Block collections (sequences and mappings) are delimited by indentation
    rather than explicit brackets.  The scanner tracks the current indent level
    and emits synthetic BLOCK_SEQUENCE_START, BLOCK_MAPPING_START, and
    BLOCK_END tokens when indentation changes.

    Simple keys
    ~~~~~~~~~~~
    YAML allows implicit mapping keys: [foo: bar] where [foo] is a key without
    a leading [?] marker.  When the scanner emits a scalar (or anchor, alias,
    or flow collection start) it saves a 'possible simple key' entry for the
    current flow level.  If a [:] is later seen, the saved entry is confirmed
    and a KEY token (plus BLOCK_MAPPING_START if needed) is retroactively
    inserted before the scalar.  Otherwise the entry expires.

    This module follows the algorithm of ruamel-yaml (Python), translated to
    idiomatic OCaml.  Differences from ruamel-yaml are noted inline.
*)

open Types

(* ------------------------------------------------------------------ *)
(* Token queue (small doubly-ended list, efficient push/pop at front)   *)
(* ------------------------------------------------------------------ *)

(** Tokens are kept in a simple list; [head] is the next token to return. The
    list is short in practice (usually < 5 tokens), so O(n) operations are
    negligible. *)

type state = {
  reader : Reader.t;
  (* Token buffer — a FIFO queue for O(1) push (back) and pop (front). *)
  tokens : token Queue.t;
  mutable tokens_taken : int;  (** total tokens returned so far *)
  (* Indentation tracking for block context *)
  mutable indent : int;  (** current block indent, -1 at stream start *)
  mutable indents : int list;  (** indent stack *)
  (* Flow context depth (0 = block) *)
  mutable flow_level : int;
  (* Stack of flow collection types: true = sequence, false = mapping *)
  mutable flow_is_sequence : bool list;
  (* Simple key tracking *)
  mutable allow_simple_key : bool;
  possible_simple_keys : (int, simple_key) Hashtbl.t;
      (** Possible simple key for each flow level. Key = flow_level; value =
          info about the pending candidate. *)
  mutable min_simple_key_token : int;
      (** Minimum [sk_token_number] across all entries in
          [possible_simple_keys], or [max_int] when the table is empty.
          Maintained incrementally so that {!earliest_possible_simple_key} and
          the early-exit guard in {!stale_possible_simple_keys} are both O(1)
          instead of O(n). *)
  (* Stream lifecycle *)
  mutable done_ : bool;
  mutable stream_start_produced : bool;
  (* Comment side-channel (block context only) *)
  comments : (int * int * bool * string) Queue.t;
      (** [(source_line, source_col, is_line_comment, text)] tuples collected
          while scanning. [source_col] is the 0-based column of the ['#'].
          [is_line_comment = true] means the ['#'] was preceded only by spaces
          on the same line (trailing/line comment); [false] means a newline was
          consumed before the ['#'] (standalone head comment). Text does not
          include the leading ['#'] character or the optional space that follows
          it. Comments inside flow collections ([flow_level > 0]) are discarded.
      *)
}

and simple_key = {
  sk_token_number : int;  (** absolute token index in the stream *)
  sk_required : bool;  (** error if not resolved before becoming stale *)
  sk_pos : pos;
}

(* ------------------------------------------------------------------ *)
(* Helpers                                                              *)
(* ------------------------------------------------------------------ *)

let make_token (tok_kind : token_kind) tok_start_pos tok_end_pos : token =
  { tok_kind; tok_start_pos; tok_end_pos }

(** Append a token to the back of the scanner's token queue. O(1). *)
let push_token scn tok = Queue.add tok scn.tokens

(** Insert a token at position [pos] (0 = front). Converts the queue to a list,
    inserts, then rebuilds. O(n) but called only for BLOCK_*_START insertion and
    retroactive KEY insertion, which are rare. *)
let insert_token scn pos tok =
  let lst = Queue.fold (fun acc t -> t :: acc) [] scn.tokens |> List.rev in
  let rec go i = function
    | tl when i = pos -> tok :: tl
    | t :: rest -> t :: go (i + 1) rest
    | [] -> [ tok ]
  in
  let new_lst = go 0 lst in
  Queue.clear scn.tokens;
  List.iter (fun t -> Queue.add t scn.tokens) new_lst

(** Insert a BLOCK_*_START token after any leading ANCHOR/TAG tokens that are
    already queued. ANCHOR and TAG are node properties that must precede the
    collection-start event, so when the block indicator (- or ?) is encountered
    the START token must be placed *after* those. *)
let insert_collection_start scn tok =
  (* Count leading ANCHOR/TAG tokens from the queue front using a temporary seq *)
  let pos =
    let n = ref 0 in
    (try
       Queue.iter
         (fun t ->
           match t.tok_kind with
           | Anchor _
           | Tag _ ->
               incr n
           | _ -> raise Exit)
         scn.tokens
     with
    | Exit -> ());
    !n
  in
  insert_token scn pos tok

(** Return the number of buffered tokens that haven't been returned yet. O(1).
*)
let pending_count scn = Queue.length scn.tokens

(** The absolute token number that the *next* pushed token will receive. *)
let next_token_number scn = scn.tokens_taken + pending_count scn

(* ------------------------------------------------------------------ *)
(* Reader helpers                                                       *)
(* ------------------------------------------------------------------ *)

let peek scn i = Reader.peek scn.reader i
let advance scn n = Reader.advance scn.reader n
let pos scn = Reader.pos scn.reader
let column scn = (pos scn).column

(** Consume one line-break (LF, already normalized by Reader) and update
    allow_simple_key. *)
let scan_line_break scn =
  let cp = peek scn 0 in
  if cp = 0x0A (* LF *) then begin
    advance scn 1;
    scn.allow_simple_key <- true;
    "\n"
  end
  else ""

(* ------------------------------------------------------------------ *)
(* Simple key min-tracker helper (defined early; used by indentation    *)
(* management and the full simple key section below)                    *)
(* ------------------------------------------------------------------ *)

(** Recompute [min_simple_key_token] from scratch by scanning the whole table.
    Called after any operation that may have removed the current minimum. O(n)
    where n = number of entries, but only invoked when an entry is actually
    deleted. *)
let recompute_min_simple_key_token scn =
  scn.min_simple_key_token <-
    Hashtbl.fold
      (fun _level sk acc -> min sk.sk_token_number acc)
      scn.possible_simple_keys max_int

(* ------------------------------------------------------------------ *)
(* Indentation management                                               *)
(* ------------------------------------------------------------------ *)

(** Emit BLOCK_END tokens for every indentation level that is deeper than [col].
    Called before each token when in block context. *)
let unwind_indent scn col =
  if scn.flow_level = 0 then begin
    let closed = ref false in
    while scn.indent > col do
      closed := true;
      let tok = make_token Block_end (pos scn) (pos scn) in
      push_token scn tok;
      scn.indent <-
        (match scn.indents with
        | hd :: tl ->
            scn.indents <- tl;
            hd
        | [] -> -1)
    done;
    (* When blocks were closed, discard simple-key candidates that belong to
       the now-closed indentation levels (they can never be confirmed). *)
    if !closed then begin
      Hashtbl.filter_map_inplace
        (fun _level sk ->
          if sk.sk_pos.column > scn.indent then None else Some sk)
        scn.possible_simple_keys;
      recompute_min_simple_key_token scn
    end
  end

(** Try to start a new indentation level at [col]. Returns [true] and updates
    the indent stack if [col > scn.indent]. When [true] is returned the caller
    should emit a BLOCK_*_START token. *)
let add_indent scn col =
  if scn.indent < col then begin
    scn.indents <- scn.indent :: scn.indents;
    scn.indent <- col;
    true
  end
  else false

(* ------------------------------------------------------------------ *)
(* Simple key management                                                *)
(* ------------------------------------------------------------------ *)

(** Remove any simple key that has become stale (i.e. its token has already been
    returned to the parser, or it spans a line boundary in a context that
    forbids multi-line keys). *)
let stale_possible_simple_keys scn =
  (* Fast path: if no key's token number is less than tokens_taken, and no
     block-context line boundary can have been crossed (level-0 key's line is
     still the current line), nothing is stale — skip the O(n) scan entirely. *)
  if
    scn.tokens_taken <= scn.min_simple_key_token
    && not
         (match Hashtbl.find_opt scn.possible_simple_keys 0 with
         | Some sk -> (pos scn).line > sk.sk_pos.line
         | None -> false)
  then ()
  else begin
    let removed_min = ref false in
    Hashtbl.filter_map_inplace
      (fun level sk ->
        if sk.sk_token_number < scn.tokens_taken then begin
          (* The token was already returned; the key window has closed. *)
          if sk.sk_required then
            Types.scan_error sk.sk_pos "could not find expected ':'";
          if sk.sk_token_number = scn.min_simple_key_token then
            removed_min := true;
          None
        end
        else if level = 0 && (pos scn).line > sk.sk_pos.line then begin
          (* In block context, an implicit key candidate must be on a single line.
           If we have moved to a later line without seeing ':', the candidate
           is stale.  Required keys in this situation are a hard error. *)
          if sk.sk_required then
            Types.scan_error sk.sk_pos "could not find expected ':'";
          if sk.sk_token_number = scn.min_simple_key_token then
            removed_min := true;
          None
        end
        else Some sk)
      scn.possible_simple_keys;
    if !removed_min then recompute_min_simple_key_token scn
  end

(** Save the current position as a potential simple key for the current flow
    level. *)
let save_possible_simple_key scn =
  if scn.allow_simple_key then begin
    (* A required key: at indent == column and in block context *)
    let required = scn.flow_level = 0 && scn.indent = column scn in
    let token_num = next_token_number scn in
    Hashtbl.replace scn.possible_simple_keys scn.flow_level
      { sk_token_number = token_num; sk_required = required; sk_pos = pos scn };
    if token_num < scn.min_simple_key_token then
      scn.min_simple_key_token <- token_num
  end

(** Remove the simple key candidate for the current flow level (called when we
    know the current context cannot produce a key). *)
let remove_possible_simple_key scn =
  match Hashtbl.find_opt scn.possible_simple_keys scn.flow_level with
  | Some sk when sk.sk_required ->
      Types.scan_error sk.sk_pos "could not find expected ':'"
  | Some sk ->
      Hashtbl.remove scn.possible_simple_keys scn.flow_level;
      if sk.sk_token_number = scn.min_simple_key_token then
        recompute_min_simple_key_token scn
  | None -> ()

(** True if we need more tokens before we can safely return the next one. This
    is the case when a pending simple key's token is the very next token to be
    returned (we cannot return it without first deciding whether it is a key).
*)
let need_more_tokens scn =
  if scn.done_ then false
  else if Queue.is_empty scn.tokens then true
  else scn.min_simple_key_token = scn.tokens_taken

(* ------------------------------------------------------------------ *)
(* Whitespace / comment skipping                                        *)
(* ------------------------------------------------------------------ *)

(** Skip spaces (and tabs in flow context or after checking) and comments. After
    each line break [allow_simple_key] is set to [true]. *)
let scan_to_next_token scn =
  let found = ref false in
  (* Tracks whether the current position is on a fresh line (no token yet on
     this line).  Initialized to [true] when we are at column 0 so that a '#'
     at the very start of the file, or right after a linebreak, is correctly
     classified as a head comment rather than a line comment. *)
  let had_newline = ref ((pos scn).column = 0) in
  while not !found do
    (* Skip spaces.  Tabs are never allowed as indentation in block context
       but are allowed as separation in flow context and after some tokens.
       We allow them here only when inside a flow collection. *)
    let in_flow = scn.flow_level > 0 in
    let had_white = ref ((pos scn).column = 0) in
    let continue_spaces = ref true in
    while !continue_spaces do
      let cp = peek scn 0 in
      if cp = 0x20 (* SPC *) then begin
        advance scn 1;
        had_white := true
      end
      else if cp = 0x09 (* TAB *) && (pos scn).column > 0 then begin
        (* Tabs are allowed as separation whitespace when not at the start of
           a line.  They are forbidden as indentation (column = 0) in both
           block and flow context. *)
        advance scn 1;
        had_white := true;
        (* In block context a tab invalidates any pending simple-key candidate:
           a token that follows a tab cannot serve as an implicit mapping key
           because tab-based indentation is not permitted. *)
        if not in_flow then scn.allow_simple_key <- false
      end
      else continue_spaces := false
    done;
    (* A '#' starts a comment only when preceded by whitespace (or at col 0).
       A '#' immediately after a token with no intervening space is not a
       comment and must be treated as an error character by the dispatcher. *)
    if peek scn 0 = 0x23 (* # *) && !had_white then
      begin if scn.flow_level = 0 then begin
        (* Block context: capture the comment text. *)
        let comment_line = (pos scn).line in
        let comment_col = (pos scn).column in
        let is_line_comment = not !had_newline in
        advance scn 1;
        (* consume '#' *)
        let buf = Buffer.create 64 in
        let continue_comment = ref true in
        while !continue_comment do
          let cp = peek scn 0 in
          if cp = Char_class.eof || Char_class.is_linebreak cp then
            continue_comment := false
          else begin
            Reader.encode_utf8_to buf cp;
            advance scn 1
          end
        done;
        Queue.add
          (comment_line, comment_col, is_line_comment, Buffer.contents buf)
          scn.comments
      end
      else begin
        (* Flow context: discard the comment as before. *)
        let continue_comment = ref true in
        while !continue_comment do
          let cp = peek scn 0 in
          if cp = Char_class.eof || Char_class.is_linebreak cp then
            continue_comment := false
          else advance scn 1
        done
      end
      end;
    (* Consume a line break if present *)
    let cp = peek scn 0 in
    if Char_class.is_linebreak cp then begin
      ignore (scan_line_break scn);
      (* Inside a block scalar or block mapping, tabs immediately following
         a newline reset the column to 0 before we look for the next token.
         allow_simple_key was already set by scan_line_break. *)
      if scn.flow_level = 0 then scn.allow_simple_key <- true;
      had_newline := true
    end
    else begin
      had_newline := false;
      found := true
    end
  done

(* ------------------------------------------------------------------ *)
(* Directive scanning                                                   *)
(* ------------------------------------------------------------------ *)

(** Scan a [%YAML] version directive value and return [(major, minor)]. *)
let scan_yaml_directive_value scn start =
  (* Skip whitespace between '%YAML' and the version *)
  while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
    advance scn 1
  done;
  let scan_num () =
    let buf = Buffer.create 4 in
    let cp = ref (peek scn 0) in
    if not (Char_class.is_digit !cp) then
      Types.scan_error (pos scn) "expected a digit in YAML version";
    while Char_class.is_digit !cp do
      Reader.encode_utf8_to buf !cp;
      advance scn 1;
      cp := peek scn 0
    done;
    int_of_string (Buffer.contents buf)
  in
  let major = scan_num () in
  if peek scn 0 <> 0x2E (* . *) then
    Types.scan_error (pos scn) "expected '.' in YAML version";
  advance scn 1;
  let minor = scan_num () in
  (* After the version, only whitespace followed by an optional comment is allowed *)
  let had_white = ref false in
  while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
    advance scn 1;
    had_white := true
  done;
  let cp = peek scn 0 in
  if cp = 0x23 (* # *) && !had_white then
    while
      (not (Char_class.is_linebreak (peek scn 0)))
      && peek scn 0 <> Char_class.eof
    do
      advance scn 1
    done
  else if not (Char_class.is_linebreak cp || cp = Char_class.eof) then
    Types.scan_error (pos scn) "unexpected extra content on YAML directive line";
  let value = Printf.sprintf "%d.%d" major minor in
  let tok = make_token (Directive ("YAML", value)) start (pos scn) in
  push_token scn tok

(** Scan a [%TAG] directive and push its token. *)
let scan_tag_directive_value scn start =
  (* handle *)
  while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
    advance scn 1
  done;
  let handle_buf = Buffer.create 8 in
  (* tag handle starts with '!' *)
  if peek scn 0 <> 0x21 then
    Types.scan_error (pos scn) "expected '!' to start tag handle";
  Buffer.add_char handle_buf '!';
  advance scn 1;
  let cp = ref (peek scn 0) in
  while Char_class.is_letter !cp || Char_class.is_digit !cp || !cp = 0x2D do
    Reader.encode_utf8_to handle_buf !cp;
    advance scn 1;
    cp := peek scn 0
  done;
  (* Consume the closing '!' if present (secondary handle '!!' or named
     '!foo!').  If the next character is not '!', this is the primary
     handle '!' alone, which has no closing '!'. *)
  if peek scn 0 = 0x21 then begin
    Buffer.add_char handle_buf '!';
    advance scn 1
  end
  else if Buffer.length handle_buf > 1 then
    (* Named handle without closing '!' is an error *)
    Types.scan_error (pos scn) "expected '!' to end tag handle";
  (* prefix *)
  while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
    advance scn 1
  done;
  let prefix_buf = Buffer.create 32 in
  let continue_prefix = ref true in
  while !continue_prefix do
    let c = peek scn 0 in
    if c = Char_class.eof || Char_class.is_linebreak c then
      continue_prefix := false
    else begin
      Reader.encode_utf8_to prefix_buf c;
      advance scn 1
    end
  done;
  let tok =
    make_token
      (Directive (Buffer.contents handle_buf, Buffer.contents prefix_buf))
      start (pos scn)
  in
  push_token scn tok

(** Fetch and push a [%YAML] or [%TAG] directive token. *)
let fetch_directive scn =
  let start = pos scn in
  unwind_indent scn (-1);
  remove_possible_simple_key scn;
  scn.allow_simple_key <- false;
  advance scn 1;
  (* consume '%' *)
  (* Read the directive name *)
  let name_buf = Buffer.create 8 in
  let cp = ref (peek scn 0) in
  while Char_class.is_letter !cp || !cp = 0x2D (* - *) do
    Reader.encode_utf8_to name_buf !cp;
    advance scn 1;
    cp := peek scn 0
  done;
  let name = Buffer.contents name_buf in
  match name with
  | "YAML" -> scan_yaml_directive_value scn start
  | "TAG" -> scan_tag_directive_value scn start
  | _ ->
      (* Unknown directive: consume and ignore per spec *)
      let value_buf = Buffer.create 32 in
      while Char_class.is_directive_char (peek scn 0) do
        Reader.encode_utf8_to value_buf (peek scn 0);
        advance scn 1
      done;
      push_token scn
        (make_token
           (Directive (name, Buffer.contents value_buf))
           start (pos scn))

(* ------------------------------------------------------------------ *)
(* Document markers                                                     *)
(* ------------------------------------------------------------------ *)

let fetch_document_start scn =
  let start = pos scn in
  if scn.flow_level > 0 then
    Types.scan_error start
      "'---' document-start marker is not allowed inside a flow collection";
  unwind_indent scn (-1);
  Hashtbl.clear scn.possible_simple_keys;
  scn.min_simple_key_token <- max_int;
  scn.allow_simple_key <- false;
  advance scn 3;
  (* consume '---' *)
  push_token scn (make_token Document_start start (pos scn))

let fetch_document_end scn =
  let start = pos scn in
  if scn.flow_level > 0 then
    Types.scan_error start
      "'...' document-end marker is not allowed inside a flow collection";
  unwind_indent scn (-1);
  Hashtbl.clear scn.possible_simple_keys;
  scn.min_simple_key_token <- max_int;
  scn.allow_simple_key <- false;
  advance scn 3;
  (* consume '...' *)
  (* YAML spec: the document-end marker must be followed only by whitespace
     and optionally a comment, then a line break (or end of stream).
     Any other content on the same line is invalid. *)
  let had_white = ref false in
  while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
    advance scn 1;
    had_white := true
  done;
  let cp = peek scn 0 in
  if cp = 0x23 (* # *) && !had_white then
    (* Valid trailing comment: consume to end of line *)
    while
      (not (Char_class.is_linebreak (peek scn 0)))
      && peek scn 0 <> Char_class.eof
    do
      advance scn 1
    done
  else if not (Char_class.is_linebreak cp || cp = Char_class.eof) then
    Types.scan_error (pos scn)
      "invalid content after document-end marker (expected end of line)";
  push_token scn (make_token Document_end start (pos scn))

(* ------------------------------------------------------------------ *)
(* Flow collection tokens                                               *)
(* ------------------------------------------------------------------ *)

let fetch_flow_sequence_start scn =
  let start = pos scn in
  save_possible_simple_key scn;
  scn.flow_level <- scn.flow_level + 1;
  scn.flow_is_sequence <- true :: scn.flow_is_sequence;
  scn.allow_simple_key <- true;
  advance scn 1;
  push_token scn (make_token Flow_sequence_start start (pos scn))

let fetch_flow_mapping_start scn =
  let start = pos scn in
  save_possible_simple_key scn;
  scn.flow_level <- scn.flow_level + 1;
  scn.flow_is_sequence <- false :: scn.flow_is_sequence;
  scn.allow_simple_key <- true;
  advance scn 1;
  push_token scn (make_token Flow_mapping_start start (pos scn))

let fetch_flow_sequence_end scn =
  let start = pos scn in
  if scn.flow_level = 0 then
    Types.scan_error start "unexpected ']' outside of a flow sequence";
  remove_possible_simple_key scn;
  scn.flow_level <- scn.flow_level - 1;
  scn.flow_is_sequence <-
    (match scn.flow_is_sequence with
    | _ :: t -> t
    | [] -> []);
  scn.allow_simple_key <- false;
  advance scn 1;
  push_token scn (make_token Flow_sequence_end start (pos scn))

let fetch_flow_mapping_end scn =
  let start = pos scn in
  if scn.flow_level = 0 then
    Types.scan_error start "unexpected '}' outside of a flow mapping";
  remove_possible_simple_key scn;
  scn.flow_level <- scn.flow_level - 1;
  scn.flow_is_sequence <-
    (match scn.flow_is_sequence with
    | _ :: t -> t
    | [] -> []);
  scn.allow_simple_key <- false;
  advance scn 1;
  push_token scn (make_token Flow_mapping_end start (pos scn))

let fetch_flow_entry scn =
  let start = pos scn in
  remove_possible_simple_key scn;
  scn.allow_simple_key <- true;
  advance scn 1;
  push_token scn (make_token Flow_entry start (pos scn))

(* ------------------------------------------------------------------ *)
(* Block sequence entry                                                 *)
(* ------------------------------------------------------------------ *)

let fetch_block_entry scn =
  let start = pos scn in
  if scn.flow_level = 0 then begin
    (* Check that we can start a new sequence entry here.
       allow_simple_key is false iff a tab was seen earlier on this line in
       block context.  Creating a new block collection after a tab is
       invalid (tabs cannot be used for block structure indentation). *)
    let col = (pos scn).column in
    if (not scn.allow_simple_key) && scn.indent < col then
      Types.scan_error start
        "block sequence entry cannot be introduced after a tab character";
    if add_indent scn col then
      insert_collection_start scn (make_token Block_sequence_start start start)
  end;
  remove_possible_simple_key scn;
  scn.allow_simple_key <- true;
  advance scn 1;
  push_token scn (make_token Block_entry start (pos scn))

(* ------------------------------------------------------------------ *)
(* Key token                                                            *)
(* ------------------------------------------------------------------ *)

let fetch_key scn =
  let start = pos scn in
  if scn.flow_level = 0 then begin
    let col = (pos scn).column in
    if (not scn.allow_simple_key) && scn.indent < col then
      Types.scan_error start
        "block mapping key cannot be introduced after a tab character";
    if add_indent scn col then
      insert_collection_start scn (make_token Block_mapping_start start start)
  end;
  remove_possible_simple_key scn;
  (* In block context, simple keys ARE allowed after '?' (the key content
     may itself be an implicit key).  In flow context they are not. *)
  scn.allow_simple_key <- scn.flow_level = 0;
  advance scn 1;
  push_token scn (make_token Key start (pos scn))

(* ------------------------------------------------------------------ *)
(* Value token (':')                                                    *)
(* ------------------------------------------------------------------ *)

let fetch_value scn =
  let start = pos scn in
  (match Hashtbl.find_opt scn.possible_simple_keys scn.flow_level with
  | Some sk ->
      (* There is a pending simple key: retroactively insert KEY (and possibly
       BLOCK_MAPPING_START) before the scalar that was the key candidate. *)
      (* Implicit keys must not span multiple lines, except in flow mappings
       where multiline keys are permitted.  Block context and flow sequences
       both require the key to fit on a single line. *)
      let in_flow_mapping =
        scn.flow_level > 0
        &&
        match scn.flow_is_sequence with
        | false :: _ -> true
        | _ -> false
      in
      if (not in_flow_mapping) && (pos scn).line > sk.sk_pos.line then
        Types.scan_error sk.sk_pos "implicit key cannot span multiple lines";
      Hashtbl.remove scn.possible_simple_keys scn.flow_level;
      if sk.sk_token_number = scn.min_simple_key_token then
        recompute_min_simple_key_token scn;
      let insert_pos = sk.sk_token_number - scn.tokens_taken in
      insert_token scn insert_pos (make_token Key sk.sk_pos sk.sk_pos);
      if scn.flow_level = 0 then begin
        let col = sk.sk_pos.column in
        if add_indent scn col then
          insert_token scn insert_pos
            (make_token Block_mapping_start sk.sk_pos sk.sk_pos)
      end;
      scn.allow_simple_key <- false
  | None ->
      (* No simple key: this value indicator starts a new key/value pair
       or is a bare value.
       In block context, a VALUE without a preceding implicit or explicit
       key is only valid when allow_simple_key is true.  allow_simple_key
       gets reset to true whenever we cross a newline in block context
       (see scan_to_next_token).  A VALUE that appears directly after a
       closing ] or } on the same line therefore has allow_simple_key=false
       and is rejected here, matching the YAML 1.2 spec. *)
      if scn.flow_level = 0 && not scn.allow_simple_key then
        Types.scan_error (pos scn) "mapping values are not allowed here";
      if scn.flow_level = 0 then begin
        let col = (pos scn).column in
        if add_indent scn col then
          insert_token scn 0 (make_token Block_mapping_start start start)
      end;
      scn.allow_simple_key <- true);
  advance scn 1;
  push_token scn (make_token Value start (pos scn))

(* ------------------------------------------------------------------ *)
(* Anchors and aliases                                                  *)
(* ------------------------------------------------------------------ *)

(** Scan an anchor ([&name]) or alias ([ *name]) and push the token. *)
let fetch_anchor_or_alias scn is_alias =
  let start = pos scn in
  save_possible_simple_key scn;
  scn.allow_simple_key <- false;
  advance scn 1;
  (* consume '&' or '*' *)
  let buf = Buffer.create 16 in
  while Char_class.is_anchor_char (peek scn 0) do
    Reader.encode_utf8_to buf (peek scn 0);
    advance scn 1
  done;
  let name = Buffer.contents buf in
  if name = "" then Types.scan_error start "empty anchor or alias name";
  let kind : token_kind = if is_alias then Alias name else Anchor name in
  push_token scn (make_token kind start (pos scn))

(* ------------------------------------------------------------------ *)
(* Tags                                                                 *)
(* ------------------------------------------------------------------ *)

(** Scan a tag and push the token. Three forms:
    - [!<uri>] verbatim tag → handle='', suffix=uri
    - [!!suffix] shorthand → handle='!!', suffix=…
    - [!handle!suffix] named → handle='!handle!', suffix=…
    - [!suffix] primary → handle='!', suffix=…
    - [!] non-specific → handle='!', suffix='' *)
let fetch_tag scn =
  let start = pos scn in
  save_possible_simple_key scn;
  scn.allow_simple_key <- false;
  advance scn 1;
  (* consume '!' *)
  let handle, suffix =
    if peek scn 0 = 0x3C (* < *) then begin
      (* Verbatim tag: !<URI> *)
      advance scn 1;
      let buf = Buffer.create 32 in
      let continue_ = ref true in
      while !continue_ do
        let cp = peek scn 0 in
        if cp = 0x3E (* > *) then (
          continue_ := false;
          advance scn 1)
        else if cp = Char_class.eof || Char_class.is_linebreak cp then
          Types.scan_error (pos scn) "unexpected end in verbatim tag"
        else begin
          Reader.encode_utf8_to buf cp;
          advance scn 1
        end
      done;
      ("", Buffer.contents buf)
    end
    else begin
      (* Collect the part up to an optional second '!' *)
      let buf = Buffer.create 16 in
      let found_bang = ref false in
      let continue_ = ref true in
      while !continue_ do
        let cp = peek scn 0 in
        if cp = 0x21 (* ! *) then begin
          (* This is a named handle like !handle!  *)
          Buffer.add_char buf '!';
          advance scn 1;
          found_bang := true;
          continue_ := false
        end
        else if Char_class.is_tag_char cp && cp <> Char_class.eof then begin
          Reader.encode_utf8_to buf cp;
          advance scn 1
        end
        else continue_ := false
      done;
      if !found_bang then begin
        (* handle = '!' + buf so far; scan suffix *)
        let handle = "!" ^ Buffer.contents buf in
        let sbuf = Buffer.create 32 in
        let continue_ = ref true in
        while !continue_ do
          let cp = peek scn 0 in
          if Char_class.is_tag_char cp then begin
            Reader.encode_utf8_to sbuf cp;
            advance scn 1
          end
          else continue_ := false
        done;
        (handle, Buffer.contents sbuf)
      end
      else begin
        (* Primary handle '!' or just suffix after '!' *)
        let s = Buffer.contents buf in
        if s = "" then ("!", "") (* bare ! non-specific tag *) else ("!", s)
      end
    end
  in
  push_token scn (make_token (Tag (handle, suffix)) start (pos scn))

(* ------------------------------------------------------------------ *)
(* Block scalars  (| and >)                                             *)
(* ------------------------------------------------------------------ *)

type chomping = Strip | Clip | Keep

(** Parse the chomping indicator (+/-) and optional indentation number that
    immediately follow [|] or [>]. Consume through the end of line. *)
let scan_block_scalar_indicators scn =
  let chomp = ref Clip in
  let explicit_indent = ref (-1) in
  (* Read an optional chomping indicator then an optional indentation digit,
     or vice versa – both orderings are allowed. *)
  let read_indicator () =
    let cp = peek scn 0 in
    if cp = 0x2B (* + *) then (
      chomp := Keep;
      advance scn 1;
      true)
    else if cp = 0x2D (* - *) then (
      chomp := Strip;
      advance scn 1;
      true)
    else if Char_class.is_digit cp && cp <> 0x30 (* 0 *) then begin
      explicit_indent := cp - 0x30;
      advance scn 1;
      true
    end
    else false
  in
  ignore (read_indicator ());
  ignore (read_indicator ());
  (* After the indicators, only optional whitespace + comment + EOL is allowed.
     Any other content (e.g., "| first line" or "|0" or "|# comment") is an
     error: the former two because they carry text in the header, the third
     because YAML requires at least one space before a comment character. *)
  let had_white = ref false in
  while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
    advance scn 1;
    had_white := true
  done;
  let cp = peek scn 0 in
  if cp = 0x23 (* # *) && !had_white then begin
    (* Valid comment preceded by whitespace – capture it as a line comment on
       the block scalar's header line so it can be re-emitted by the printer. *)
    let comment_line = (pos scn).line in
    let comment_col = (pos scn).column in
    advance scn 1;
    (* consume '#' *)
    let buf = Buffer.create 32 in
    while
      (not (Char_class.is_linebreak (peek scn 0)))
      && peek scn 0 <> Char_class.eof
    do
      Reader.encode_utf8_to buf (peek scn 0);
      advance scn 1
    done;
    Queue.add
      (comment_line, comment_col, true, Buffer.contents buf)
      scn.comments
  end
  else if not (Char_class.is_linebreak cp || cp = Char_class.eof) then
    Types.scan_error (pos scn)
      "invalid character in block scalar header (only whitespace and a comment \
       are allowed)";
  (!chomp, !explicit_indent)

(** Scan a literal ([|]) or folded ([>]) block scalar. *)
let scan_block_scalar scn style =
  let start = pos scn in
  advance scn 1;
  (* consume | or > *)
  let chomp, explicit_indent = scan_block_scalar_indicators scn in
  (* Consume the line break after the header *)
  if Char_class.is_linebreak (peek scn 0) then ignore (scan_line_break scn);
  (* Tracks the scanner position right after the last content character on the
     last content line (i.e. right at the trailing newline or EOF of that line).
     Initialized to the position after the header so that an empty block scalar
     has a loc that ends right after the indicator character. *)
  let last_content_pos = ref (pos scn) in
  (* Minimum indentation: at top level (scn.indent = -1) allow content at
     column 0 (min_indent = 0); inside a block, content must exceed its parent. *)
  let min_indent = max 0 (scn.indent + 1) in
  let block_indent =
    ref (if explicit_indent > 0 then min_indent + explicit_indent - 1 else 0)
  in

  (* ---- Leading blank lines (before first non-blank content) ---- *)
  (* Uses lookahead so we can consume exactly block_indent spaces when a     *)
  (* non-blank line is found (instead of consuming all leading spaces).      *)
  let leading_buf = Buffer.create 16 in
  (* first_content_look: number of leading spaces on the first content line.
     Used to initialize next_line_col so that folded scalars correctly treat
     more-indented (or spaced) first lines. *)
  let first_content_look = ref !block_indent in
  let max_blank_indent = ref 0 in
  let cont = ref true in
  while !cont do
    let look = ref 0 in
    while peek scn !look = 0x20 do
      incr look
    done;
    let next_cp = peek scn !look in
    if Char_class.is_linebreak next_cp || next_cp = Char_class.eof then
      (* Blank line *)
      begin if next_cp = Char_class.eof then cont := false
      else begin
        if !look > !max_blank_indent then max_blank_indent := !look;
        for _ = 1 to !look do
          advance scn 1
        done;
        Buffer.add_char leading_buf '\n';
        ignore (scan_line_break scn)
      end
      end
    else begin
      (* Non-blank line: auto-detect block_indent from the first content line *)
      if !block_indent = 0 then block_indent := max min_indent !look;
      (* Blank lines must not be more indented than the first content line *)
      if !max_blank_indent > !block_indent then
        Types.scan_error (pos scn)
          "block scalar contains a blank line more indented than the content";
      first_content_look := !look;
      (* Consume exactly block_indent spaces (if the line has enough) *)
      if !look >= !block_indent then
        for _ = 1 to !block_indent do
          advance scn 1
        done;
      cont := false
    end
  done;

  (* Helper: are we at a document-start or document-end marker? *)
  let at_doc_marker () =
    peek scn 0 = 0x2D
    && peek scn 1 = 0x2D
    && peek scn 2 = 0x2D
    && (Char_class.is_white (peek scn 3) || peek scn 3 = Char_class.eof)
    || peek scn 0 = 0x2E
       && peek scn 1 = 0x2E
       && peek scn 2 = 0x2E
       && (Char_class.is_white (peek scn 3) || peek scn 3 = Char_class.eof)
  in

  (* ---- Content lines ---- *)
  let content_buf = Buffer.create 64 in
  (* trailing_buf accumulates blank-line newlines after the last content line
     (used by Keep chomping to preserve trailing blank lines). *)
  let trailing_buf = Buffer.create 16 in
  let blank_count = ref 0 in
  (* next_line_col: column count of the next content line's leading spaces,
     optionally bumped by 1 when the first content char is whitespace (spaced
     line) so that folded scalars treat it like a more-indented line.
     Initialized from first_content_look so that a more-indented or spaced
     first content line is correctly detected by this_more_indented. *)
  let first_spaced =
    style = Folded
    && !first_content_look >= !block_indent
    && Char_class.is_blank (peek scn 0)
  in
  let next_line_col =
    ref (!first_content_look + if first_spaced then 1 else 0)
  in
  let prev_more_indented = ref false in
  let cont = ref true in

  while !cont do
    let current_col = (pos scn).column in
    if
      current_col < !block_indent
      || peek scn 0 = Char_class.eof
      || (!block_indent = 0 && at_doc_marker ())
    then cont := false
    else begin
      let this_more_indented = !next_line_col > !block_indent in

      (* Emit separator between consecutive content lines.
         NOTE: only when content_buf is already non-empty (not before the first
         content line – leading_buf is prepended at the end instead). *)
      if Buffer.length content_buf > 0 then begin
        (match style with
        | Folded ->
            if !prev_more_indented || this_more_indented then
              (* Lines adjacent to a more-indented/spaced line: preserve newlines *)
              for _ = 1 to !blank_count + 1 do
                Buffer.add_char content_buf '\n'
              done
            else if !blank_count = 0 then
              Buffer.add_char content_buf ' ' (* fold single newline to space *)
            else
              for _ = 1 to !blank_count do
                Buffer.add_char content_buf '\n'
              done
        | Literal
        | Plain
        | Single_quoted
        | Double_quoted ->
            for _ = 1 to !blank_count + 1 do
              Buffer.add_char content_buf '\n'
            done);
        Buffer.clear trailing_buf;
        blank_count := 0
      end;

      prev_more_indented := this_more_indented;

      (* Read the content of this line *)
      while
        (not (Char_class.is_linebreak (peek scn 0)))
        && peek scn 0 <> Char_class.eof
      do
        Reader.encode_utf8_to content_buf (peek scn 0);
        advance scn 1
      done;
      (* pos scn is now right at the line terminator (or EOF) following the
         last character of this content line — record it as the candidate
         end_pos so trailing newlines are excluded from the token's loc. *)
      last_content_pos := pos scn;

      if peek scn 0 = Char_class.eof then cont := false
      else begin
        ignore (scan_line_break scn);
        (* Scan ahead to collect blank lines and locate the next content line *)
        let scanning_ahead = ref true in
        blank_count := 0;
        Buffer.clear trailing_buf;
        next_line_col := 0;
        while !scanning_ahead do
          let look = ref 0 in
          while peek scn !look = 0x20 do
            incr look
          done;
          let next_cp = peek scn !look in
          (* A "blank" line has at most block_indent leading spaces then a
             line-break or EOF.  A line with MORE spaces then a line-break is
             a content line whose value is just spaces beyond block_indent. *)
          if
            (Char_class.is_linebreak next_cp || next_cp = Char_class.eof)
            && !look <= !block_indent
          then
            begin if next_cp = Char_class.eof then
              (* EOF: the scalar just ends; no trailing blank line to record *)
              scanning_ahead := false
            else begin
              (* Blank line: record for Keep mode *)
              Buffer.add_char trailing_buf '\n';
              for _ = 1 to !look do
                advance scn 1
              done;
              ignore (scan_line_break scn);
              incr blank_count
            end
            end
          else begin
            (* Non-blank content line *)
            (* For folded scalars, treat a line whose first char (after
               block_indent) is whitespace as "more-indented" so that the
               surrounding line breaks are preserved rather than folded. *)
            let spaced =
              style = Folded && !look >= !block_indent
              && Char_class.is_blank (peek scn !look)
            in
            next_line_col := !look + if spaced then 1 else 0;
            if !look < !block_indent || (!block_indent = 0 && at_doc_marker ())
            then
              scanning_ahead := false (* dedented or doc marker: scalar ends *)
            else begin
              for _ = 1 to !block_indent do
                advance scn 1
              done;
              scanning_ahead := false
            end
          end
        done
      end
    end
  done;

  let leading = Buffer.contents leading_buf in
  let content = Buffer.contents content_buf in
  let trailing = Buffer.contents trailing_buf in
  let value =
    match chomp with
    | Strip ->
        (* No trailing newline; leading blank lines only kept with content *)
        if content = "" then "" else leading ^ content
    | Clip ->
        (* Exactly one trailing newline; leading blank lines only kept with content *)
        if content = "" then "" else (leading ^ content) ^ "\n"
    | Keep ->
        (* All trailing blank lines preserved; leading included unconditionally *)
        if content = "" then leading else leading ^ content ^ "\n" ^ trailing
  in
  push_token scn (make_token (Scalar (value, style)) start !last_content_pos)

(* ------------------------------------------------------------------ *)
(* Flow scalars: single-quoted and double-quoted                        *)
(* ------------------------------------------------------------------ *)

(** Scan a single-quoted scalar. *)
let scan_single_quoted_scalar scn =
  let _start = pos scn in
  advance scn 1;
  (* consume opening ' *)
  let buf = Buffer.create 32 in
  let done_ = ref false in
  while not !done_ do
    let cp = peek scn 0 in
    if cp = 0x27 (* ' *) then
      begin if peek scn 1 = 0x27 (* '' → literal ' *) then begin
        Buffer.add_char buf '\'';
        advance scn 2
      end
      else begin
        advance scn 1;
        (* closing quote *)
        done_ := true
      end
      end
    else if cp = Char_class.eof then
      Types.scan_error (pos scn)
        "unexpected end of input in single-quoted scalar"
    else if Char_class.is_linebreak cp then begin
      (* Fold line break: newline + leading spaces → single space,
         or multiple blank lines → (n-1) newlines.
         Per YAML spec, trailing white space before the line break is stripped. *)
      let s = Buffer.contents buf in
      let len = String.length s in
      let trimmed_len = ref len in
      while
        !trimmed_len > 0
        &&
        let c = s.[!trimmed_len - 1] in
        c = ' ' || c = '\t'
      do
        decr trimmed_len
      done;
      if !trimmed_len < len then begin
        Buffer.clear buf;
        Buffer.add_string buf (String.sub s 0 !trimmed_len)
      end;
      ignore (scan_line_break scn);
      scn.allow_simple_key <- false;
      (* Consume leading spaces on next line *)
      while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
        advance scn 1
      done;
      (* Document markers (--- or ...) at column 0 always end a document;
         their presence inside a quoted scalar is an error. *)
      let is_doc_marker () =
        let cp0 = peek scn 0 in
        (cp0 = 0x2D (* - *) || cp0 = 0x2E (* . *))
        && peek scn 1 = cp0
        && peek scn 2 = cp0
        && (pos scn).column = 0
        &&
        let n = peek scn 3 in
        Char_class.is_white n || n = Char_class.eof
      in
      if is_doc_marker () then
        Types.scan_error (pos scn)
          "document marker found inside single-quoted scalar";
      (* In block context, non-blank continuation lines must be more indented
         than the current block level (blank lines are exempt). *)
      if
        scn.flow_level = 0
        && (not (Char_class.is_linebreak (peek scn 0)))
        && peek scn 0 <> Char_class.eof
        && (pos scn).column <= scn.indent
      then
        Types.scan_error (pos scn)
          "continuation line of single-quoted scalar is not sufficiently \
           indented";
      (* Consume additional blank lines *)
      let blank_count = ref 0 in
      while Char_class.is_linebreak (peek scn 0) do
        ignore (scan_line_break scn);
        incr blank_count;
        while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
          advance scn 1
        done
      done;
      if !blank_count = 0 then Buffer.add_char buf ' '
      else begin
        for _ = 1 to !blank_count do
          Buffer.add_char buf '\n'
        done
      end
    end
    else begin
      Reader.encode_utf8_to buf cp;
      advance scn 1
    end
  done;
  buf

(** Process double-quoted escape sequences. [advance scn] has already consumed
    the backslash. *)
let scan_dq_escape scn =
  let cp = peek scn 0 in
  advance scn 1;
  match cp with
  | 0x30 -> "\x00" (* \0 *)
  | 0x61 -> "\x07" (* \a *)
  | 0x62 -> "\x08" (* \b *)
  | 0x74
  | 0x09 ->
      "\t" (* \t or literal tab *)
  | 0x6E -> "\n" (* \n *)
  | 0x76 -> "\x0B" (* \v *)
  | 0x66 -> "\x0C" (* \f *)
  | 0x72 -> "\r" (* \r *)
  | 0x65 -> "\x1B" (* \e *)
  | 0x20 -> " " (* space *)
  | 0x22 -> "\"" (* backslash-double-quote *)
  | 0x2F -> "/" (* \/ *)
  | 0x5C -> "\\" (* \\ *)
  | 0x4E ->
      (* \N = NEL U+0085 *)
      let b = Buffer.create 2 in
      Reader.encode_utf8_to b 0x85;
      Buffer.contents b
  | 0x5F ->
      (* \_ = non-breaking space U+00A0 *)
      let b = Buffer.create 2 in
      Reader.encode_utf8_to b 0xA0;
      Buffer.contents b
  | 0x4C ->
      (* \L = LS U+2028 *)
      let b = Buffer.create 3 in
      Reader.encode_utf8_to b 0x2028;
      Buffer.contents b
  | 0x50 ->
      (* \P = PS U+2029 *)
      let b = Buffer.create 3 in
      Reader.encode_utf8_to b 0x2029;
      Buffer.contents b
  | 0x78 ->
      (* \xXX *)
      let read_hex scn =
        let c = peek scn 0 in
        if not (Char_class.is_hex_digit c) then
          Types.scan_error (pos scn)
            "expected hex digit in escape sequence, got %s" (Char_class.show c);
        advance scn 1;
        Char_class.hex_value c
      in
      let hi = read_hex scn in
      let lo = read_hex scn in
      let b = Buffer.create 2 in
      Reader.encode_utf8_to b ((hi lsl 4) lor lo);
      Buffer.contents b
  | 0x75 ->
      (* \uXXXX *)
      let cp = ref 0 in
      for _ = 0 to 3 do
        let c = peek scn 0 in
        if not (Char_class.is_hex_digit c) then
          Types.scan_error (pos scn)
            "expected hex digit in \\uXXXX escape, got %s" (Char_class.show c);
        cp := (!cp lsl 4) lor Char_class.hex_value c;
        advance scn 1
      done;
      let b = Buffer.create 4 in
      Reader.encode_utf8_to b !cp;
      Buffer.contents b
  | 0x55 ->
      (* \UXXXXXXXX *)
      let cp = ref 0 in
      for _ = 0 to 7 do
        let c = peek scn 0 in
        if not (Char_class.is_hex_digit c) then
          Types.scan_error (pos scn)
            "expected hex digit in \\UXXXXXXXX escape, got %s"
            (Char_class.show c);
        cp := (!cp lsl 4) lor Char_class.hex_value c;
        advance scn 1
      done;
      if !cp > 0x10FFFF then
        Types.scan_error (pos scn)
          "Unicode escape \\U%08X is out of range (max U+10FFFF)" !cp;
      let b = Buffer.create 4 in
      Reader.encode_utf8_to b !cp;
      Buffer.contents b
  | _ ->
      Types.scan_error (pos scn) "unknown escape sequence '\\%s'"
        (Char_class.show cp)

(** Scan a double-quoted scalar. *)
let scan_double_quoted_scalar scn =
  let start = pos scn in
  advance scn 1;
  (* consume opening double-quote *)
  let buf = Buffer.create 32 in
  (* Source-whitespace accumulator.  Tabs and spaces that come directly from
     the source (not from escape sequences) are held here until the next
     non-whitespace character or a line break.  On a line break they are
     discarded (YAML trailing-whitespace stripping); otherwise they are
     flushed into [buf] first. *)
  let pending_ws = Buffer.create 8 in
  let flush_pending () =
    Buffer.add_buffer buf pending_ws;
    Buffer.clear pending_ws
  in
  let done_ = ref false in
  while not !done_ do
    let cp = peek scn 0 in
    if cp = 0x22 (* 0x22 = double-quote *) then begin
      flush_pending ();
      (* trailing spaces before '"' are content *)
      advance scn 1;
      done_ := true
    end
    else if cp = Char_class.eof then
      Types.scan_error (pos scn)
        "unexpected end of input in double-quoted scalar"
    else if cp = 0x5C (* \ *) then begin
      flush_pending ();
      (* whitespace before escape is content *)
      advance scn 1;
      let next = peek scn 0 in
      if Char_class.is_linebreak next then begin
        (* Line continuation: backslash + newline + leading spaces are ignored *)
        ignore (scan_line_break scn);
        scn.allow_simple_key <- false;
        while peek scn 0 = 0x20 || peek scn 0 = 0x09 do
          advance scn 1
        done
      end
      else begin
        let esc = scan_dq_escape scn in
        Buffer.add_string buf esc
      end
    end
    else if Char_class.is_linebreak cp then begin
      (* Implicit line folding (no backslash).
         Trailing SOURCE white space before a line break is stripped — that is
         exactly what is in [pending_ws], so we discard it. *)
      Buffer.clear pending_ws;
      ignore (scan_line_break scn);
      scn.allow_simple_key <- false;
      (* Collect additional blank lines; also consume leading spaces of next
         content line (they are indentation, not content). *)
      let blank_count = ref 0 in
      while
        peek scn 0 = 0x20
        || peek scn 0 = 0x09
        || Char_class.is_linebreak (peek scn 0)
      do
        if Char_class.is_linebreak (peek scn 0) then begin
          ignore (scan_line_break scn);
          incr blank_count
        end
        else begin
          (* In block context (inside a block collection, indent >= 0), a tab
             at column 0 means the tab is being used as indentation, which is
             forbidden in YAML. *)
          if
            scn.flow_level = 0 && scn.indent >= 0
            && peek scn 0 = 0x09
            && (pos scn).column = 0
          then
            Types.scan_error (pos scn)
              "tab character used as indentation in double-quoted scalar \
               continuation";
          advance scn 1
        end
      done;
      (* Document markers (--- or ...) at column 0 always end a document;
         their presence inside a quoted scalar is an error. *)
      let is_doc_marker () =
        let cp0 = peek scn 0 in
        (cp0 = 0x2D (* - *) || cp0 = 0x2E (* . *))
        && peek scn 1 = cp0
        && peek scn 2 = cp0
        && (pos scn).column = 0
        &&
        let n = peek scn 3 in
        Char_class.is_white n || n = Char_class.eof
      in
      if is_doc_marker () then
        Types.scan_error (pos scn)
          "document marker found inside double-quoted scalar";
      (* In block context, continuation lines must be more indented than
         the current block level. *)
      if scn.flow_level = 0 && (pos scn).column <= scn.indent then
        Types.scan_error (pos scn)
          "continuation line of double-quoted scalar is not sufficiently \
           indented";
      if !blank_count = 0 then Buffer.add_char buf ' '
      else begin
        for _ = 1 to !blank_count do
          Buffer.add_char buf '\n'
        done
      end
    end
    else if cp = 0x20 (* space *) || cp = 0x09 (* tab *) then begin
      (* Source whitespace: accumulate; flush only when more content follows *)
      Buffer.add_char pending_ws (Char.chr cp);
      advance scn 1
    end
    else begin
      flush_pending ();
      Reader.encode_utf8_to buf cp;
      advance scn 1
    end
  done;
  ignore start;
  buf

(** Fetch a single-quoted scalar token. *)
let fetch_single_quoted scn =
  let start = pos scn in
  save_possible_simple_key scn;
  scn.allow_simple_key <- false;
  let buf = scan_single_quoted_scalar scn in
  push_token scn
    (make_token (Scalar (Buffer.contents buf, Single_quoted)) start (pos scn))

(** Fetch a double-quoted scalar token. *)
let fetch_double_quoted scn =
  let start = pos scn in
  save_possible_simple_key scn;
  scn.allow_simple_key <- false;
  let buf = scan_double_quoted_scalar scn in
  push_token scn
    (make_token (Scalar (Buffer.contents buf, Double_quoted)) start (pos scn))

(* ------------------------------------------------------------------ *)
(* Plain scalars                                                         *)
(* ------------------------------------------------------------------ *)

(** Scan a plain (unquoted, non-block) scalar. Plain scalars are the trickiest
    because their termination depends on context (block vs. flow) and on whether
    the next character starts a structure token ([:], [,], etc.). *)
let scan_plain_scalar scn =
  let buf = Buffer.create 32 in
  let spaces = Buffer.create 8 in
  let indent = scn.indent + 1 in
  (* minimum column to continue *)
  let in_flow = scn.flow_level > 0 in
  let stop = ref false in
  (* Track the scanner position right after the last content character, before
     any trailing whitespace or line terminators are consumed.  This is used as
     the token's end_pos so that trailing whitespace is excluded from the loc. *)
  let last_content_pos = ref (pos scn) in
  while not !stop do
    (* Check for termination at current position *)
    let cp0 = peek scn 0 in
    let cp1 = peek scn 1 in
    let terminated =
      cp0 = Char_class.eof
      || Char_class.is_linebreak cp0
      || (cp0 = 0x3A (* : *) && Char_class.is_white cp1) (* ': ' or ':\n' *)
      || (cp0 = 0x3A && cp1 = Char_class.eof)
      (* '#' starts a comment only when preceded by whitespace.  At the outer
         loop level this is true iff we have already accumulated content (buf
         or spaces non-empty), i.e., we are in a continuation and spaces were
         consumed before the '#'. *)
      || cp0 = 0x23
         (* # *) && (Buffer.length buf > 0 || Buffer.length spaces > 0)
      (* In flow context, also stop at flow indicators and commas *)
      || (in_flow && (cp0 = 0x2C (* , *) || Char_class.is_flow_indicator cp0))
      (* In flow, ':' is an indicator only when followed by an unsafe char
         (whitespace, eof, comma, or flow indicator).  When followed by a safe
         char, ':' is allowed even mid-scalar (e.g. "http://"). *)
      || in_flow && cp0 = 0x3A
         && (Char_class.is_white cp1 || cp1 = Char_class.eof || cp1 = 0x2C
            || Char_class.is_flow_indicator cp1)
    in
    if terminated then stop := true
    else begin
      (* Read as many plain characters as possible *)
      let line_buf = Buffer.create 32 in
      let reading = ref true in
      let prev_cp = ref 0x20 in
      (* track previous char for comment detection *)
      while !reading do
        let cp = peek scn 0 in
        let next = peek scn 1 in
        let end_char =
          cp = Char_class.eof || Char_class.is_white cp
          || (cp = 0x3A && (Char_class.is_white next || next = Char_class.eof))
          (* '#' is a comment only when preceded by whitespace *)
          || (cp = 0x23 (* # *) && Char_class.is_blank !prev_cp)
          || (in_flow && (cp = 0x2C || Char_class.is_flow_indicator cp))
          (* In flow, ':' is an indicator only when followed by an unsafe char *)
          || in_flow && cp = 0x3A
             && (Char_class.is_white next || next = Char_class.eof
               || next = 0x2C
                || Char_class.is_flow_indicator next)
        in
        if end_char then reading := false
        else begin
          prev_cp := cp;
          Reader.encode_utf8_to line_buf cp;
          advance scn 1
        end
      done;
      let line = Buffer.contents line_buf in
      if line = "" then stop := true
      else begin
        (* pos scn is now right after the last content character on this line
           (at the first trailing space, comment marker, or line terminator).
           Record it so the token's end_pos excludes trailing whitespace. *)
        last_content_pos := pos scn;
        scn.allow_simple_key <- false;
        Buffer.add_buffer buf spaces;
        Buffer.clear spaces;
        Buffer.add_string buf line;
        (* Collect whitespace / line breaks that follow.
           If trailing blanks are present, consume them first; if a line break
           follows the blanks treat it the same as a direct line break. *)
        let cp = peek scn 0 in
        if Char_class.is_blank cp then begin
          (* Peek ahead to determine what follows the blanks before consuming. *)
          let ahead = ref 0 in
          while Char_class.is_blank (peek scn !ahead) do
            incr ahead
          done;
          let following = peek scn !ahead in
          if Char_class.is_linebreak following || following = Char_class.eof
          then
            (* Blanks before linebreak/EOF: consume them (stripped during folding) *)
            for _ = 1 to !ahead do
              advance scn 1
            done
          else if following <> 0x23 (* # *) then begin
            (* Blanks between words on the same line: consume + preserve one space *)
            for _ = 1 to !ahead do
              advance scn 1
            done;
            Buffer.add_char spaces ' '
          end
          (* Blanks immediately before '#': leave unconsumed so that
             scan_to_next_token sees them and correctly identifies '#' as a
             comment (requires preceding whitespace per the YAML spec). *)
        end;
        (* Handle line break (either directly after content or after blanks) *)
        if Char_class.is_linebreak (peek scn 0) then begin
          ignore (scan_line_break scn);
          scn.allow_simple_key <- true;
          (* Skip blank lines and whitespace-only lines.
             A line containing only spaces/tabs (but no content) is treated as
             blank in plain scalar folding (YAML 1.2 §7.4). *)
          let blank_lines = ref 0 in
          let continue_blanks = ref true in
          while !continue_blanks do
            if Char_class.is_linebreak (peek scn 0) then begin
              ignore (scan_line_break scn);
              incr blank_lines
            end
            else begin
              (* Peek ahead: if all chars before the next line break are blanks
                 (spaces or tabs) this is a whitespace-only line → treat as blank. *)
              let ahead = ref 0 in
              while Char_class.is_blank (peek scn !ahead) do
                incr ahead
              done;
              if !ahead > 0 && Char_class.is_linebreak (peek scn !ahead) then begin
                for _ = 1 to !ahead do
                  advance scn 1
                done;
                ignore (scan_line_break scn);
                incr blank_lines
              end
              else continue_blanks := false
            end
          done;
          (* Check for document markers that terminate the plain scalar *)
          if
            peek scn 0 = 0x2D
            && peek scn 1 = 0x2D
            && peek scn 2 = 0x2D
            && (Char_class.is_white (peek scn 3) || peek scn 3 = Char_class.eof)
            || peek scn 0 = 0x2E
               && peek scn 1 = 0x2E
               && peek scn 2 = 0x2E
               && (Char_class.is_white (peek scn 3)
                  || peek scn 3 = Char_class.eof)
          then stop := true
          else begin
            (* In block context, count leading spaces on next line to get
               effective indent, since (pos scn).column is 0 before any spaces
               are consumed. *)
            let dedented =
              (not in_flow)
              &&
              let ahead = ref 0 in
              while peek scn !ahead = 0x20 do
                incr ahead
              done;
              !ahead < indent
            in
            if dedented then stop := true
            else begin
              (* Consume leading indentation of the continuation line so the
                 inner loop starts at actual content, not at indentation spaces.
                 Tabs after the spaces are also consumed (legal content separators). *)
              while Char_class.is_blank (peek scn 0) do
                advance scn 1
              done;
              if !blank_lines = 0 then
                Buffer.add_char spaces ' ' (* fold one newline to space *)
              else
                for _ = 1 to !blank_lines do
                  Buffer.add_char spaces '\n'
                done
            end
          end
        end
        else if not (Char_class.is_blank cp) then
          (* If we didn't start with a blank and didn't hit a line break, stop.
             (If we consumed blanks above, the outer loop will re-check.) *)
          stop := true
      end
    end
  done;
  (Buffer.contents buf, !last_content_pos)

(** Fetch a plain scalar token. *)
let fetch_plain scn =
  let start = pos scn in
  save_possible_simple_key scn;
  scn.allow_simple_key <- false;
  let value, end_pos = scan_plain_scalar scn in
  push_token scn (make_token (Scalar (value, Plain)) start end_pos)

(* ------------------------------------------------------------------ *)
(* Stream start / end                                                    *)
(* ------------------------------------------------------------------ *)

let fetch_stream_start scn =
  scn.indent <- -1;
  scn.indents <- [];
  push_token scn (make_token Stream_start (pos scn) (pos scn));
  scn.stream_start_produced <- true

let fetch_stream_end scn =
  (* Unclosed flow collections are a parse error *)
  if scn.flow_level > 0 then
    Types.scan_error (pos scn) "unexpected end of stream in flow collection";
  (* Close any open block collections *)
  unwind_indent scn (-1);
  scn.allow_simple_key <- false;
  Hashtbl.clear scn.possible_simple_keys;
  scn.min_simple_key_token <- max_int;
  push_token scn (make_token Stream_end (pos scn) (pos scn));
  scn.done_ <- true

(* ------------------------------------------------------------------ *)
(* Main dispatch                                                         *)
(* ------------------------------------------------------------------ *)

(** True if the three characters at the current position form [---] or [...] and
    are immediately followed by whitespace or EOF. *)
let check_document_marker scn ch =
  peek scn 0 = ch
  && peek scn 1 = ch
  && peek scn 2 = ch
  && (pos scn).column = 0
  &&
  let n = peek scn 3 in
  Char_class.is_white n || n = Char_class.eof

(** True if the current character starts a value indicator ([:] followed by
    whitespace, EOF, or a flow indicator, or a pending simple key in flow). *)
let check_value scn =
  let next = peek scn 1 in
  Char_class.is_white next || next = Char_class.eof
  || Char_class.is_flow_indicator next
  || (scn.flow_level > 0 && next = 0x2C (* , *))
  || (scn.flow_level > 0 && Hashtbl.mem scn.possible_simple_keys scn.flow_level)

(** True if the current [-] is a block sequence entry (not a plain scalar that
    starts with [-]). *)
let check_block_entry scn =
  let next = peek scn 1 in
  Char_class.is_white next || next = Char_class.eof

(** True if the current [?] is an explicit mapping key marker. *)
let check_explicit_key scn =
  let next = peek scn 1 in
  Char_class.is_white next || next = Char_class.eof

(** Scan enough tokens to satisfy the next [get_token] call. *)
let rec fetch_more_tokens scn =
  scan_to_next_token scn;
  stale_possible_simple_keys scn;
  unwind_indent scn (column scn);
  let cp = peek scn 0 in
  (* Inside a flow collection, each continuation line must be more indented
     than the surrounding block context (col > scn.indent).  When scn.indent
     is -1 (document root) there is no surrounding block, so any column is
     fine.  EOF is also exempt. *)
  if
    scn.flow_level > 0 && scn.indent >= 0
    && (pos scn).column <= scn.indent
    && cp <> Char_class.eof
  then
    Types.scan_error (pos scn)
      "flow collection content must be more indented than the surrounding \
       block context";
  if not scn.stream_start_produced then fetch_stream_start scn
  else if cp = Char_class.eof then fetch_stream_end scn
  else if cp = 0x25 (* % *) && (pos scn).column = 0 then fetch_directive scn
  else if check_document_marker scn 0x2D (* - *) then fetch_document_start scn
  else if check_document_marker scn 0x2E (* . *) then fetch_document_end scn
  else if cp = 0x5B (* [ *) then fetch_flow_sequence_start scn
  else if cp = 0x7B (* { *) then fetch_flow_mapping_start scn
  else if cp = 0x5D (* ] *) then fetch_flow_sequence_end scn
  else if cp = 0x7D (* } *) then fetch_flow_mapping_end scn
  else if cp = 0x2C (* , *) then fetch_flow_entry scn
  else if cp = 0x2D (* - *) && check_block_entry scn then fetch_block_entry scn
  else if cp = 0x3F (* ? *) && check_explicit_key scn then fetch_key scn
  else if cp = 0x3A (* : *) && check_value scn then fetch_value scn
  else if cp = 0x2A (* * *) then fetch_anchor_or_alias scn true
  else if cp = 0x26 (* & *) then fetch_anchor_or_alias scn false
  else if cp = 0x21 (* ! *) then fetch_tag scn
  else if cp = 0x7C (* | *) && scn.flow_level = 0 then
    scan_block_scalar scn Literal
  else if cp = 0x3E (* > *) && scn.flow_level = 0 then
    scan_block_scalar scn Folded
  else if cp = 0x27 (* ' *) then fetch_single_quoted scn
  else if cp = 0x22 (* ' *) then fetch_double_quoted scn
  else if cp = 0x09 (* TAB *) && scn.flow_level > 0 then begin
    (* A tab at the start of a line (column 0) inside a flow collection.
       Blank lines and lines starting with flow-structure characters (], }, ,)
       are OK; a tab followed by YAML content is a parse error since tabs may
       not serve as indentation. *)
    while peek scn 0 = 0x09 do
      advance scn 1
    done;
    let next_cp = peek scn 0 in
    if
      Char_class.is_linebreak next_cp
      || next_cp = Char_class.eof || next_cp = 0x5D (* ] *) || next_cp = 0x7D
      (* } *) || next_cp = 0x2C
    (* , *)
    then fetch_more_tokens scn (* OK: blank line or flow-structure token *)
    else
      Types.scan_error (pos scn)
        "tab character cannot be used to indent content inside a flow \
         collection"
  end
  else if cp = 0x09 (* TAB *) && scn.flow_level = 0 then begin
    (* A tab at the start of a line in block context: only legal before a
       flow-collection indicator ([ or {).  Consume the tab(s) and re-dispatch
       when the next character is a flow indicator; otherwise it is an illegal
       use of tab as block indentation. *)
    while peek scn 0 = 0x09 do
      advance scn 1
    done;
    let next_cp = peek scn 0 in
    if next_cp = 0x5B (* [ *) || next_cp = 0x7B (* { *) then
      fetch_more_tokens scn (* re-dispatch without the leading tab(s) *)
    else
      Types.scan_error (pos scn)
        "tab character cannot be used as block indentation"
  end
  else if can_start_plain scn cp then fetch_plain scn
  else Types.scan_error (pos scn) "unexpected character %s" (Char_class.show cp)

and can_start_plain scn cp =
  let in_flow = scn.flow_level > 0 in
  let next = peek scn 1 in
  (* Per YAML spec, ':', '-', '?' can start a plain scalar when the following
     character is not a safe separator. *)
  if cp = 0x3A (* : *) then
    not
      (Char_class.is_white next || next = Char_class.eof
      || (in_flow && (next = 0x2C || Char_class.is_flow_indicator next)))
  else if cp = 0x2D (* - *) || cp = 0x3F (* ? *) then
    if in_flow then
      not
        (Char_class.is_white next || next = Char_class.eof || next = 0x2C
        || Char_class.is_flow_indicator next)
    else not (Char_class.is_white next || next = Char_class.eof)
  else if in_flow then Char_class.can_start_plain_flow cp
  else Char_class.can_start_plain_block cp

(* ------------------------------------------------------------------ *)
(* Public interface                                                      *)
(* ------------------------------------------------------------------ *)

let create (reader : Reader.t) : state =
  {
    reader;
    tokens = Queue.create ();
    tokens_taken = 0;
    indent = -1;
    indents = [];
    flow_level = 0;
    flow_is_sequence = [];
    allow_simple_key = true;
    possible_simple_keys = Hashtbl.create 4;
    min_simple_key_token = max_int;
    done_ = false;
    stream_start_produced = false;
    comments = Queue.create ();
  }

(** Return all accumulated comments in source order as
    [(line, col, is_line_comment, text)] tuples. Typically called after the full
    token stream has been consumed. *)
let drain_comments (scn : state) : (int * int * bool * string) list =
  Queue.fold (fun acc x -> x :: acc) [] scn.comments |> List.rev

(** Ensure at least one token is available, fetching more if necessary. *)
let ensure_token (scn : state) : unit =
  while need_more_tokens scn do
    fetch_more_tokens scn
  done

(** Return (without consuming) the next token in the stream. *)
let peek_token (scn : state) : token =
  ensure_token scn;
  if Queue.is_empty scn.tokens then
    failwith "Scanner.peek_token: internal error: no token available"
  else Queue.peek scn.tokens

(** Consume and return the next token. *)
let get_token (scn : state) : token =
  ensure_token scn;
  if Queue.is_empty scn.tokens then
    failwith "Scanner.get_token: internal error: no token available"
  else begin
    scn.tokens_taken <- scn.tokens_taken + 1;
    Queue.pop scn.tokens
  end

(** True if the next token's kind is in [kinds]. *)
let check_token (scn : state) (kinds : token_kind list) : bool =
  ensure_token scn;
  if Queue.is_empty scn.tokens then false
  else List.mem (Queue.peek scn.tokens).tok_kind kinds

(** Peek at the kind of the next token without consuming it. Returns
    [Stream_end] when no token is available. *)
let peek_kind (scn : state) : token_kind =
  ensure_token scn;
  if Queue.is_empty scn.tokens then Stream_end
  else (Queue.peek scn.tokens).tok_kind
