(** Comment attachment pass.

    After the Composer has built a node tree (with all comment fields empty),
    this module takes the raw comment list collected by the Scanner and attaches
    each comment to the most appropriate node. *)

(*
    Best-effort semantics
    ~~~~~~~~~~~~~~~~~~~~~
    Comment attachment is a heuristic process and cannot be perfect in all
    cases.  The rules applied are:

    - Head comments: standalone comment lines (lines that contain nothing but a
      comment) that appear immediately before a node are attached as
      [head_comments] of that node.

    - Line comments: a comment on the same line as a node's start position is
      attached as [line_comment].  For mapping pairs where key and value start
      on the same line, the line comment is attached to the value (the last node
      on that line), not the key.

    - Foot comments: standalone comment lines that appear after the last child
      of a collection, before the next sibling, are attached as [foot_comments]
      of the collection.

    Comments inside flow collections are not recorded by the Scanner.  Comments
    that cannot be attributed to any node are silently discarded.  The behavior
    is subject to change as the heuristics are refined.
*)

open Types

(* ------------------------------------------------------------------ *)
(* Comment cursor                                                        *)
(* ------------------------------------------------------------------ *)

type cursor = (int * int * bool * string) list ref
(** A mutable cursor through the sorted comment list. Each entry is
    [(line, col, is_line_comment, text)]. *)

let make_cursor comments =
  ref (List.sort (fun (a, _, _, _) (b, _, _, _) -> compare a b) comments)

(** Take elements from the front of [xs] as long as they satisfy [p]. Returns
    the matching prefix (in order) and the remaining suffix. *)
let rec take_while p acc xs =
  match xs with
  | [] -> (List.rev acc, [])
  | x :: tail -> if p x then take_while p (x :: acc) tail else (List.rev acc, xs)

(** Consume and return the texts of non-line-comment entries at lines strictly
    before [line]. When [~min_col] is given (default 0), only entries whose
    column is >= [min_col] are taken; entries with a smaller column are left in
    the cursor for an outer scope to claim as head comments. *)
let take_head_before ?(min_col = 0) line cur =
  let taken, rest =
    take_while
      (fun (l, col, is_line, _) -> (not is_line) && l < line && col >= min_col)
      [] !cur
  in
  cur := rest;
  List_ext.map (fun (_, _, _, t) -> t) taken

(** Consume and return the text of a line-comment entry exactly at [line], if
    present. *)
let take_line_comment line (cur : cursor) =
  match !cur with
  | (l, _, true, t) :: rest when l = line ->
      cur := rest;
      Some t
  | _ -> None

(* ------------------------------------------------------------------ *)
(* Node helpers                                                          *)
(* ------------------------------------------------------------------ *)

let node_line = function
  | Scalar_node r -> r.loc.start_pos.line
  | Sequence_node r -> r.loc.start_pos.line
  | Mapping_node r -> r.loc.start_pos.line
  | Alias_node r -> r.loc.start_pos.line

(** Return the [head_comments] field of a node. *)
let get_heads = function
  | Scalar_node r -> r.head_comments
  | Sequence_node r -> r.head_comments
  | Mapping_node r -> r.head_comments
  | Alias_node r -> r.head_comments

(** Return the [line_comment] field of a node. *)
let get_lc = function
  | Scalar_node r -> r.line_comment
  | Sequence_node r -> r.line_comment
  | Mapping_node r -> r.line_comment
  | Alias_node r -> r.line_comment

(** Set [head_comments] on a node. *)
let set_heads heads = function
  | Scalar_node r -> Scalar_node { r with head_comments = heads }
  | Sequence_node r -> Sequence_node { r with head_comments = heads }
  | Mapping_node r -> Mapping_node { r with head_comments = heads }
  | Alias_node r -> Alias_node { r with head_comments = heads }

(** Set [line_comment] on a node. *)
let set_lc lc = function
  | Scalar_node r -> Scalar_node { r with line_comment = lc }
  | Sequence_node r -> Sequence_node { r with line_comment = lc }
  | Mapping_node r -> Mapping_node { r with line_comment = lc }
  | Alias_node r -> Alias_node { r with line_comment = lc }

(** Set [foot_comments] on a node (scalars, aliases, and collections). *)
let set_feet feet = function
  | Scalar_node r -> Scalar_node { r with foot_comments = feet }
  | Sequence_node r -> Sequence_node { r with foot_comments = feet }
  | Mapping_node r -> Mapping_node { r with foot_comments = feet }
  | Alias_node r -> Alias_node { r with foot_comments = feet }

(* ------------------------------------------------------------------ *)
(* Attachment traversal                                                  *)
(* ------------------------------------------------------------------ *)

(** Attach comments to a single node. [next_line] is the source line where the
    next sibling begins (or [max_int] for the last sibling), used to bound
    foot-comment collection. *)
let rec attach_node cur ~next_line node =
  let nl = node_line node in

  (* Head comments: standalone lines before this node *)
  let heads = take_head_before nl cur in
  let node = set_heads (get_heads node @ heads) node in

  (* Descend into children and then collect foot comments *)
  match node with
  | Scalar_node _
  | Alias_node _ ->
      let lc = take_line_comment nl cur in
      let node = set_lc lc node in
      let feet = take_head_before next_line ~min_col:0 cur in
      set_feet feet node
  | Sequence_node r ->
      (* For a flow sequence or an empty sequence, a same-line comment belongs to
       the sequence node itself.  For a non-empty block sequence the comment on
       its start line belongs to the first item, so we skip take_line_comment
       here and let attach_siblings pick it up. *)
      let lc =
        if r.flow || r.items = [] then take_line_comment nl cur else None
      in
      let items = attach_siblings cur r.items ~parent_next_line:next_line in
      let feet =
        take_head_before next_line ~min_col:r.loc.start_pos.column cur
      in
      Sequence_node
        {
          r with
          head_comments = get_heads node;
          line_comment = lc;
          items;
          foot_comments = feet;
        }
  | Mapping_node r ->
      (* Same reasoning as Sequence_node above. *)
      let lc =
        if r.flow || r.pairs = [] then take_line_comment nl cur else None
      in
      let pairs = attach_pairs cur r.pairs ~parent_next_line:next_line in
      let feet =
        take_head_before next_line ~min_col:r.loc.start_pos.column cur
      in
      Mapping_node
        {
          r with
          head_comments = get_heads node;
          line_comment = lc;
          pairs;
          foot_comments = feet;
        }

(** Attach comments to a list of sibling nodes. [parent_next_line] bounds the
    foot-comment zone of the last sibling. *)
and attach_siblings cur nodes ~parent_next_line:_ =
  let arr = Array.of_list nodes in
  let n = Array.length arr in
  for i = 0 to n - 1 do
    (* For the last sibling, use a tight bound (one line past the item itself)
       rather than parent_next_line.  This prevents the last scalar/alias from
       consuming trailing comments that belong to the parent collection as foot
       comments — those are picked up by the parent's own foot-collection step. *)
    let next =
      if i + 1 < n then node_line arr.(i + 1) else node_line arr.(i) + 1
    in
    arr.(i) <- attach_node cur ~next_line:next arr.(i)
  done;
  Array.to_list arr

(** Attach comments to a list of mapping pairs. [parent_next_line] bounds the
    foot-comment zone of the last pair. *)
and attach_pairs cur pairs ~parent_next_line =
  let arr = Array.of_list pairs in
  let n = Array.length arr in
  for i = 0 to n - 1 do
    let k, v = arr.(i) in
    let next_pair_line =
      if i + 1 < n then node_line (fst arr.(i + 1)) else parent_next_line
    in
    let key_line = node_line k in
    let value_line = node_line v in

    (* Attach to the key with a tight next_line so comments between the key
       and value are NOT consumed as foot comments of the key — they should
       become head comments of the value instead. *)
    let k' = attach_node cur ~next_line:(key_line + 1) k in
    let k', transferred_lc =
      if key_line = value_line then
        (* Strip the line comment we just put on the key and defer it *)
        (set_lc None k', get_lc k')
      else (k', None)
    in

    let v' = attach_node cur ~next_line:next_pair_line v in
    (* Apply the transferred line comment to the value if it has none *)
    let v' =
      match transferred_lc with
      | Some _ when get_lc v' = None -> set_lc transferred_lc v'
      | _ -> v'
    in

    arr.(i) <- (k', v')
  done;
  Array.to_list arr

(* ------------------------------------------------------------------ *)
(* Entry point                                                           *)
(* ------------------------------------------------------------------ *)

(** Attach [raw_comments] to [docs] and return the annotated node list.
    [raw_comments] is the [(line, col, is_line_comment, text)] list returned by
    {!Scanner.drain_comments}. [doc_start_lines] is the source line of each
    document's [Document_start] event (the [---] line for explicit documents, or
    the first-content line for implicit ones); it is used as the upper bound for
    foot-comment collection on the preceding document, preventing standalone
    comments that precede a [---] from being mis-attached as head comments of
    the next document's root. Comments after the last document are discarded. *)
let attach ~doc_start_lines (docs : node list)
    (raw_comments : (int * int * bool * string) list) : node list =
  if raw_comments = [] then docs
  else begin
    let cur = make_cursor raw_comments in
    let arr = Array.of_list docs in
    let starts = Array.of_list doc_start_lines in
    let n = Array.length arr in
    for i = 0 to n - 1 do
      (* Use the next document's --- line as boundary so that comments between
         the end of document i and the --- of document i+1 become foot comments
         of document i rather than head comments of document i+1. *)
      let next =
        if i + 1 < n && i + 1 < Array.length starts then starts.(i + 1)
        else max_int
      in
      arr.(i) <- attach_node cur ~next_line:next arr.(i)
    done;
    Array.to_list arr
  end
