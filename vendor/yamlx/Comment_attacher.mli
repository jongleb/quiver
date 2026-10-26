(** Comment attachment pass.

    Attaches the raw comments collected by the Scanner to the appropriate nodes
    in the parsed AST. See {!attach} for details. *)

val attach :
  doc_start_lines:int list ->
  Types.node list ->
  (int * int * bool * string) list ->
  Types.node list
(** Attach [raw_comments] to [docs] and return the annotated node list.

    [raw_comments] is the [(line, col, is_line_comment, text)] list returned by
    {!Scanner.drain_comments} after the full stream has been consumed. Documents
    are treated as top-level siblings.

    This is a best-effort heuristic pass. The attachment rules are:
    - Standalone comment lines before a node → {!Types.node} [head_comments].
    - A comment on the same line as a node → {!Types.node} [line_comment]. For
      key-value pairs where key and value are on the same line, the comment is
      attached to the value.
    - Standalone comment lines after a collection's last child (before the next
      sibling) → [foot_comments] of the collection.

    Comments that cannot be attributed to any node are silently discarded.
    Behavior is subject to change as the heuristics are refined. *)
