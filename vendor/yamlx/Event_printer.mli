(** Convert a parser event list to the yaml-test-suite tree notation. The tree
    notation is a human-readable, line-by-line representation of the event
    stream, used by the test suite to specify expected output.

    Each event maps to one line:
    - [+STR] / [-STR]: stream start / end
    - [+DOC] [---] / [-DOC] [...]: document start / end
    - [+MAP] [{}] / [-MAP]: mapping start ([{}] when flow) / end
    - [+SEQ] [[]] / [-SEQ]: sequence start when block / flow / end
    - [=VAL] [&anchor] [<tag>] followed by a style character and value: scalar,
      where the style character is [:] plain, a single-quote for single-quoted,
      a double-quote for double-quoted, [|] literal block, [>] folded block
    - [=ALI] [*name]: alias

    Special characters in scalar values are escaped: In scalar values, actual
    newlines escape as backslash-n, tabs as backslash-t, and backslashes as
    double-backslash. *)

val event_to_line : Types.event -> string
(** Render a single event as a tree-format line. *)

val to_tree : Types.event list -> string
(** Convert an event list to the multi-line tree string. Each event becomes one
    line; the result ends with a newline. *)

val normalize_tree : string -> string list
(** Split a tree string into trimmed, non-empty lines for comparison. Strips the
    leading whitespace used for visual indentation in the test suite's reference
    files. *)

val diff_trees : expected:string -> actual:string -> string option
(** Return a human-readable diff between [expected] and [actual] tree strings,
    or [None] if they are equal. *)
