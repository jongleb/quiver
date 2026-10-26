(** YAMLx — pure-OCaml YAML 1.2 parser.

    Typical usage:
    {[
      (* Read a single-document config file — most common pattern *)
      match YAMLx.Value.of_yaml_file "config.yaml" with
      | Ok value  -> (* process value *)
      | Error msg -> (* handle error *)

      (* Parse a YAML string into a single typed value — raising variant *)
      let value = YAMLx.Value.of_yaml_exn "answer: 42\nflag: true"

      (* Multi-document YAML stream *)
      match YAMLx.Values.of_yaml "doc1\n---\ndoc2" with
      | Ok docs  -> (* process docs *)
      | Error msg -> (* handle error *)

      (* Serialize typed values back to YAML *)
      let yaml = YAMLx.Values.to_yaml docs

      (* Parse preserving the full AST (tags, anchors, positions) *)
      let nodes = YAMLx.Nodes.of_yaml_exn "- foo\n- bar"

      (* Serialize nodes back to YAML *)
      let yaml = YAMLx.Nodes.to_yaml nodes

      (* Create a YAML value programmatically *)
      let value = YAMLx.Value.Build.(
        map [
          "name", string "Alice";
          "scores", seq [ int 95; int 87 ]
        ]
      )
    ]}

    All errors are reported by raising {!Error}. Use {!Value.of_yaml} or
    {!Value.of_yaml_file} to get a [result] instead of raising. *)

(** {1 Source positions} *)

type pos = {
  line : int;
  column : int;  (** 0-based Unicode codepoint column *)
  column_bytes : int;  (** 0-based UTF-8 byte column *)
  offset : int;  (** codepoint index from the start of the input *)
  offset_bytes : int;  (** UTF-8 byte offset from the start of the input *)
}

(** A location in the YAML source text. [line] is 1-based. [column] and
    [column_bytes] are 0-based distances from the start of the line, in
    codepoints and UTF-8 bytes respectively. [offset] and [offset_bytes] are
    absolute distances from the start of the input, measured the same way. The
    byte fields make it easy to slice the original [string] without re-encoding.
*)

type loc = { start_pos : pos; end_pos : pos }
(** A source range. [start_pos] is the first character of the node; [end_pos] is
    the position immediately after the last character. *)

val zero_pos : pos
(** The position at the very start of an empty input: [line=1], [column=0],
    [offset=0], all byte fields [0]. Useful for constructing nodes
    programmatically when source positions are not meaningful. *)

val zero_loc : loc
(** A zero-length location at the start of an empty input: both [start_pos] and
    [end_pos] equal {!zero_pos}. Useful for constructing nodes programmatically
    when source locations are not meaningful. *)

(** {1 YAML schema} *)

type schema =
  | Yaml_1_2
  | Yaml_1_1
      (** YAML schema used to resolve untagged plain scalars to typed values.

          - {!Yaml_1_2} (the default): YAML 1.2 JSON schema. Booleans are only
            [true]/[false]; octal uses [0o…] prefix; sexagesimal not recognised.
          - {!Yaml_1_1}: YAML 1.1 schema. Extended booleans
            ([yes]/[no]/[on]/[off] etc.), [0…]-prefixed octal, sexagesimal
            integers and floats, and merge-key ([<<]) expansion. Use this to
            read legacy YAML files.

          New projects should use YAML 1.2. Pass [~schema:Yaml_1_1] to
          {!Values.of_yaml_exn} (and friends) when reading older files. A
          [%YAML 1.1] or [%YAML 1.2] directive in the stream selects the schema
          automatically for that document (use [~strict_schema:true] to make a
          mismatch an error instead). *)

(** {1 Errors} *)

type yaml_error = { msg : string; loc : loc }

type error =
  | Scan_error of yaml_error
      (** Invalid character sequence or encoding error detected by the scanner.
          Carries a position. *)
  | Parse_error of yaml_error
      (** Well-formed tokens in an invalid order detected by the parser. Carries
          a position. *)
  | Expansion_limit_exceeded of int
      (** Alias expansion visited more nodes than the configured limit. The
          payload is the limit that was exceeded. See
          {!default_expansion_limit}. *)
  | Depth_limit_exceeded of int
      (** YAML nesting depth exceeded the configured maximum during composition.
          The payload is the limit that was exceeded. See {!default_max_depth}.
      *)
  | Printer_error of string
      (** A feature unsupported by the plain-YAML printer was encountered (e.g.
          a tag, a complex mapping key). *)
  | Document_count_error of string
      (** The input contained the wrong number of documents for a
          single-document operation. *)
  | Schema_error of yaml_error
      (** A schema conflict: the document's [%YAML] directive disagrees with the
          requested schema (when [~strict_schema:true]), or a plain scalar is
          ambiguous between YAML 1.1 and 1.2 (when [~reject_ambiguous:true]). *)
  | Simplicity_error of yaml_error
      (** A YAML feature not allowed in plain mode was encountered: an anchor,
          alias, explicit tag, or (in YAML 1.1 mode) a merge key ([<<]). Raised
          when [~plain:true] is passed to {!Values} functions. *)
  | Duplicate_key_error of yaml_error
      (** A mapping contains a duplicate key. Raised when [~strict_keys:true] is
          passed to {!Values} functions. The location points to the second
          (duplicate) occurrence. *)
  | Cycle_error of yaml_error
      (** A cyclic alias was encountered during value resolution. The YAML
          structure is valid (e.g. [&doc {a: *doc}]) but cannot be represented
          as a finite [value] tree. The location points to the alias that closes
          the cycle. *)

exception Error of error
(** The single exception raised by this library. Match on the payload to
    distinguish error kinds:
    {[
      match YAMLx.Nodes.of_yaml_exn input with
      | nodes -> ...
      | exception YAMLx.Error (YAMLx.Scan_error e) -> ...
      | exception YAMLx.Error (YAMLx.Parse_error e) -> ...
      | exception YAMLx.Error (YAMLx.Depth_limit_exceeded n) -> ...
      | exception YAMLx.Error _ -> ...
    ]} *)

val format_loc : ?file:string -> loc -> string
(** Default location formatter used by {!catch_errors} and
    {!register_exception_printers}.

    The output format depends on the extent of [loc]:
    - Zero-width (start = end): ["line 3, column 8"]
    - Single-line range: ["line 3, columns 8-11"]
    - Multi-line range: ["lines 3-12, columns 8-4"]

    When [~file] is given, a ["file <name>, "] prefix is prepended, e.g.
    ["file foo.yaml, line 3, columns 8-11"].

    Columns are 0-based Unicode codepoint offsets from the start of the line,
    matching the {!pos} fields [column] (not [column_bytes]). *)

val default_format_loc : ?file:string -> loc -> string
[@@deprecated "Use 'format_loc' instead."]
(** @deprecated After version 0.1.0, this function was renamed {!format_loc}. *)

val catch_errors :
  ?file:string ->
  ?format_loc:(?file:string -> loc -> string) ->
  (unit -> 'a) ->
  ('a, string) result
(** Catch {!Error} and return [Ok _] or [Error msg].

    When [~file] is given it is prepended to every error message: positional
    errors (scan/parse/schema) become
    ["file foo.yaml, line L, columns C1-C2: msg"] and non-positional errors
    become ["file foo.yaml: msg"].

    [~format_loc] overrides how source locations are formatted (default:
    {!format_loc}). Provide a custom implementation to adapt the output for
    editors, LSP servers, or structured logging. *)

val register_exception_printers :
  ?format_loc:(?file:string -> loc -> string) -> unit -> unit
(** Register a printer for {!Error} so it displays legibly in uncaught-exception
    output. [~format_loc] overrides location formatting (default:
    {!default_format_loc}). *)

val default_expansion_limit : int
(** Default node-visit budget for alias expansion (1,000,000). *)

val default_max_depth : int
(** Default maximum nesting depth (512). *)

val show_yaml_error :
  ?format_loc:(?file:string -> loc -> string) -> yaml_error -> string
(** Format a {!yaml_error} as ["location: message"]. Uses {!default_format_loc}
    by default; pass [~format_loc] to customise location formatting. *)

(** {1 Scalar styles} *)

(** How a scalar value was written in the source. Preserved in AST nodes so
    callers can distinguish, for example, a quoted empty string from an unquoted
    null. *)
type scalar_style =
  | Plain  (** unquoted, e.g. [foo] *)
  | Single_quoted  (** e.g. ['foo'] *)
  | Double_quoted  (** e.g. ["foo"] *)
  | Literal  (** block scalar [|]: newlines preserved *)
  | Folded  (** block scalar [>]: newlines folded to spaces *)


(** {1 AST nodes} *)

(** An in-memory representation of a parsed YAML document that preserves all
    source-level detail: tags, anchors, scalar styles, source positions, and —
    on a best-effort basis — comments.

    {b Comment preservation} is best-effort. Comments inside flow collections
    are dropped. The attachment rules may change in future versions.

    - [head_comments]: standalone comment lines immediately before the node.
      Each string is one comment line's text, without the leading ['#'].
    - [line_comment]: a comment on the same source line as the node, after its
      content. Text does not include the leading ['#'].
    - [foot_comments] (collections only): standalone comment lines after the
      last child of the collection, before the next sibling. *)
type node =
  | Scalar_node of {
      anchor : string option;  (** [&name] if present *)
      tag : string option;  (** resolved tag URI if present *)
      value : string;
      style : scalar_style;
      loc : loc;
      height : int;  (** always 1 for scalars *)
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Sequence_node of {
      anchor : string option;
      tag : string option;
      items : node list;
      flow : bool;  (** true for [[a, b]] style, false for block *)
      loc : loc;
      height : int;  (** 1 + max height of items, or 1 if empty *)
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Mapping_node of {
      anchor : string option;
      tag : string option;
      pairs : (node * node) list;
      flow : bool;  (** true for [{a: b}] style, false for block *)
      loc : loc;
      height : int;  (** 1 + max height of keys and values, or 1 if empty *)
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }
  | Alias_node of {
      name : string;  (** the anchor name, without the [*] *)
      resolved : node Lazy.t;
          (** The node this alias refers to. Lazy to allow cycles (e.g. a node
              that contains an alias to itself). Force with [Lazy.force] when
              traversing. *)
      loc : loc;
      height : int;  (** always 1 for alias nodes *)
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }


(** {1 Typed values} *)

(** A YAML value resolved according to the YAML 1.2 JSON schema.

    Plain (unquoted) scalars are matched against the following patterns:
    - [null], [Null], [NULL], [~], or empty string → {!Null}
    - [true]/[True]/[TRUE]/[false]/[False]/[FALSE] → {!Bool}
    - Decimal, [0x…] hex, or [0o…] octal integers → {!Int}
    - Decimal or scientific floats; [.inf], [.nan] variants → {!Float}
    - Everything else, and all quoted or block scalars → {!String}

    Each constructor carries a {!type-loc} giving the source range of the
    corresponding YAML node. Use {!Value.equal} for location-independent
    structural equality.

    To build new values more conveniently, use {!Value.Build.bool},
    {!Value.Build.string}, etc. *)
type value =
  | Null of loc
  | Bool of loc * bool
  | Int of loc * int64
  | Float of loc * float
  | String of loc * string
  | Seq of loc * value list
  | Map of loc * (loc * value * value) list

val value_loc : value -> loc
[@@deprecated "Use Value.loc instead."]
(** Return the source location carried by a {!value}.
    @deprecated Use {!Value.loc} instead. *)

(** {1 Node operations} *)

(**/**)

val node_height : node -> int
(** Precomputed subtree height (O(1)). Internal — subject to change. *)

(**/**)

(** Operations on the lossless AST node representation.

    Use [Nodes] when you need full fidelity: tags, anchors, scalar styles,
    source positions, or comment preservation. For plain data extraction and
    round-tripping typed values, {!Values} is simpler. *)
module Nodes : sig
  type t = node list
  (** One {!node} per YAML document in the input stream. *)

  val of_yaml : ?file:string -> ?max_depth:int -> string -> (t, string) result
  (** Like {!of_yaml_exn} but returns [Ok nodes] on success or [Error msg] on
      any failure. Does not raise. [~file] is prepended to error messages (see
      {!catch_errors}). *)

  val of_yaml_file : ?max_depth:int -> string -> (t, string) result
  (** Like {!of_yaml} but reads the YAML from the file at the given path.
      File-read errors are returned as [Error msg]. The file path is
      automatically prepended to all error messages. *)

  val of_yaml_exn : ?max_depth:int -> string -> t
  (** Parse a YAML string and return one node per document. Use this when you
      need tags, anchors, scalar styles, source positions, or comments. For
      plain data extraction prefer {!Values.of_yaml_exn}.

      Raises {!Error} [(Scan_error _)] on invalid YAML syntax, [(Parse_error _)]
      on a malformed token stream, [(Depth_limit_exceeded _)] when nesting
      exceeds [max_depth] (default: {!default_max_depth}). *)

  val to_yaml : t -> string
  (** Serialize nodes back to a YAML string. Scalar styles and flow/block mode
      are preserved. The output round-trips through {!of_yaml_exn} to equivalent
      nodes. Does not raise. *)

  val to_yaml_file : string -> t -> unit
  (** [to_yaml_file path nodes] serializes nodes to YAML (via {!to_yaml}) and
      writes the result to [path], overwriting any existing file. Raises
      [Sys_error] on file I/O failure. *)

  val to_plain_yaml_exn : ?strict:bool -> ?expansion_limit:int -> t -> string
  (** Like {!to_yaml} but produces a plain subset of YAML:
      - Aliases are expanded; anchor declarations are stripped.
      - Tags are stripped unless [~strict:true], in which case they raise
        {!Error} [(Printer_error _)].
      - Complex (non-scalar) mapping keys raise {!Error} [(Printer_error _)].
      - Flow collections are converted to block style.

      Raises {!Error} [(Expansion_limit_exceeded _)] when alias expansion
      exceeds [expansion_limit] (default: {!default_expansion_limit}). *)

  val to_plain_yaml_file :
    ?strict:bool -> ?expansion_limit:int -> string -> t -> (unit, string) result
  (** [to_plain_yaml_file path nodes] serializes nodes to plain YAML (via
      {!to_plain_yaml_exn}) and writes the result to [path], overwriting any
      existing file. Returns [Error msg] on serialization failure (same errors
      as {!to_plain_yaml_exn}). Raises [Sys_error] on file I/O failure. *)

  val has_comments : t -> bool
  (** [has_comments nodes] is [true] if any node in the list (one per document)
      contains a comment. Equivalent to [List.exists Node.has_comments nodes].
  *)
end

(** Operations on a single lossless AST node.

    [Node] provides the single-node interface analogous to {!Value}: a [type t]
    alias and node-level utilities. For parsing and serialising whole documents
    use {!Nodes}. *)
module Node : sig
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
        resolved : t Lazy.t;
        loc : loc;
        height : int;
        head_comments : string list;
        line_comment : string option;
        foot_comments : string list;
      }
        (** Alias for {!YAMLx.node}. Exposes the constructors under the [Node]
            namespace so that [Node.Scalar_node], [Node.Mapping_node], etc. work
            alongside [YAMLx.Scalar_node]. *)

  val has_comments : t -> bool
  (** [has_comments node] is [true] if [node] or any of its descendants carries
      at least one comment (head, line, or foot). *)

end

(** {1 Value operations} *)

val equal_value : value -> value -> bool
[@@deprecated "Use Value.equal instead."]
(** Structural equality that ignores source locations.
    @deprecated Use {!Value.equal} instead. *)

(**/**)

val value_height : value -> int
(** Compute subtree height (O(n)). Internal — subject to change. *)

(**/**)

(** Operations on typed YAML values for multi-document streams.

    A YAML file may contain multiple documents separated by [---] markers.
    [Values] handles all of them at once, returning one {!value} per document.
    For the common case of a single-document config file, prefer {!Value}.

    Resolves YAML scalars to typed OCaml values ({!Null}, {!Bool}, {!Int},
    {!Float}, {!String}, {!Seq}, {!Map}). The default schema is YAML 1.2; pass
    [~schema:Yaml_1_1] for legacy files. *)
module Values : sig
  type t = value list
  (** One {!value} per YAML document in the input stream. *)

  val of_yaml :
    ?file:string ->
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    (t, string) result
  (** Parse a YAML string and resolve each document to a typed value. Returns
      [Ok docs] on success or [Error msg] on any failure. Does not raise.
      [~file] is prepended to error messages.

      [~schema] selects the resolver (default: {!Yaml_1_2}). A [%YAML] directive
      in the stream overrides [~schema] per document; use [~strict_schema:true]
      to turn such a mismatch into an error instead.

      [~reject_ambiguous:true] (only meaningful with the default [Yaml_1_2]
      schema) raises {!Schema_error} for plain scalars that would resolve
      differently under YAML 1.1 (e.g. [yes], [0755], sexagesimal, [<<] mapping
      keys).

      [~plain:true] restricts input to plain YAML: raises {!Simplicity_error} if
      any anchor, alias, or explicit tag is encountered, and (in YAML 1.1 mode)
      if any merge key ([<<]) is encountered.

      [~strict_keys:true] raises {!Duplicate_key_error} when a mapping contains
      the same key more than once. Without this flag the last occurrence
      silently wins. *)

  val of_yaml_file :
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    (t, string) result
  (** Like {!of_yaml} but reads the YAML from the file at the given path.
      File-read errors are returned as [Error msg]. The file path is
      automatically prepended to all error messages. *)

  val of_yaml_exn :
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    t
  (** Like {!of_yaml} but raises instead of returning a result.

      Raises {!Error} [(Scan_error _)] on invalid YAML syntax, [(Parse_error _)]
      on a malformed token stream, [(Depth_limit_exceeded _)] when nesting
      exceeds [max_depth] (default: {!default_max_depth}),
      [(Expansion_limit_exceeded _)] when alias expansion exceeds
      [expansion_limit] (default: {!default_expansion_limit}),
      [(Schema_error _)] on schema conflicts (see [~strict_schema] and
      [~reject_ambiguous]), [(Simplicity_error _)] on disallowed YAML features
      (see [~plain]), [(Duplicate_key_error _)] on duplicate mapping keys (see
      [~strict_keys]). *)

  val of_nodes_exn :
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    node list ->
    t
  (** Resolve a list of AST nodes to typed values. The [%YAML] version directive
      is not available from bare nodes, so only the explicit [~schema] is used
      (no per-document override).

      Raises {!Error} [(Expansion_limit_exceeded _)] on excessive alias
      expansion, [(Schema_error _)] on ambiguity (see [~reject_ambiguous]),
      [(Simplicity_error _)] on disallowed features (see [~plain]),
      [(Duplicate_key_error _)] on duplicate keys (see [~strict_keys]). *)

  val of_nodes :
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    node list ->
    (t, string) result
  (** Like {!of_nodes_exn} but returns [Ok values] on success or [Error msg] on
      failure. Does not raise. *)

  val one_of_yaml :
    ?file:string ->
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    (value, string) result
  [@@deprecated "Use Value.of_yaml instead."]
  (** @deprecated Use {!Value.of_yaml} instead. *)

  val one_of_yaml_file :
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    (value, string) result
  [@@deprecated "Use Value.of_yaml_file instead."]
  (** @deprecated Use {!Value.of_yaml_file} instead. *)

  val one_of_yaml_exn :
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    value
  [@@deprecated "Use Value.of_yaml_exn instead."]
  (** @deprecated Use {!Value.of_yaml_exn} instead. *)

  val to_nodes : ?strict_keys:bool -> t -> node list
  (** Convert typed values back to AST nodes. Each {!value} becomes one {!node}
      document. Source locations in the returned nodes are zeroed.

      [~strict_keys] (default: [true]) controls whether duplicate mapping keys
      are rejected. The default is the opposite of the import-side default
      ([false]) because exporting a map with duplicate keys is almost always a
      bug: the second key silently shadows the first, and many YAML
      implementations reject duplicate keys outright. Raises
      {!Duplicate_key_error} when a mapping in any document contains the same
      key more than once. Pass [~strict_keys:false] to allow duplicates through.

      String scalars are styled as follows:
      - Strings that look like YAML keywords ([null], [true], numbers) →
        double-quoted.
      - Strings longer than 70 characters with internal newlines and only safe
        characters → [Literal] block style ([|] or [|-]).
      - Strings longer than 70 characters with spaces but no newlines and only
        printable characters → [Folded] block style ([>] or [>-]), with line
        breaks inserted at word boundaries to keep lines near 70 characters.
      - Everything else → [Plain] or [Double_quoted]. *)

  val to_yaml : ?strict_keys:bool -> t -> string
  (** Serialize typed values to a YAML string. Equivalent to
      [Nodes.to_yaml (to_nodes ?strict_keys values)].

      [~strict_keys] (default: [true]) — see {!to_nodes}. *)

  val to_yaml_file : ?strict_keys:bool -> string -> t -> unit
  (** [to_yaml_file path values] serializes values to YAML (via {!to_yaml}) and
      writes the result to [path], overwriting any existing file. Raises
      [Sys_error] on file I/O failure.

      [~strict_keys] (default: [true]) — see {!to_nodes}. *)
end

(** Single-document interface for typed YAML values.

    Most YAML config files contain exactly one document. [Value] provides a
    simpler interface than {!Values} for this common case: functions return a
    single {!value} rather than a list, and the [_exn] variants raise on wrong
    document count just as they do on parse errors.

    [Value] and {!Values} share the same underlying parser and resolver; the
    only difference is that [Value] functions check that the input contains
    exactly one document and unwrap the list. *)
module Value : sig
  type t = value =
    | Null of loc
    | Bool of loc * bool
    | Int of loc * int64
    | Float of loc * float
    | String of loc * string
    | Seq of loc * t list
    | Map of loc * (loc * t * t) list
        (** Alias for {!YAMLx.value}. Exposes the constructors under the [Value]
            namespace so that, for example, [Value.Null loc] and
            [YAMLx.Null loc] refer to the same constructor. *)

  val of_yaml :
    ?file:string ->
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    (value, string) result
  (** Parse a YAML string that must contain exactly one document and return its
      typed value. Returns [Ok v] on success or [Error msg] on any failure,
      including zero or more than one document. Does not raise. [~file] is
      prepended to error messages. Options are the same as {!Values.of_yaml}. *)

  val of_yaml_exn :
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    value
  (** Parse a YAML string expecting exactly one document and return its typed
      value. Raises {!Error} [(Document_count_error _)] if the input contains
      zero or more than one document. Other failures same as
      {!Values.of_yaml_exn}. *)

  val of_yaml_file :
    ?max_depth:int ->
    ?expansion_limit:int ->
    ?schema:schema ->
    ?strict_schema:bool ->
    ?reject_ambiguous:bool ->
    ?plain:bool ->
    ?strict_keys:bool ->
    string ->
    (value, string) result
  (** Like {!of_yaml} but reads the YAML from the file at the given path.
      File-read errors and wrong document count are returned as [Error msg]. The
      file path is automatically prepended to all error messages. *)

  val to_yaml : ?strict_keys:bool -> value -> string
  (** Serialize a single typed value to a YAML string. Equivalent to
      [Values.to_yaml ?strict_keys [v]].

      [~strict_keys] (default: [true]) — see {!Values.to_nodes}. *)

  val to_yaml_file : ?strict_keys:bool -> string -> value -> unit
  (** [to_yaml_file path v] serializes [v] to YAML (via {!to_yaml}) and writes
      the result to [path], overwriting any existing file. Raises [Sys_error] on
      file I/O failure.

      [~strict_keys] (default: [true]) — see {!Values.to_nodes}. *)

  val equal : value -> value -> bool
  (** Structural equality that ignores source locations. Two values are equal
      when they represent the same YAML data regardless of where they appear in
      the source. *)

  val compare : value -> value -> int
  (** Total order on values that ignores source locations. Constructor order:
      [Null < Bool < Int < Float < String < Seq < Map]. Within the same
      constructor, values are ordered naturally (booleans by [false < true],
      integers and floats by numeric value, strings lexicographically, sequences
      and maps lexicographically by element). *)


  val loc : value -> loc
  (** Return the source location carried by a {!value}. Every constructor stores
      the location of the corresponding YAML node. Useful for building error
      messages in catch-all match arms:
      {[
      match x with
      | Int (_, i) -> i
      | bad ->
          ksprintf failwith "%s: expected an int"
            (bad |> YAMLx.Value.loc |> YAMLx.format_loc)
      ]} *)

  (** Convenience constructors that use {!zero_loc} as the source location.

      These are useful when building {!value} trees programmatically and there's
      no meaningful source location or the tree is destined to be written as a
      YAML document straight away.

      {v
      module Y = YAMLx.Value.Build

      let v =
        Y.map [
          "name", Y.string "Alice";
          "scores", Y.seq [ Y.int 95; Y.int 87 ]
        ]
      v}

      or

      {v
      let v =
        let open YAMLx.Value.Build in
        map [
          "name", string "Alice";
          "scores", seq [ int 95; int 87 ]
        ]
      v} *)
  module Build : sig
    val null : value
    (** [Null zero_loc] *)

    val bool : bool -> value
    (** [Bool (zero_loc, b)] *)

    val int : int -> value
    (** [Int (zero_loc, Int64.of_int i)]. Use this for ordinary OCaml ints. See
        also {!int64}. *)

    val int64 : int64 -> value
    (** [Int (zero_loc, i)]. *)

    val float : float -> value
    (** [Float (zero_loc, f)] *)

    val string : string -> value
    (** [String (zero_loc, s)] *)

    val seq : value list -> value
    (** [Seq (zero_loc, items)] *)

    val map : (string * value) list -> value
    (** [Map (zero_loc, pairs)] where each key is a plain string and each pair
        also uses [zero_loc]. If you need to produce non-string keys, use the
        {!Map} constructor directly. *)
  end
end

(**/**)

(** {1 Event stream — internal / subject to change} *)

type event_kind =
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

type event = { kind : event_kind; start_pos : pos; end_pos : pos }

val parse_events : string -> event list
val events_to_tree : event list -> string
val diff_event_trees : expected:string -> actual:string -> string option

(**/**)
