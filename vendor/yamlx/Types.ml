(** Core types shared by all YAMLx modules. This module defines the data
    structures that flow through the parsing pipeline: positions and errors,
    then tokens (Scanner → Parser), events (Parser → Composer), and AST nodes
    (Composer → user).

    Pipeline overview: string/channel └─▶ Reader (UTF-8 decoding, line tracking)
    └─▶ Scanner (tokenization) → token list └─▶ Parser (grammar) → event list
    └─▶ Composer (node building) → node list └─▶ Resolver (type resolution) →
    value list *)

(** {1 Source positions} *)

type pos = {
  line : int;
  column : int;  (** 0-based Unicode codepoint column *)
  column_bytes : int;  (** 0-based UTF-8 byte column *)
  offset : int;  (** codepoint index from the start of the input *)
  offset_bytes : int;  (** UTF-8 byte offset from the start of the input *)
}
(** A location in the source input. [line] is 1-based; [column] and
    [column_bytes] are 0-based distances from the start of the current line,
    measured in Unicode codepoints and UTF-8 bytes respectively. [offset] and
    [offset_bytes] are absolute distances from the start of the input, measured
    the same way. *)

let zero_pos =
  { line = 1; column = 0; column_bytes = 0; offset = 0; offset_bytes = 0 }

type loc = { start_pos : pos; end_pos : pos }
(** A source range from [start_pos] (inclusive) to [end_pos] (exclusive). *)

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
      (** A schema conflict was detected: either the document's [%YAML]
          directive disagrees with the requested schema (when
          [~strict_schema:true]), or a plain scalar is ambiguous between YAML
          1.1 and 1.2 (when [~reject_ambiguous:true]). *)
  | Simplicity_error of yaml_error
      (** A YAML feature not allowed in plain mode was encountered: an anchor,
          alias, explicit tag, or (in YAML 1.1 mode) a merge key ([<<]). Raised
          when [~plain:true] is passed to resolver functions. *)
  | Duplicate_key_error of yaml_error
      (** A mapping contains a duplicate key. Raised when [~strict_keys:true] is
          passed to resolver functions. The location points to the second
          (duplicate) occurrence. *)
  | Cycle_error of yaml_error
      (** A cyclic alias was encountered during value resolution. The YAML
          structure is valid but cannot be represented as a finite [value] tree.
          The location points to the alias node that closes the cycle. *)

exception Error of error
(** The single exception raised by this library. Match on the payload to
    distinguish error kinds. *)

(** Default maximum number of nodes that may be visited during alias expansion.
    Applies to both {!Resolver.resolve_documents} and {!Printer.to_plain_yaml}.
*)
let default_expansion_limit = 1_000_000

(** Default maximum nesting depth accepted during composition. Inputs deeper
    than this raise {!Error (Depth_limit_exceeded _)}. *)
let default_max_depth = 512

(** YAML schema used to resolve plain scalars to typed values. *)
type schema =
  | Yaml_1_2
      (** YAML 1.2 JSON schema (the default). Booleans are only [true]/[false];
          octal uses [0o…] prefix; sexagesimal notation is not recognised. *)
  | Yaml_1_1
      (** YAML 1.1 schema. Adds extended booleans ([yes]/[no]/[on]/[off] etc.),
          [0…] octal, sexagesimal integers and floats, and merge-key ([<<])
          expansion. Use this to read legacy YAML files. *)

let scan_error pos fmt =
  Printf.ksprintf
    (fun msg ->
      raise
        (Error (Scan_error { msg; loc = { start_pos = pos; end_pos = pos } })))
    fmt

let parse_error pos fmt =
  Printf.ksprintf
    (fun msg ->
      raise
        (Error (Parse_error { msg; loc = { start_pos = pos; end_pos = pos } })))
    fmt

(** {1 Scalar styles} *)

(** How a scalar was written in the YAML source. The style is preserved through
    the parsing pipeline because it affects tag resolution and round-trip
    fidelity. *)
type scalar_style =
  | Plain  (** unquoted, e.g. [foo] *)
  | Single_quoted  (** e.g. ['foo'] *)
  | Double_quoted  (** e.g. ['foo'] *)
  | Literal  (** block scalar [|]: newlines preserved *)
  | Folded  (** block scalar [>]: newlines folded to spaces *)

(** {1 Tokens — Scanner output} *)

(** A token represents one lexical element. The YAML grammar requires the
    scanner to emit synthetic structural tokens (BLOCK_SEQUENCE_START,
    BLOCK_MAPPING_START, BLOCK_END) in addition to the characters that appear in
    the source. *)
type token_kind =
  | Stream_start
  | Stream_end
  | Directive of string * string
      (** [Directive (name, value)]: e.g. [%YAML 1.2] or [%TAG ! !foo/] *)
  | Document_start  (** [---] *)
  | Document_end  (** [...] *)
  | Block_sequence_start
      (** Synthetic: emitted when a block sequence begins (indentation increases
          to a new level). *)
  | Block_mapping_start  (** Synthetic: emitted when a block mapping begins. *)
  | Block_end
      (** Synthetic: emitted when indentation decreases, closing one or more
          block collections. *)
  | Flow_sequence_start  (** \[ *)
  | Flow_sequence_end  (** \] *)
  | Flow_mapping_start  (** \{ *)
  | Flow_mapping_end  (** \} *)
  | Block_entry  (** [-] followed by whitespace or newline *)
  | Flow_entry  (** [,] *)
  | Key  (** [?] (explicit) or synthetic (implicit key) *)
  | Value  (** [:] followed by whitespace or end-of-line *)
  | Alias of string  (** [*name] *)
  | Anchor of string  (** [&name] *)
  | Tag of string * string  (** (handle, suffix) *)
  | Scalar of string * scalar_style

type token = { tok_kind : token_kind; tok_start_pos : pos; tok_end_pos : pos }

(** {1 Events — Parser output} *)

(** Events are the output of the Parser. They form a flat, sequential stream
    that can be processed without building a full tree in memory. The format
    mirrors the yaml-test-suite event notation. *)
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

(** {1 AST nodes — Composer output} *)

(** Nodes are the in-memory representation of a parsed YAML document. Anchors
    are resolved at compose time so [Alias_node] carries the actual target node.
    The [resolved] field is lazy to support cyclic YAML (e.g. a node that
    contains an alias to itself). *)
type node =
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
      resolved : node Lazy.t;
          (** The node this alias refers to. [Lazy.t] to allow cyclic structures
              where an alias refers to an ancestor node. *)
      loc : loc;
      height : int;
      head_comments : string list;
      line_comment : string option;
      foot_comments : string list;
    }

(** {1 Resolved values — Resolver output} *)

(** Typed values produced by applying the YAML 1.2 JSON schema to a composed
    node tree. *)
type value =
  | Null of loc
  | Bool of loc * bool
  | Int of loc * int64
  | Float of loc * float
  | String of loc * string
  | Seq of loc * value list
  | Map of loc * (loc * value * value) list

(** Structural equality that ignores source locations. *)
let rec equal_value a b =
  match (a, b) with
  | Null _, Null _ -> true
  | Bool (_, x), Bool (_, y) -> x = y
  | Int (_, x), Int (_, y) -> Int64.equal x y
  | Float (_, x), Float (_, y) -> Float.equal x y
  | String (_, x), String (_, y) -> x = y
  | Seq (_, xs), Seq (_, ys) -> List.equal equal_value xs ys
  | Map (_, ps), Map (_, qs) ->
      List.equal
        (fun (_, k1, v1) (_, k2, v2) -> equal_value k1 k2 && equal_value v1 v2)
        ps qs
  | _ -> false

(** Total order on values that ignores source locations. Constructor order: Null
    < Bool < Int < Float < String < Seq < Map. *)
let rec compare_value a b =
  match (a, b) with
  | Null _, Null _ -> 0
  | Null _, _ -> -1
  | _, Null _ -> 1
  | Bool (_, x), Bool (_, y) -> Bool.compare x y
  | Bool _, _ -> -1
  | _, Bool _ -> 1
  | Int (_, x), Int (_, y) -> Int64.compare x y
  | Int _, _ -> -1
  | _, Int _ -> 1
  | Float (_, x), Float (_, y) -> Float.compare x y
  | Float _, _ -> -1
  | _, Float _ -> 1
  | String (_, x), String (_, y) -> String.compare x y
  | String _, _ -> -1
  | _, String _ -> 1
  | Seq (_, xs), Seq (_, ys) -> List.compare compare_value xs ys
  | Seq _, _ -> -1
  | _, Seq _ -> 1
  | Map (_, ps), Map (_, qs) ->
      List.compare
        (fun (_, k1, v1) (_, k2, v2) ->
          let c = compare_value k1 k2 in
          if c <> 0 then c else compare_value v1 v2)
        ps qs

module Value_set = Set.Make (struct
  type t = value

  let compare = compare_value
end)
