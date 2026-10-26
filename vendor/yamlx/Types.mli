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

val zero_pos : pos
(** A {!pos} value with all fields set to zero. Useful as a placeholder when no
    actual source location is available. *)

(** YAML schema used to resolve plain scalars to typed values. *)
type schema =
  | Yaml_1_2
      (** YAML 1.2 JSON schema (the default). Booleans are only [true]/[false];
          octal uses [0o…] prefix; sexagesimal notation is not recognised. *)
  | Yaml_1_1
      (** YAML 1.1 schema. Adds extended booleans ([yes]/[no]/[on]/[off] etc.),
          [0…] octal, sexagesimal integers and floats, and merge-key ([<<])
          expansion. Use this to read legacy YAML files. *)

type loc = { start_pos : pos; end_pos : pos }
(** A source range: [start_pos] is the first character of the node; [end_pos] is
    the position immediately after the last character. *)

(** {1 Errors} *)

type yaml_error = { msg : string; loc : loc }

type error =
  | Scan_error of yaml_error
  | Parse_error of yaml_error
  | Expansion_limit_exceeded of int
  | Depth_limit_exceeded of int
  | Printer_error of string
  | Document_count_error of string
  | Schema_error of yaml_error
  | Simplicity_error of yaml_error
      (** A YAML feature not allowed in plain mode was encountered: an anchor,
          alias, explicit tag, or (in YAML 1.1 mode) a merge key ([<<]). *)
  | Duplicate_key_error of yaml_error
      (** A mapping contains a duplicate key. Raised when [~strict_keys:true] is
          passed to resolver functions. The location points to the second
          (duplicate) occurrence. *)
  | Cycle_error of yaml_error
      (** A cyclic alias was encountered during value resolution. The YAML
          structure is valid but cannot be represented as a finite [value] tree.
          The location points to the alias node that closes the cycle. *)

exception Error of error

val default_expansion_limit : int
(** Default node-visit budget for alias expansion (1,000,000). *)

val default_max_depth : int
(** Default maximum nesting depth (512). *)

val scan_error : pos -> ('a, unit, string, 'b) format4 -> 'a
val parse_error : pos -> ('a, unit, string, 'b) format4 -> 'a

(** {1 Scalar styles} *)

(** How a scalar was written in the YAML source. The style is preserved through
    the parsing pipeline because it affects tag resolution and round-trip
    fidelity. *)
type scalar_style =
  | Plain  (** unquoted, e.g. [foo] *)
  | Single_quoted  (** e.g. ['foo'] *)
  | Double_quoted  (** e.g. ["foo"] *)
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
  | Flow_sequence_start  (** {v [ v} *)
  | Flow_sequence_end  (** {v ] v} *)
  | Flow_mapping_start  (** {v { v} *)
  | Flow_mapping_end  (** {v } v} *)
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
*)
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
          (** The node this alias refers to. Lazy to allow cyclic structures. *)
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

val equal_value : value -> value -> bool
(** Structural equality that ignores source locations. *)

val compare_value : value -> value -> int
(** Total order on values that ignores source locations. Constructor order: Null
    < Bool < Int < Float < String < Seq < Map. *)

module Value_set : Set.S with type elt = value
(** Sets of values, ordered by [compare_value]. *)
