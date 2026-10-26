(** YAML tag resolver. Resolves composed node trees into typed value trees.

    Supports YAML 1.2 (JSON schema, the default) and YAML 1.1.

    YAML 1.2 JSON schema rules (plain scalars only):
    - null : [null|Null|NULL|~|""]
    - bool : [true|True|TRUE|false|False|FALSE]
    - int : decimal, [0x…] hex, [0o…] octal
    - float : decimal/scientific; [.inf], [.nan] variants
    - str : everything else; all non-plain scalars

    Additional YAML 1.1 rules (plain scalars only):
    - bool : also [y|Y|yes|Yes|YES|n|N|no|No|NO|on|On|ON|off|Off|OFF]
    - int : also octal [0[0-7]+] and sexagesimal ([H:MM:SS])
    - float : also sexagesimal ([H:MM:SS.s])
    - merge : plain [<<] mapping key triggers merge-key expansion *)

val resolve_documents :
  ?expansion_limit:int ->
  ?schema:Types.schema ->
  ?strict_schema:bool ->
  ?reject_ambiguous:bool ->
  ?plain:bool ->
  ?strict_keys:bool ->
  ((int * int) option * Types.node) list ->
  Types.value list
(** Resolve a list of [(version, node)] pairs into typed values.

    [version] is the [%YAML] directive from the document (e.g. [(1,1)]). When it
    disagrees with [schema] and [strict_schema = true], {!Types.Schema_error} is
    raised. Otherwise the directive overrides [schema] for that document.

    [expansion_limit] caps total nodes visited during alias expansion. Defaults
    to {!Types.default_expansion_limit}.

    [schema] sets the default schema (default: {!Types.Yaml_1_2}).

    [strict_schema] raises {!Types.Schema_error} when a document's [%YAML]
    directive conflicts with [schema] (default: [false]).

    [reject_ambiguous] raises {!Types.Schema_error} for plain scalars that would
    resolve differently under YAML 1.1 (only meaningful with
    [schema = Yaml_1_2], default: [false]).

    [plain] raises {!Types.Simplicity_error} if any anchor, alias, or explicit
    tag is encountered, or (in YAML 1.1 mode) if a merge key ([<<]) is
    encountered (default: [false]).

    [strict_keys] raises {!Types.Duplicate_key_error} when a mapping contains
    the same key more than once. Without this flag the last occurrence wins
    (default: [false]). *)
