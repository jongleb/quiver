(** YAML pretty-printer. Converts a list of parsed nodes back into a YAML
    string, preserving scalar styles and collection flow/block style. *)

val to_yaml : Types.node list -> string
(** Serialize [docs] (one {!Types.node} per document) as a YAML string.

    Scalar styles are preserved:
    - [Plain] scalars are written unquoted.
    - [Single_quoted] scalars use ['…'] encoding (embedded [''] for [']).
    - [Double_quoted] scalars use ["…"] encoding with backslash escapes.
    - [Literal] block scalars use [|], [|-], or [|+] as appropriate.
    - [Folded] block scalars use [>], [>-], or [>+] as appropriate.

    Collection style is preserved: flow sequences/mappings emit [{…}]/[[…]];
    block sequences/mappings use dash-and-newline / key-colon notation.

    [Alias_node] values are emitted as [*name] references; the caller must
    ensure the corresponding anchored node appears earlier in the stream. *)

val to_plain_yaml :
  ?strict:bool -> ?expansion_limit:int -> Types.node list -> string
(** Like {!to_yaml} but restricted to plain YAML: aliases are expanded, anchor
    declarations are stripped, tags are stripped (or raise {!Types.Error}
    [(Plain_error _)] when [~strict:true]), complex mapping keys always raise
    {!Types.Error} [(Plain_error _)], and all flow collections are converted to
    block style.

    [expansion_limit] caps the total number of nodes visited during alias
    expansion. Raises {!Types.Error} [(Expansion_limit_exceeded _)] if exceeded.
    Defaults to {!Types.default_expansion_limit}. *)
