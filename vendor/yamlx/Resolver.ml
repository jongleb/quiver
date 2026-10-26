(** YAML tag resolver. Supports YAML 1.2 (JSON schema, the default) and YAML 1.1
    schemas.

    Schema selection per call:
    - [~schema:Yaml_1_2] (default): strict YAML 1.2 JSON schema.
    - [~schema:Yaml_1_1]: extended YAML 1.1 schema (legacy files).
    - [%YAML 1.x] directives in the stream override the default per document
      unless [~strict_schema:true] is set, in which case a mismatch raises
      {!Types.Schema_error}.

    YAML 1.2 JSON schema rules (plain scalars only):
    - null : [null|Null|NULL|~|""]
    - bool : [true|True|TRUE|false|False|FALSE]
    - int : decimal [[-+]?[0-9]+], hex [0x…], octal [0o…]
    - float : decimal/scientific; [.inf], [.nan] variants
    - str : everything else; all non-plain scalars

    Additional YAML 1.1 rules (plain scalars only):
    - bool : also [y|Y|yes|Yes|YES|n|N|no|No|NO|on|On|ON|off|Off|OFF]
    - int : also octal [0[0-7]+] and sexagesimal [H:MM:SS]
    - float : also sexagesimal [H:MM:SS.s]
    - merge : plain [<<] mapping key triggers merge-key expansion

    [~reject_ambiguous:true] (YAML 1.2 only): raises {!Types.Schema_error} for
    plain scalars that would resolve differently under YAML 1.1. *)

open Types

(** [string_forall_from i f s] is true iff [f] holds for every character of [s]
    at index [>= i]. Avoids allocating a substring. *)
let string_forall_from start f s =
  let n = String.length s in
  let i = ref start in
  while !i < n && f s.[!i] do
    incr i
  done;
  !i = n

(* ------------------------------------------------------------------ *)
(* YAML 1.2 matchers                                                     *)
(* ------------------------------------------------------------------ *)

let is_null_str s = s = "null" || s = "Null" || s = "NULL" || s = "~" || s = ""
let is_true_str_12 s = s = "true" || s = "True" || s = "TRUE"
let is_false_str_12 s = s = "false" || s = "False" || s = "FALSE"

(** True if [s] is a valid YAML 1.2 JSON-schema decimal integer. The JSON schema
    allows [0] or [[-+]?[1-9][0-9]*] — no leading zeros. *)
let is_decimal_int s =
  let n = String.length s in
  if n = 0 then false
  else begin
    let start = if s.[0] = '+' || s.[0] = '-' then 1 else 0 in
    let rest_len = n - start in
    if rest_len = 0 then false
    else if rest_len = 1 then s.[start] >= '0' && s.[start] <= '9'
    else
      (* Multi-digit: must not start with 0 *)
      s.[start] >= '1'
      && s.[start] <= '9'
      && string_forall_from (start + 1) (fun c -> c >= '0' && c <= '9') s
  end

(** True if [s] is a hex integer ([0x…]). *)
let is_hex_int s =
  let n = String.length s in
  n > 2
  && s.[0] = '0'
  && (s.[1] = 'x' || s.[1] = 'X')
  && string_forall_from 2 Char_class.(fun c -> is_hex_digit (Char.code c)) s

(** True if [s] is a YAML 1.2 octal integer ([0o…]). *)
let is_octal_int_12 s =
  let n = String.length s in
  n > 2
  && s.[0] = '0'
  && (s.[1] = 'o' || s.[1] = 'O')
  && string_forall_from 2 (fun c -> c >= '0' && c <= '7') s

(** Try to parse a float. Returns [None] for non-numeric strings. *)
let try_float_12 s =
  if s = ".inf" || s = ".Inf" || s = ".INF" then Some Float.infinity
  else if s = "+.inf" || s = "+.Inf" || s = "+.INF" then Some Float.infinity
  else if s = "-.inf" || s = "-.Inf" || s = "-.INF" then Some Float.neg_infinity
  else if s = ".nan" || s = ".NaN" || s = ".NAN" then Some Float.nan
  else
    match float_of_string_opt s with
    | Some f
      when String.contains s '.' || String.contains s 'e'
           || String.contains s 'E' ->
        Some f
    | _ -> None

(* ------------------------------------------------------------------ *)
(* YAML 1.1 additional matchers                                          *)
(* ------------------------------------------------------------------ *)

(** YAML 1.1 boolean values not recognised in YAML 1.2. *)
let is_true_str_11_only s =
  s = "y" || s = "Y" || s = "yes" || s = "Yes" || s = "YES" || s = "on"
  || s = "On" || s = "ON"

let is_false_str_11_only s =
  s = "n" || s = "N" || s = "no" || s = "No" || s = "NO" || s = "off"
  || s = "Off" || s = "OFF"

let is_true_str_11 s = is_true_str_12 s || is_true_str_11_only s
let is_false_str_11 s = is_false_str_12 s || is_false_str_11_only s

(** True if [s] is a YAML 1.1 octal integer: leading [0] followed by one or more
    octal digits, with no [o]/[x]/[b] prefix. For example [0755]. *)
let is_octal_int_11 s =
  let n = String.length s in
  if n < 2 then false
  else
    let c1 = s.[1] in
    s.[0] = '0'
    && c1 >= '0' && c1 <= '7'
    && string_forall_from 1 (fun c -> c >= '0' && c <= '7') s

(** Try to parse a YAML 1.1 sexagesimal integer like [3:25:45] (= 12345). The
    first component must be [1–9]; subsequent components are [0–59]. Returns
    [None] if [s] does not match the pattern. *)
let parse_sexagesimal_int s =
  (* Optional leading sign *)
  let sign, s =
    if String.length s > 0 && s.[0] = '-' then
      (-1L, String.sub s 1 (String.length s - 1))
    else if String.length s > 0 && s.[0] = '+' then
      (1L, String.sub s 1 (String.length s - 1))
    else (1L, s)
  in
  let parts = String.split_on_char ':' s in
  match parts with
  | []
  | [ _ ] ->
      None (* need at least two components *)
  | first_s :: rest -> (
      try
        let first = int_of_string first_s in
        (* First component must be 1–9 (per yaml.org/type/int) *)
        if first < 1 then None
        else
          let rest_ns = List.map int_of_string rest in
          (* Each subsequent component must be 0–59 and contain only digits *)
          if List.exists (fun n -> n < 0 || n > 59) rest_ns then None
          else if
            List.exists
              (fun p ->
                String.length p = 0
                || not
                     (String.to_seq p
                     |> Seq.for_all (fun c -> c >= '0' && c <= '9')))
              rest
          then None
          else
            let v =
              List.fold_left
                (fun acc n -> Int64.add (Int64.mul acc 60L) (Int64.of_int n))
                (Int64.of_int first) rest_ns
            in
            Some (Int64.mul sign v)
      with
      | _ -> None)

(** Try to parse a YAML 1.1 sexagesimal float like [20:30.15] (= 1230.15). The
    integer components are base-60; the last component carries the fractional
    part. Returns [None] if [s] does not match. *)
let parse_sexagesimal_float s =
  let sign, s =
    if String.length s > 0 && s.[0] = '-' then
      (-1.0, String.sub s 1 (String.length s - 1))
    else if String.length s > 0 && s.[0] = '+' then
      (1.0, String.sub s 1 (String.length s - 1))
    else (1.0, s)
  in
  let parts = String.split_on_char ':' s in
  match parts with
  | []
  | [ _ ] ->
      None
  | _ -> (
      match List.rev parts with
      | []
      | [ _ ] ->
          None (* unreachable: handled above *)
      | last :: rev_int_parts -> (
          let int_parts = List.rev rev_int_parts in
          (* Last component must contain a '.' to be a float, not a sexagesimal int *)
          if not (String.contains last '.') then None
          else
            try
              let int_ns = List.map int_of_string int_parts in
              let last_f = float_of_string last in
              match int_ns with
              | [] -> None
              | first :: rest ->
                  if first < 1 then None
                  else if List.exists (fun n -> n < 0 || n > 59) rest then None
                  else
                    let int_val =
                      List.fold_left (fun acc n -> (acc * 60) + n) 0 int_ns
                    in
                    Some (sign *. ((float_of_int int_val *. 60.0) +. last_f))
            with
            | _ -> None))

(* ------------------------------------------------------------------ *)
(* Ambiguity detection (reject_ambiguous mode)                          *)
(* ------------------------------------------------------------------ *)

(** Return an error message if plain scalar [s] would resolve differently under
    YAML 1.1 than under YAML 1.2. Returns [None] if unambiguous. *)
let ambiguity_message s =
  if is_true_str_11_only s then
    Some
      (Printf.sprintf
         "%S is a boolean (true) in YAML 1.1 but a string in YAML 1.2" s)
  else if is_false_str_11_only s then
    Some
      (Printf.sprintf
         "%S is a boolean (false) in YAML 1.1 but a string in YAML 1.2" s)
  else if is_octal_int_11 s then
    Some
      (Printf.sprintf
         "%S is an octal integer in YAML 1.1 but a string in YAML 1.2 (use %S \
          for YAML 1.2 octal)"
         s
         ("0o" ^ String.sub s 1 (String.length s - 1)))
  else
    match parse_sexagesimal_int s with
    | Some _ ->
        Some
          (Printf.sprintf
             "%S is a sexagesimal integer in YAML 1.1 but a string in YAML 1.2"
             s)
    | None -> (
        match parse_sexagesimal_float s with
        | Some _ ->
            Some
              (Printf.sprintf
                 "%S is a sexagesimal float in YAML 1.1 but a string in YAML \
                  1.2"
                 s)
        | None -> None)

(* ------------------------------------------------------------------ *)
(* Tag resolution                                                        *)
(* ------------------------------------------------------------------ *)

let yaml_prefix = "tag:yaml.org,2002:"
let null_tag = yaml_prefix ^ "null"
let bool_tag = yaml_prefix ^ "bool"
let int_tag = yaml_prefix ^ "int"
let float_tag = yaml_prefix ^ "float"
let str_tag = yaml_prefix ^ "str"

let resolve_plain_tag_12 s =
  if is_null_str s then null_tag
  else if is_true_str_12 s || is_false_str_12 s then bool_tag
  else if is_decimal_int s || is_hex_int s || is_octal_int_12 s then int_tag
  else
    match try_float_12 s with
    | Some _ -> float_tag
    | None -> str_tag

let resolve_plain_tag_11 s =
  if is_null_str s then null_tag
  else if is_true_str_11 s || is_false_str_11 s then bool_tag
  else if
    is_decimal_int s || is_hex_int s || is_octal_int_12 s || is_octal_int_11 s
    ||
    match parse_sexagesimal_int s with
    | Some _ -> true
    | None -> false
  then int_tag
  else
    match try_float_12 s with
    | Some _ -> float_tag
    | None -> (
        match parse_sexagesimal_float s with
        | Some _ -> float_tag
        | None -> str_tag)

let effective_tag ~schema ~explicit_tag ~style ~value =
  match explicit_tag with
  | Some t -> t
  | None -> (
      match style with
      | Plain -> (
          match schema with
          | Yaml_1_2 -> resolve_plain_tag_12 value
          | Yaml_1_1 -> resolve_plain_tag_11 value)
      | Single_quoted
      | Double_quoted
      | Literal
      | Folded ->
          str_tag)

(* ------------------------------------------------------------------ *)
(* Value construction                                                    *)
(* ------------------------------------------------------------------ *)

let parse_int_12 s =
  if is_hex_int s then
    Int64.of_string ("0x" ^ String.sub s 2 (String.length s - 2))
  else if is_octal_int_12 s then
    Int64.of_string ("0o" ^ String.sub s 2 (String.length s - 2))
  else Int64.of_string s

let parse_int_11 s =
  if is_hex_int s then
    Int64.of_string ("0x" ^ String.sub s 2 (String.length s - 2))
  else if is_octal_int_12 s then
    Int64.of_string ("0o" ^ String.sub s 2 (String.length s - 2))
  else if is_octal_int_11 s then
    Int64.of_string ("0o" ^ String.sub s 1 (String.length s - 1))
  else
    match parse_sexagesimal_int s with
    | Some n -> n
    | None -> Int64.of_string s

let resolve_scalar ~schema ~reject_ambiguous ~(loc : loc)
    ~(explicit_tag : string option) ~(style : scalar_style) ~(value : string) :
    Types.value =
  let tag = effective_tag ~schema ~explicit_tag ~style ~value in
  if tag = null_tag then Null loc
  else if tag = bool_tag then
    let b =
      match schema with
      | Yaml_1_2 -> is_true_str_12 value
      | Yaml_1_1 -> is_true_str_11 value
    in
    Bool (loc, b)
  else if tag = int_tag then
    let parse_int =
      match schema with
      | Yaml_1_2 -> parse_int_12
      | Yaml_1_1 -> parse_int_11
    in
    try Int (loc, parse_int value) with
    | _ -> String (loc, value)
  else if tag = float_tag then
    let f =
      match try_float_12 value with
      | Some f -> Some f
      | None -> (
          match schema with
          | Yaml_1_2 -> None
          | Yaml_1_1 -> parse_sexagesimal_float value)
    in
    match f with
    | Some f -> Float (loc, f)
    | None -> String (loc, value)
  else if tag = str_tag then (
    (* In 1.2 + reject_ambiguous mode, flag scalars that 1.1 would read differently *)
    (if reject_ambiguous && style = Plain && explicit_tag = None then
       match ambiguity_message value with
       | Some msg -> raise (Types.Error (Types.Schema_error { msg; loc }))
       | None -> ());
    String (loc, value))
  else String (loc, value)

(* ------------------------------------------------------------------ *)
(* Schema conflict detection                                             *)
(* ------------------------------------------------------------------ *)

(** Determine the effective schema for a document. [requested] is what the
    caller specified; [doc_version] is the document's [%YAML] directive; [loc]
    is the source range of the document content (used in error messages). Raises
    [Schema_error] when [strict_schema = true] and they conflict. *)
let effective_schema ~strict_schema ~(requested : schema) ~(loc : Types.loc)
    (doc_version : (int * int) option) : schema =
  match doc_version with
  | None -> requested
  | Some (major, minor) ->
      let doc_schema =
        if major = 1 && minor = 1 then Yaml_1_1
        else if major = 1 && minor >= 2 then Yaml_1_2
        else requested (* unknown version: fall back to requested *)
      in
      if strict_schema && doc_schema <> requested then
        raise
          (Types.Error
             (Types.Schema_error
                {
                  msg =
                    Printf.sprintf
                      "document declares %%YAML %d.%d but the requested schema \
                       is %s"
                      major minor
                      (match requested with
                      | Yaml_1_2 -> "YAML 1.2"
                      | Yaml_1_1 -> "YAML 1.1");
                  loc;
                }))
      else doc_schema

(* ------------------------------------------------------------------ *)
(* Merge key expansion (YAML 1.1)                                       *)
(* ------------------------------------------------------------------ *)

(** True when [node] is the plain scalar [<<] (or tagged [!!merge]), i.e. a YAML
    1.1 merge key. *)
let is_merge_key_node = function
  | Scalar_node { value = "<<"; style = Plain; tag = None; _ } -> true
  | Scalar_node { tag = Some t; _ } when t = yaml_prefix ^ "merge" -> true
  | _ -> false

(** Expand a resolved merge value into a list of [(loc, key, value)] triples. A
    merge value must be a [Map] (or a [Seq] of [Map]s); anything else is
    silently ignored. *)
let expand_merge_value = function
  | Map (_, pairs) -> pairs
  | Seq (_, items) ->
      List.concat_map
        (function
          | Map (_, pairs) -> pairs
          | _ -> [])
        items
  | _ -> []

(* ------------------------------------------------------------------ *)
(* Key deduplication                                                     *)
(* ------------------------------------------------------------------ *)

(** Keep only the last occurrence of each key in a resolved pair list,
    preserving the relative order of surviving entries. Returns the surviving
    pairs together with the set of their keys.

    Implemented by reversing, keeping the first occurrence of each key (= last
    in the original), then reversing back. *)
let dedup_keep_last pairs =
  let rev = List.rev pairs in
  let seen = ref Types.Value_set.empty in
  let deduped =
    List.filter
      (fun (_, k, _) ->
        if Types.Value_set.mem k !seen then false
        else (
          seen := Types.Value_set.add k !seen;
          true))
      rev
  in
  (List.rev deduped, !seen)

(** Raise [Duplicate_key_error] if any key appears more than once. The error
    location points to the second (duplicate) key-value pair. Returns the set of
    keys on success. *)
let check_unique_keys pairs =
  List.fold_left
    (fun seen (pair_loc, k, _) ->
      if Types.Value_set.mem k seen then
        raise
          (Types.Error
             (Types.Duplicate_key_error
                { msg = "duplicate mapping key"; loc = pair_loc }))
      else Types.Value_set.add k seen)
    Types.Value_set.empty pairs

(** Filter [pairs] to those whose key is not already in [known_keys]. Used to
    suppress merged keys that duplicate an explicit regular key. *)
let keep_new_keys known_keys pairs =
  List.filter (fun (_, k, _) -> not (Types.Value_set.mem k known_keys)) pairs

(* ------------------------------------------------------------------ *)
(* Full node resolution                                                  *)
(* ------------------------------------------------------------------ *)

let tick ~limit ~counter =
  incr counter;
  if !counter > limit then
    raise (Types.Error (Types.Expansion_limit_exceeded limit))

let node_loc : Types.node -> Types.loc = function
  | Scalar_node r -> r.loc
  | Sequence_node r -> r.loc
  | Mapping_node r -> r.loc
  | Alias_node r -> r.loc

(** Raise [Simplicity_error] if [plain] is [true] and [node] uses an anchor, is
    an alias, or has an explicit tag. *)
let check_simple ~plain (node : Types.node) =
  if plain then begin
    (* Check anchor *)
    (let anchor_opt =
       match node with
       | Scalar_node { anchor; _ } -> anchor
       | Sequence_node { anchor; _ } -> anchor
       | Mapping_node { anchor; _ } -> anchor
       | Alias_node _ -> None
     in
     match anchor_opt with
     | Some name ->
         let loc = node_loc node in
         raise
           (Types.Error
              (Types.Simplicity_error
                 {
                   msg =
                     Printf.sprintf "anchor '&%s' is not allowed in plain mode"
                       name;
                   loc;
                 }))
     | None -> ());
    (* Check alias *)
    (match node with
    | Alias_node { name; loc; _ } ->
        raise
          (Types.Error
             (Types.Simplicity_error
                {
                  msg =
                    Printf.sprintf "alias '*%s' is not allowed in plain mode"
                      name;
                  loc;
                }))
    | _ -> ());
    (* Check explicit tag *)
    let tag_opt =
      match node with
      | Scalar_node { tag; _ } -> tag
      | Sequence_node { tag; _ } -> tag
      | Mapping_node { tag; _ } -> tag
      | Alias_node _ -> None
    in
    match tag_opt with
    | Some t ->
        let loc = node_loc node in
        raise
          (Types.Error
             (Types.Simplicity_error
                {
                  msg = Printf.sprintf "tag '%s' is not allowed in plain mode" t;
                  loc;
                }))
    | None -> ()
  end

let rec resolve_node ~schema ~reject_ambiguous ~plain ~strict_keys ~limit
    ~counter ~visiting (node : Types.node) : Types.value =
  tick ~limit ~counter;
  check_simple ~plain node;
  visiting := node :: !visiting;
  let result =
    match node with
    | Scalar_node { tag; value; style; loc; _ } ->
        resolve_scalar ~schema ~reject_ambiguous ~loc ~explicit_tag:tag ~style
          ~value
    | Sequence_node { items; loc; _ } ->
        Seq
          ( loc,
            List_ext.map
              (resolve_node ~schema ~reject_ambiguous ~plain ~strict_keys ~limit
                 ~counter ~visiting)
              items )
    | Mapping_node { pairs; loc; _ } -> (
        let resolve =
          resolve_node ~schema ~reject_ambiguous ~plain ~strict_keys ~limit
            ~counter ~visiting
        in
        match schema with
        | Yaml_1_2 ->
            (* In 1.2 mode, check for merge keys when reject_ambiguous is set *)
            let pairs_resolved =
              List_ext.map
                (fun (k, v) ->
                  let pair_loc =
                    {
                      start_pos = (node_loc k).start_pos;
                      end_pos = (node_loc v).end_pos;
                    }
                  in
                  let k' = resolve k in
                  (* Flag <<  as ambiguous if requested *)
                  if
                    reject_ambiguous
                    &&
                    match k with
                    | Scalar_node { value = "<<"; style = Plain; tag = None; _ }
                      ->
                        true
                    | _ -> false
                  then
                    raise
                      (Types.Error
                         (Types.Schema_error
                            {
                              msg =
                                "mapping key \"<<\" is a merge key in YAML 1.1 \
                                 but a plain string in YAML 1.2";
                              loc = node_loc k;
                            }))
                  else (pair_loc, k', resolve v))
                pairs
            in
            let pairs_final, _ =
              if strict_keys then
                (pairs_resolved, check_unique_keys pairs_resolved)
              else dedup_keep_last pairs_resolved
            in
            Map (loc, pairs_final)
        | Yaml_1_1 ->
            (* Separate merge-key pairs from regular pairs *)
            let regular =
              List.filter (fun (k, _) -> not (is_merge_key_node k)) pairs
            in
            let merge_nodes =
              List.filter_map
                (fun (k, v) -> if is_merge_key_node k then Some v else None)
                pairs
            in
            (* In plain mode, merge keys are not allowed *)
            (match List.find_opt (fun (k, _) -> is_merge_key_node k) pairs with
            | Some (merge_k, _) when plain ->
                raise
                  (Types.Error
                     (Types.Simplicity_error
                        {
                          msg = "merge key '<<' is not allowed in plain mode";
                          loc = node_loc merge_k;
                        }))
            | _ -> ());
            (* Resolve regular pairs *)
            let reg_resolved =
              List_ext.map
                (fun (k, v) ->
                  let pair_loc =
                    {
                      start_pos = (node_loc k).start_pos;
                      end_pos = (node_loc v).end_pos;
                    }
                  in
                  (pair_loc, resolve k, resolve v))
                regular
            in
            (* Resolve merge values and expand *)
            let merge_expanded =
              List.concat_map
                (fun mv -> expand_merge_value (resolve mv))
                merge_nodes
            in
            (* Check/deduplicate regular pairs; keep merged keys not already
               present among the regular keys *)
            let reg_final, reg_keys =
              if strict_keys then (reg_resolved, check_unique_keys reg_resolved)
              else dedup_keep_last reg_resolved
            in
            let extra = keep_new_keys reg_keys merge_expanded in
            Map (loc, reg_final @ extra))
    | Alias_node { resolved; loc; _ } ->
        let target = Lazy.force resolved in
        if List.memq target !visiting then
          raise
            (Types.Error
               (Types.Cycle_error
                  { msg = "cyclic alias cannot be represented as a value"; loc }))
        else
          resolve_node ~schema ~reject_ambiguous ~plain ~strict_keys ~limit
            ~counter ~visiting target
  in
  (match !visiting with
  | _ :: rest -> visiting := rest
  | [] -> ());
  result

let resolve_documents ?(expansion_limit = Types.default_expansion_limit)
    ?(schema = Yaml_1_2) ?(strict_schema = false) ?(reject_ambiguous = false)
    ?(plain = false) ?(strict_keys = false)
    (versioned_nodes : ((int * int) option * Types.node) list) :
    Types.value list =
  let counter = ref 0 in
  List_ext.map
    (fun (doc_version, node) ->
      let eff_schema =
        effective_schema ~strict_schema ~requested:schema ~loc:(node_loc node)
          doc_version
      in
      let visiting = ref [] in
      resolve_node ~schema:eff_schema ~reject_ambiguous ~plain ~strict_keys
        ~limit:expansion_limit ~counter ~visiting node)
    versioned_nodes
