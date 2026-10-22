open Base.Result.Let_syntax

type error = Syntax of int | Number_too_big of int | Too_deep | Key_too_long [@@deriving sexp_of ~portable]

(** A parsed value, as the server's DOM holds it. *)
type value =
  | Literal of int  (** 0 null, 1 true, 2 false. *)
  | Int of int64  (** RapidJSON's [Int], [Uint] and [Int64]. *)
  | Uint of int64  (** RapidJSON's [Uint64]: unsigned past [UINT32_MAX]. *)
  | Double of float
  | String of string
  | Array of value list
  | Object of (string * value) list  (** Keys by length, then bytes. *)

let fits16 v = Int64.compare v (-32768L) >= 0 && Int64.compare v 32767L <= 0
let fits32 v = Int64.compare v (-2147483648L) >= 0 && Int64.compare v 2147483647L <= 0
let ufits16 v = Int64.unsigned_compare v 65535L <= 0
let ufits32 v = Int64.unsigned_compare v 4294967295L <= 0

let inlined value ~large =
  match value with
  | Literal _ -> true
  | Int v -> fits16 v || (large && fits32 v)
  | Uint v -> ufits16 v || (large && ufits32 v)
  | Double _ | String _ | Array _ | Object _ -> false

let rec size value ~large =
  let o = if large then 4 else 2 in
  match value with
  | Literal _ -> 1
  | Int v -> if fits16 v then 2 else if fits32 v then 4 else 8
  | Uint v -> if ufits16 v then 2 else if ufits32 v then 4 else 8
  | Double _ -> 8
  | String s ->
      let rec varlen n = if n < 0x80 then 1 else 1 + varlen (n lsr 7) in
      varlen (String.length s) + String.length s
  | Array items ->
      (2 * o) + (List.length items * (1 + o)) + Base.List.sum (module Base.Int) items ~f:(fun item -> child item ~large)
  | Object members ->
      (2 * o)
      + (List.length members * ((2 + o) + (1 + o)))
      + Base.List.sum (module Base.Int) members ~f:(fun (key, item) -> String.length key + child item ~large)

and child value ~large = if inlined value ~large then 0 else size value ~large:(wide value ~parent:large)
and wide value ~parent = parent && size value ~large:false > 0xFFFF

let binary bytes ~pos ~length =
  let%bind root =
      let stop = pos + length in
      let peek at = if at < stop then Bytes.get bytes at else '\000' in
      let digit at = peek at >= '0' && peek at <= '9' in
      let syntax at = Error (`Json (Syntax (at - pos))) in
      let rec space at = match peek at with ' ' | '\n' | '\r' | '\t' -> space (at + 1) | _ -> at in
      let word at text value =
        let n = String.length text in
        if at + n <= stop && Bytes.sub_string bytes at n = text then Ok (value, at + n) else syntax at
      in
      let hex4 at =
        let rec go k acc =
          if k = 4 then Ok acc
          else
            match peek (at + k) with
            | '0' .. '9' as c -> go (k + 1) ((acc * 16) + Char.code c - 48)
            | 'a' .. 'f' as c -> go (k + 1) ((acc * 16) + Char.code c - 87)
            | 'A' .. 'F' as c -> go (k + 1) ((acc * 16) + Char.code c - 55)
            | _ -> syntax (at + k)
        in
        go 0 0
      in
      let utf8 buffer point =
        let add n = Buffer.add_char buffer (Char.chr n) in
        if point < 0x80 then add point
        else if point < 0x800 then (
          add (0xC0 lor (point lsr 6));
          add (0x80 lor (point land 0x3F)))
        else if point < 0x10000 then (
          add (0xE0 lor (point lsr 12));
          add (0x80 lor ((point lsr 6) land 0x3F));
          add (0x80 lor (point land 0x3F)))
        else (
          add (0xF0 lor (point lsr 18));
          add (0x80 lor ((point lsr 12) land 0x3F));
          add (0x80 lor ((point lsr 6) land 0x3F));
          add (0x80 lor (point land 0x3F)))
      in
      let text at =
        let buffer = Buffer.create 16 in
        let rec go at =
          match peek at with
          | '"' when at < stop -> Ok (Buffer.contents buffer, at + 1)
          | '\\' -> (
              let escaped c =
                Buffer.add_char buffer c;
                go (at + 2)
              in
              match peek (at + 1) with
              | '"' -> escaped '"'
              | '\\' -> escaped '\\'
              | '/' -> escaped '/'
              | 'b' -> escaped '\b'
              | 'f' -> escaped '\012'
              | 'n' -> escaped '\n'
              | 'r' -> escaped '\r'
              | 't' -> escaped '\t'
              | 'u' -> (
                  let%bind point = hex4 (at + 2) in
                  match point with
                  | high when high >= 0xD800 && high <= 0xDBFF -> (
                      if peek (at + 6) <> '\\' || peek (at + 7) <> 'u' then syntax at
                      else
                        let%bind low = hex4 (at + 8) in
                        match low with
                        | low when low >= 0xDC00 && low <= 0xDFFF ->
                            utf8 buffer ((((high - 0xD800) lsl 10) lor (low - 0xDC00)) + 0x10000);
                            go (at + 12)
                        | _ -> syntax at)
                  | low when low >= 0xDC00 && low <= 0xDFFF -> syntax at
                  | point ->
                      utf8 buffer point;
                      go (at + 6))
              | _ -> syntax at)
          | c when Char.code c < 0x20 -> syntax at
          | c ->
              Buffer.add_char buffer c;
              go (at + 1)
        in
        go at
      in
      let number start =
        let minus = peek start = '-' in
        let first = if minus then start + 1 else start in
        let rec digits at = if digit at then digits (at + 1) else at in
        let%bind integral = if peek first = '0' then Ok (first + 1) else if digit first then Ok (digits first) else syntax first in
        let%bind fraction =
          if peek integral <> '.' then Ok integral else if digit (integral + 1) then Ok (digits (integral + 1)) else syntax (integral + 1)
        in
        let%bind last =
          if peek fraction <> 'e' && peek fraction <> 'E' then Ok fraction
          else
            let sign = fraction + 1 in
            let exponent = if peek sign = '+' || peek sign = '-' then sign + 1 else sign in
            if digit exponent then Ok (digits exponent) else syntax exponent
        in
        let floating () =
          match float_of_string_opt (Bytes.sub_string bytes start (last - start)) with
          | Some d when Float.is_finite d -> Ok (Double d, last)
          | Some _ | None -> Error (`Json (Number_too_big (start - pos)))
        in
        let magnitude = Bytes.sub_string bytes first (integral - first) in
        let within limit =
          String.length magnitude < String.length limit
          || (String.length magnitude = String.length limit && String.compare magnitude limit <= 0)
        in
        let unsigned = String.fold_left (fun acc c -> Int64.add (Int64.mul acc 10L) (Int64.of_int (Char.code c - 48))) 0L in
        if last > integral then floating ()
        else if minus && within "9223372036854775808" then Ok (Int (Int64.neg (unsigned magnitude)), last)
        else if minus then floating ()
        else if within "4294967295" then Ok (Int (unsigned magnitude), last)
        else if within "18446744073709551615" then Ok (Uint (unsigned magnitude), last)
        else floating ()
      in
      let rec value at depth =
        match peek at with
        | 'n' -> word at "null" (Literal 0)
        | 't' -> word at "true" (Literal 1)
        | 'f' -> word at "false" (Literal 2)
        | '"' ->
            let%map s, at = text (at + 1) in
            (String s, at)
        | '[' ->
            if depth >= 100 then Error (`Json Too_deep)
            else
              let at = space (at + 1) in
              if peek at = ']' then Ok (Array [], at + 1)
              else
                let rec items at acc =
                  let%bind item, at = value (space at) (depth + 1) in
                  let at = space at in
                  match peek at with
                  | ',' -> items (at + 1) (item :: acc)
                  | ']' -> Ok (Array (List.rev (item :: acc)), at + 1)
                  | _ -> syntax at
                in
                items at []
        | '{' ->
            if depth >= 100 then Error (`Json Too_deep)
            else
              let at = space (at + 1) in
              let ordered members =
                let sorted =
                  List.stable_sort
                    (fun (a, _) (b, _) ->
                      match Int.compare (String.length a) (String.length b) with 0 -> String.compare a b | order -> order)
                    members
                in
                let rec unique = function
                  | ((a, _) as first) :: (b, _) :: rest when String.equal a b -> unique (first :: rest)
                  | first :: rest -> first :: unique rest
                  | [] -> []
                in
                Object (unique sorted)
              in
              if peek at = '}' then Ok (Object [], at + 1)
              else
                let rec members at acc =
                  if peek at <> '"' then syntax at
                  else
                    let%bind key, at = text (at + 1) in
                    let at = space at in
                    if peek at <> ':' then syntax at
                    else
                      let%bind item, at = value (space (at + 1)) (depth + 1) in
                      let at = space at in
                      match peek at with
                      | ',' -> members (space (at + 1)) ((key, item) :: acc)
                      | '}' -> Ok (ordered ((key, item) :: acc), at + 1)
                      | _ -> syntax at
                in
                members at []
        | _ -> number at
      in
      let%bind root, at = value (space pos) 0 in
      if space at = stop then Ok root else syntax (space at)
  in
  let rec keys = function
    | Object members ->
        List.exists (fun (key, item) -> String.length key > 0xFFFF || keys item) members
    | Array items -> List.exists keys items
    | Literal _ | Int _ | Uint _ | Double _ | String _ -> false
  in
  if keys root then Error (`Json Key_too_long)
  else
    let large = size root ~large:false > 0xFFFF in
    let out = Bytes.create (1 + size root ~large) in
    let kind value ~large =
      match value with
      | Object _ -> if large then 1 else 0
      | Array _ -> if large then 3 else 2
      | Literal _ -> 4
      | Int v -> if fits16 v then 5 else if fits32 v then 7 else 9
      | Uint v -> if ufits16 v then 6 else if ufits32 v then 8 else 10
      | Double _ -> 11
      | String _ -> 12
    in
    let offset at n ~large = if large then Bytes.set_int32_le out at (Int32.of_int n) else Bytes.set_uint16_le out at (n land 0xFFFF) in
    let rec write at value ~large =
      match value with
      | Literal k ->
          Bytes.set_uint8 out at k;
          at + 1
      | Int v | Uint v -> (
          match kind value ~large with
          | 5 | 6 ->
              Bytes.set_uint16_le out at (Int64.to_int v land 0xFFFF);
              at + 2
          | 7 | 8 ->
              Bytes.set_int32_le out at (Int64.to_int32 v);
              at + 4
          | _ ->
              Bytes.set_int64_le out at v;
              at + 8)
      | Double f ->
          Bytes.set_int64_le out at (Int64.bits_of_float f);
          at + 8
      | String s ->
          let rec length at n =
            if n < 0x80 then (
              Bytes.set_uint8 out at n;
              at + 1)
            else (
              Bytes.set_uint8 out at (0x80 lor (n land 0x7F));
              length (at + 1) (n lsr 7))
          in
          let at = length at (String.length s) in
          Bytes.blit_string s 0 out at (String.length s);
          at + String.length s
      | Array items -> container at ~large (List.map (fun item -> ("", item)) items) ~keyed:false
      | Object members -> container at ~large members ~keyed:true
    and container start ~large members ~keyed =
      let o = if large then 4 else 2 in
      let n = List.length members in
      offset start n ~large;
      let named = if keyed then members else [] in
      let entries = start + (2 * o) + (List.length named * (2 + o)) in
      let names = entries + (n * (1 + o)) in
      let (_ : int * int) =
        List.fold_left
          (fun (at, name) (key, _) ->
            offset at (name - start) ~large;
            Bytes.set_uint16_le out (at + o) (String.length key);
            Bytes.blit_string key 0 out name (String.length key);
            (at + 2 + o, name + String.length key))
          (start + (2 * o), names)
          named
      in
      let data = names + Base.List.sum (module Base.Int) named ~f:(fun (key, _) -> String.length key) in
      let after, _ =
        List.fold_left
          (fun (cursor, entry) (_, item) ->
            if inlined item ~large then (
              Bytes.set_uint8 out entry (kind item ~large);
              let v = match item with Literal k -> k | Int v | Uint v -> Int64.to_int v | Double _ | String _ | Array _ | Object _ -> 0 in
              offset (entry + 1) v ~large;
              (cursor, entry + 1 + o))
            else
              let child = wide item ~parent:large in
              Bytes.set_uint8 out entry (kind item ~large:child);
              offset (entry + 1) (cursor - start) ~large;
              (write cursor item ~large:child, entry + 1 + o))
          (data, entries) members
      in
      offset (start + o) (after - start) ~large;
      after
    in
    Bytes.set_uint8 out 0 (kind root ~large);
    let (_ : int) = write 1 root ~large in
    Ok (Bytes.unsafe_to_string out)
