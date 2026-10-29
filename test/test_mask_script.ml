open Quiver_core
open Sexplib0.Sexp_conv

let key = Keyed.of_secret "script secret"

(** An expression of the masking language, as the tests build it. *)
type e =
  | Lit_text of string
  | Lit_int of int64
  | Lit_bool of bool
  | Lit_null
  | Value
  | Row of string
  | Var of string
  | If of e * e * e
  | Let of string * e * e
  | Call of string * e list
[@@deriving sexp_of]

(** A value as the reference interpreter knows it. *)
type v = Null | Text of string | Int of int64 | Bool of bool | Date of int | Datetime of int64 [@@deriving sexp_of]

let civil days = Option.map Ptime.to_date (Ptime.of_span (Ptime.Span.v (days, 0L)))
let days_of ymd = Option.map (fun t -> fst (Ptime.Span.to_d_ps (Ptime.to_span t))) (Ptime.of_date ymd)
let per_day = 86_400_000_000L

let floor_by a b =
  let q = Int64.div a b in
  if Int64.rem a b < 0L then Int64.pred q else q

let date_shown days =
  Option.fold (civil days) ~none:"?" ~some:(fun (y, m, d) -> Printf.sprintf "%04d-%02d-%02d" y m d)

let date_read text =
  match String.split_on_char '-' text with
  | [ y; m; d ]
    when String.length text = 10
         && List.for_all (fun part -> part <> "" && String.for_all (fun c -> c >= '0' && c <= '9') part) [ y; m; d ] ->
      Option.fold (days_of (int_of_string y, int_of_string m, int_of_string d)) ~none:Null ~some:(fun days -> Date days)
  | _ -> Null

let datetime_read text =
  let text = String.trim text in
  let date, clock = match String.index_opt text ' ' with Some at -> (String.sub text 0 at, String.sub text (at + 1) (String.length text - at - 1)) | None -> (text, "00:00:00") in
  let whole, fraction = match String.index_opt clock '.' with Some at -> (String.sub clock 0 at, String.sub clock (at + 1) (String.length clock - at - 1)) | None -> (clock, "") in
  let digits part = part <> "" && String.for_all (fun c -> c >= '0' && c <= '9') part in
  match (date_read date, String.split_on_char ':' whole) with
  | Date days, [ h; m; sec ]
    when String.length whole = 8 && digits h && digits m && digits sec
         && (fraction = "" || (String.length fraction <= 6 && digits fraction)) ->
      let h = int_of_string h and m = int_of_string m and sec = int_of_string sec in
      if h < 24 && m < 60 && sec < 60 then
        let micros = if fraction = "" then 0 else int_of_string (fraction ^ String.make (6 - String.length fraction) '0') in
        Datetime (Int64.add (Int64.mul (Int64.of_int days) per_day) (Int64.of_int ((((h * 60) + m) * 60 + sec) * 1_000_000 + micros)))
      else Null
  | _ -> Null

let datetime_shown micros =
  let day = floor_by micros per_day in
  let within = Int64.to_int (Int64.sub micros (Int64.mul day per_day)) in
  let seconds = within / 1_000_000 and fraction = within mod 1_000_000 in
  Printf.sprintf "%s %02d:%02d:%02d%s" (date_shown (Int64.to_int day)) (seconds / 3600) (seconds / 60 mod 60) (seconds mod 60)
    (if fraction = 0 then "" else Printf.sprintf ".%06d" fraction)

let in_years days = match civil days with Some (y, _, _) when y >= 0 && y <= 9999 -> Date days | _ -> Null

let quoted text =
  let out = Buffer.create (String.length text + 2) in
  Buffer.add_char out '"';
  String.iter
    (function
      | '"' -> Buffer.add_string out "\\\""
      | '\\' -> Buffer.add_string out "\\\\"
      | '\n' -> Buffer.add_string out "\\n"
      | '\t' -> Buffer.add_string out "\\t"
      | c -> Buffer.add_char out c)
    text;
  Buffer.add_char out '"';
  Buffer.contents out

let rec printed = function
  | Lit_text text -> quoted text
  | Lit_int n -> Int64.to_string n
  | Lit_bool truth -> Bool.to_string truth
  | Lit_null -> "null"
  | Value -> "value"
  | Row column -> "(row " ^ column ^ ")"
  | Var name -> name
  | If (c, a, b) -> String.concat " " [ "(if"; printed c; printed a; printed b ^ ")" ]
  | Let (name, bound, body) -> Printf.sprintf "(let ((%s %s)) %s)" name (printed bound) (printed body)
  | Call (name, arguments) -> "(" ^ String.concat " " (name :: List.map printed arguments) ^ ")"

let points text =
  let n = String.length text in
  let byte i = Char.code text.[i] in
  let continues i = i < n && byte i land 0xC0 = 0x80 in
  let rec go i acc =
    if i >= n then List.rev acc
    else
      let lead = byte i in
      let width =
        if lead < 0x80 then 1
        else if lead land 0xE0 = 0xC0 && continues (i + 1) then 2
        else if lead land 0xF0 = 0xE0 && continues (i + 1) && continues (i + 2) then 3
        else if lead land 0xF8 = 0xF0 && continues (i + 1) && continues (i + 2) && continues (i + 3) then 4
        else 1
      in
      go (i + width) (String.sub text i width :: acc)
  in
  go 0 []

let cased case text =
  String.concat ""
    (List.map
       (fun point ->
         match String.get_utf_8_uchar point 0 with
         | decoded when Uchar.utf_decode_is_valid decoded && Uchar.utf_decode_length decoded = String.length point ->
             let code = Uchar.to_int (Uchar.utf_decode_uchar decoded) in
             let mapped =
               match case with
               | `Upper when (code >= 97 && code <= 122) || (code >= 0x430 && code <= 0x44F) -> code - 32
               | `Upper when code = 0x451 -> 0x401
               | `Lower when (code >= 65 && code <= 90) || (code >= 0x410 && code <= 0x42F) -> code + 32
               | `Lower when code = 0x401 -> 0x451
               | `Upper | `Lower -> code
             in
             let out = Buffer.create 4 in
             Buffer.add_utf_8_uchar out (Uchar.of_int mapped);
             Buffer.contents out
         | _ -> point)
       (points text))

let occurrences text pattern =
  let n = String.length text and m = String.length pattern in
  List.filter (fun i -> String.equal (String.sub text i m) pattern) (List.init (Int.max 0 (n - m + 1)) Fun.id)

let replaced text pattern with_ =
  let m = String.length pattern in
  let rec go i acc =
    if i > String.length text - m then List.rev (String.sub text i (String.length text - i) :: acc)
    else if String.equal (String.sub text i m) pattern then go (i + m) (with_ :: acc)
    else go (i + 1) (String.make 1 text.[i] :: acc)
  in
  if m = 0 then text else String.concat "" (go 0 [])

let clamp n ~most = Int64.to_int (Int64.max 0L (Int64.min n (Int64.of_int most)))

let rec eval ~value ~row ~env e =
  let go = eval ~value ~row ~env in
  let all = List.map go in
  let text_args args = Base.Option.all (List.map (function Text t -> Some t | _ -> None) args) in
  let int_args args = Base.Option.all (List.map (function Int n -> Some n | _ -> None) args) in
  match e with
  | Lit_text t -> Text t
  | Lit_int n -> Int n
  | Lit_bool b -> Bool b
  | Lit_null -> Null
  | Value -> value
  | Row column -> List.assoc column row
  | Var name -> List.assoc name env
  | If (c, a, b) -> ( match go c with Bool true -> go a | _ -> go b)
  | Let (name, bound, body) -> eval ~value ~row ~env:((name, go bound) :: env) body
  | Call (name, arguments) -> (
      let args = all arguments in
      let texts f = Option.fold (text_args args) ~none:Null ~some:f in
      let ints f = Option.fold (int_args args) ~none:Null ~some:f in
      match (name, args) with
      | "concat", _ -> texts (fun ts -> Text (String.concat "" ts))
      | "length", _ -> texts (fun ts -> Int (Int64.of_int (List.length (points (List.hd ts)))))
      | "substring", [ Text t; Int start; Int count ] ->
          let start = clamp start ~most:max_int and count = clamp count ~most:max_int in
          Text (String.concat "" (List.filteri (fun i _ -> i >= start && i - start < count) (points t)))
      | "left", [ Text t; Int n ] -> Text (String.concat "" (Base.List.take (points t) (clamp n ~most:max_int)))
      | "right", [ Text t; Int n ] ->
          let p = points t in
          Text (String.concat "" (Base.List.drop p (List.length p - clamp n ~most:(List.length p))))
      | "upper", [ Text t ] -> Text (cased `Upper t)
      | "lower", [ Text t ] -> Text (cased `Lower t)
      | "trim", [ Text t ] -> Text (String.trim t)
      | "replace", [ Text t; Text from; Text to_ ] -> Text (replaced t from to_)
      | ("before" | "after" | "before-last" | "after-last"), [ Text t; Text sep ] -> (
          let found = if String.equal sep "" then [] else occurrences t sep in
          let at = match name with "before" | "after" -> Base.List.hd found | _ -> Base.List.last found in
          match (name, at) with
          | ("before" | "before-last"), None -> Text t
          | _, None -> Text ""
          | ("before" | "before-last"), Some at -> Text (String.sub t 0 at)
          | _, Some at -> Text (String.sub t (at + String.length sep) (String.length t - at - String.length sep)))
      | ("pad-left" | "pad-right"), [ Text t; Int width; Text fill ] ->
          let missing = clamp width ~most:65535 - List.length (points t) in
          if missing <= 0 || String.equal fill "" then Text t
          else
            let padding = String.concat "" (List.init missing (fun _ -> List.hd (points fill))) in
            let padded = if String.equal name "pad-left" then padding ^ t else t ^ padding in
            Text padded
      | "str", [ Int n ] -> Text (Int64.to_string n)
      | "int", [ Text t ] ->
          let t = String.trim t in
          let digits = Base.String.chop_prefix_if_exists t ~prefix:"-" in
          if digits <> "" && String.for_all (fun c -> c >= '0' && c <= '9') digits then
            Option.fold (Int64.of_string_opt t) ~none:Null ~some:(fun n -> Int n)
          else Null
      | "starts-with", [ Text t; Text p ] -> Bool (String.starts_with ~prefix:p t)
      | "ends-with", [ Text t; Text p ] -> Bool (String.ends_with ~suffix:p t)
      | "contains", [ Text t; Text p ] -> Bool (String.equal p "" || occurrences t p <> [])
      | "shape", [ Text t ] -> Text (Keyed.shape key t)
      | "email", [ Text t ] -> Text (Keyed.email key t)
      | "hash", [ Text t ] -> Text (Keyed.hash_text key t)
      | "hash-bytes", [ Text t ] -> Text (Keyed.hash_bytes key t)
      | "hash-int", [ Int n ] -> Int (Keyed.hash_int64 key n)
      | "keyed-int", [ Text t; Int low; Int high ] -> Int (Keyed.within key t ~low ~high)
      | "digits", [ Text t; Int n ] -> Text (Keyed.digits key t (clamp n ~most:65535))
      | "pick", Text seed :: _ -> go (List.nth (List.tl arguments) (Keyed.choice key seed (List.length arguments - 1)))
      | "pick", _ -> Null
      | "+", _ -> ints (fun ns -> Int (List.fold_left Int64.add 0L ns))
      | "*", _ -> ints (fun ns -> Int (List.fold_left Int64.mul 1L ns))
      | "-", [ Int n ] -> Int (Int64.neg n)
      | "-", [ Int a; Int b ] -> Int (Int64.sub a b)
      | "/", [ Int _; Int 0L ] | "mod", [ Int _; Int 0L ] -> Null
      | "/", [ Int a; Int b ] -> Int (Int64.div a b)
      | "mod", [ Int a; Int b ] -> Int (Int64.rem a b)
      | "abs", [ Int n ] -> Int (Int64.abs n)
      | ("min" | "max"), _ when List.mem Null args -> Null
      | "min", first :: rest -> List.fold_left (fun best next -> if compare next best < 0 then next else best) first rest
      | "max", first :: rest -> List.fold_left (fun best next -> if compare next best > 0 then next else best) first rest
      | ("=" | "<>" | "<" | "<=" | ">" | ">="), [ a; b ] -> (
          let order =
            match (a, b) with
            | Text x, Text y -> Some (String.compare x y)
            | Int x, Int y | Datetime x, Datetime y -> Some (Int64.compare x y)
            | Bool x, Bool y -> Some (Bool.compare x y)
            | Date x, Date y -> Some (Int.compare x y)
            | _ -> None
          in
          match order with
          | None -> Null
          | Some order ->
              let order = Int.compare order 0 in
              let holds =
                match name with
                | "=" -> order = 0
                | "<>" -> order <> 0
                | "<" -> order < 0
                | "<=" -> order <= 0
                | ">" -> order > 0
                | _ -> order >= 0
              in
              Bool holds)
      | "and", _ -> Bool (List.for_all (( = ) (Bool true)) args)
      | "or", _ -> Bool (List.exists (( = ) (Bool true)) args)
      | "not", [ Bool b ] -> Bool (not b)
      | "is-null", [ a ] -> Bool (a = Null)
      | "coalesce", _ -> Option.value (List.find_opt (( <> ) Null) args) ~default:Null
      | "split-part", [ Text t; Text sep; Int n ] ->
          let rec split text acc =
            match if sep = "" then [] else occurrences text sep with
            | [] -> List.rev (text :: acc)
            | at :: _ ->
                let rest = at + String.length sep in
                split (String.sub text rest (String.length text - rest)) (String.sub text 0 at :: acc)
          in
          let parts = split t [] in
          Text (if n < 1L || n > Int64.of_int (List.length parts) then "" else List.nth parts (Int64.to_int n - 1))
      | "index-of", [ Text t; Text sub ] ->
          if sub = "" then Int 0L
          else (
            match occurrences t sub with
            | [] -> Int (-1L)
            | at :: _ -> Int (Int64.of_int (List.length (points (String.sub t 0 at)))))
      | "repeat", [ Text t; Int n ] ->
          let n = clamp n ~most:65535 in
          Text (if String.length t * n > 1_048_576 then "" else String.concat "" (List.init n (fun _ -> t)))
      | "reverse", [ Text t ] -> Text (String.concat "" (List.rev (points t)))
      | "title", [ Text t ] ->
          let code point =
            let decoded = String.get_utf_8_uchar point 0 in
            if Uchar.utf_decode_is_valid decoded && Uchar.utf_decode_length decoded = String.length point then
              Uchar.to_int (Uchar.utf_decode_uchar decoded)
            else -1
          in
          let letter c = (c >= 97 && c <= 122) || (c >= 65 && c <= 90) || c >= 0x80 in
          let _, pieces =
            List.fold_left
              (fun (previous, acc) point ->
                let cased_point = if letter previous then cased `Lower point else cased `Upper point in
                (code point, cased_point :: acc))
              (-1, []) (points t)
          in
          Text (String.concat "" (List.rev pieces))
      | "translit", [ Text t ] ->
          let table =
            [ ("а", "a"); ("б", "b"); ("в", "v"); ("г", "g"); ("д", "d"); ("е", "e"); ("ё", "yo"); ("ж", "zh");
              ("з", "z"); ("и", "i"); ("й", "y"); ("к", "k"); ("л", "l"); ("м", "m"); ("н", "n"); ("о", "o");
              ("п", "p"); ("р", "r"); ("с", "s"); ("т", "t"); ("у", "u"); ("ф", "f"); ("х", "kh"); ("ц", "ts");
              ("ч", "ch"); ("ш", "sh"); ("щ", "shch"); ("ъ", ""); ("ы", "y"); ("ь", ""); ("э", "e"); ("ю", "yu");
              ("я", "ya") ]
          in
          Text
            (String.concat ""
               (List.map
                  (fun point ->
                    match List.assoc_opt point table with
                    | Some latin -> latin
                    | None -> (
                        match List.assoc_opt (cased `Lower point) table with
                        | Some latin when cased `Lower point <> point -> String.capitalize_ascii latin
                        | _ -> point))
                  (points t)))
      | "only-digits", [ Text t ] -> Text (String.concat "" (List.filter (fun p -> String.length p = 1 && p.[0] >= '0' && p.[0] <= '9') (points t)))
      | "redact", [ Text t; Int l; Int r ] ->
          let all = points t in
          let total = List.length all in
          let l = clamp l ~most:total and r = clamp r ~most:total in
          Text
            (String.concat ""
               (List.mapi
                  (fun i point ->
                    let ascii_other = String.length point = 1 && not (Char.code point.[0] >= 128 || (point.[0] >= '0' && point.[0] <= '9') || (point.[0] >= 'a' && point.[0] <= 'z') || (point.[0] >= 'A' && point.[0] <= 'Z')) in
                    if i < l || i >= total - r || ascii_other then point else "*")
                  all))
      | "like", [ Text t; Text pattern ] ->
          let text = Array.of_list (points t) and wanted = Array.of_list (points pattern) in
          let n = Array.length text and m = Array.length wanted in
          let memo = Hashtbl.create 64 in
          let rec fits i j =
            match Hashtbl.find_opt memo (i, j) with
            | Some answer -> answer
            | None ->
                let answer =
                  if j = m then i = n
                  else if wanted.(j) = "%" then fits i (j + 1) || (i < n && fits (i + 1) j)
                  else if wanted.(j) = "_" then i < n && fits (i + 1) (j + 1)
                  else if wanted.(j) = "\\" && j + 1 < m then i < n && text.(i) = wanted.(j + 1) && fits (i + 1) (j + 2)
                  else i < n && text.(i) = wanted.(j) && fits (i + 1) (j + 1)
                in
                Hashtbl.add memo (i, j) answer;
                answer
          in
          Bool (fits 0 0)
      | "format", Text template :: rest when List.for_all (function Text _ -> true | _ -> false) rest ->
          let fillers = List.map (function Text t -> t | _ -> "") rest in
          let rec fill text fillers acc =
            match (occurrences text "{}", fillers) with
            | at :: _, filler :: more ->
                fill (String.sub text (at + 2) (String.length text - at - 2)) more (filler :: String.sub text 0 at :: acc)
            | _ -> String.concat "" (List.rev (text :: acc))
          in
          Text (fill template fillers [])
      | "format", _ -> Null
      | "round-to", [ Int x; Int step ] ->
          if step <= 0L then Null
          else
            let half = Int64.div step 2L in
            Int (Int64.mul (Int64.div (if x >= 0L then Int64.add x half else Int64.sub x half) step) step)
      | "floor-to", [ Int x; Int step ] -> if step <= 0L then Null else Int (Int64.mul (floor_by x step) step)
      | "clamp", [ Int x; Int low; Int high ] -> if low > high then Null else Int (max low (min high x))
      | "keyed-noise", [ Int x; Int percent ] ->
          let percent = max 0L (min 100L percent) in
          let spread = Int64.div (Int64.mul (Int64.abs (max x (-4_000_000_000_000_000L))) percent) 100L in
          Int (Int64.add x (Keyed.within key (Int64.to_string x) ~low:(Int64.neg spread) ~high:spread))
      | "keyed-uuid", [ Text seed ] ->
          let h = Keyed.hex key seed 32 in
          Text
            (Printf.sprintf "%s-%s-4%s-%c%s-%s" (String.sub h 0 8) (String.sub h 8 4) (String.sub h 13 3)
               "89ab".[Char.code h.[16] mod 4] (String.sub h 17 3) (String.sub h 20 12))
      | "luhn", [ Text t ] ->
          let digits = List.filter_map (fun c -> if c >= '0' && c <= '9' then Some (Char.code c - 48) else None) (List.of_seq (String.to_seq t)) in
          let total =
            List.fold_left ( + ) 0
              (List.mapi (fun i d -> if i mod 2 = 0 then (if d * 2 > 9 then d * 2 - 9 else d * 2) else d) (List.rev digits))
          in
          Text (string_of_int ((10 - (total mod 10)) mod 10))
      | ("year" | "month" | "day"), [ Date d ] -> (
          match civil d with
          | Some (y, m, day) -> Int (Int64.of_int (match name with "year" -> y | "month" -> m | _ -> day))
          | None -> Null)
      | "make-date", [ Int y; Int m; Int d ] ->
          if y < 0L || y > 9999L || m < 1L || m > 12L || d < 1L || d > 31L then Null
          else Option.fold (days_of (Int64.to_int y, Int64.to_int m, Int64.to_int d)) ~none:Null ~some:(fun days -> Date days)
      | "add-days", [ Date d; Int n ] -> if Int64.abs n > 4_000_000L then Null else in_years (d + Int64.to_int n)
      | "days-between", [ Date a; Date b ] -> Int (Int64.of_int (b - a))
      | "date->text", [ Date d ] -> Text (date_shown d)
      | "text->date", [ Text t ] -> date_read (String.trim t)
      | "keyed-date", [ Text seed; Date low; Date high ] ->
          if low > high then Null else Date (Int64.to_int (Keyed.within key seed ~low:(Int64.of_int low) ~high:(Int64.of_int high)))
      | "date-of", [ Datetime x ] -> Date (Int64.to_int (floor_by x per_day))
      | "seconds-of-day", [ Datetime x ] -> Int (Int64.div (Int64.sub x (Int64.mul (floor_by x per_day) per_day)) 1_000_000L)
      | "make-datetime", [ Date d; Int sec ] ->
          if sec < 0L || sec > 86_399L then Null else Datetime (Int64.add (Int64.mul (Int64.of_int d) per_day) (Int64.mul sec 1_000_000L))
      | "add-seconds", [ Datetime x; Int n ] ->
          if Int64.abs n > 400_000_000_000L then Null
          else
            let moved = Int64.add x (Int64.mul n 1_000_000L) in
            (match in_years (Int64.to_int (floor_by moved per_day)) with Date _ -> Datetime moved | _ -> Null)
      | "datetime->text", [ Datetime x ] -> Text (datetime_shown x)
      | "text->datetime", [ Text t ] -> datetime_read t
      | _, _ -> Null)

(** What a test expression gives. *)
type kind = K_text | K_int | K_bool | K_date | K_datetime [@@deriving sexp_of]

let generated ~value_kind =
  QCheck2.Gen.(
    let text_pool =
      [ ""; "a"; "Иван Петров"; "x@mail.ru"; "+7 (912) 345-67-89"; " pad "; "a\"b\\c"; "ёЁ"; "12"; "-7"; "aXa";
        "щука-ЮЛЯ o'brien"; "4111 1111 1111 111"; "{}-{}"; "a.b.c" ]
    and date_pool = [ "2000-02-29"; "1999-12-31"; "1970-01-01"; "2024-13-01"; "abc"; "0001-01-01"; "9999-12-31"; "2023-02-29" ]
    and datetime_pool = [ "2024-03-01 13:45:10"; "2024-03-01 23:59:59.5"; "1969-12-31 23:59:59"; "2000-01-01"; "bad"; "2024-03-01 24:00:00" ]
    and pattern_pool = [ "%"; "a%"; "%@%.ru"; "_X_"; "И%ов"; "\\%"; "%\\"; "" ] in
    let rec gen env kind depth =
      let vars = List.filter_map (fun (name, k) -> if k = kind then Some (pure (Var name)) else None) env in
      let constants =
        match kind with
        | K_text -> [ map (fun t -> Lit_text t) (oneof_list text_pool); pure (Row "a") ]
        | K_int -> [ map (fun n -> Lit_int (Int64.of_int n)) (int_range (-5) 40); pure (Row "n") ]
        | K_bool -> [ map (fun b -> Lit_bool b) bool ]
        | K_date -> [ map (fun t -> Call ("text->date", [ Lit_text t ])) (oneof_list date_pool); pure (Row "d") ]
        | K_datetime -> [ map (fun t -> Call ("text->datetime", [ Lit_text t ])) (oneof_list datetime_pool); pure (Row "t") ]
      and current = if kind = value_kind then [ pure Value ] else [] in
      let leaf = oneof (constants @ current @ vars) in
      if depth <= 0 then oneof_weighted [ (12, leaf); (1, pure Lit_null) ]
      else
        let sub k = gen env k (depth - 1) in
        let many k lo hi = list_size (int_range lo hi) (sub k) in
        let call name args = map (fun args -> Call (name, args)) (flatten_list args) in
        let small = map (fun n -> Lit_int (Int64.of_int n)) (int_range (-3) 30) in
        let short = map (fun t -> Lit_text t) (oneof_list text_pool) in
        let let_ =
          let* bound_kind = oneof_list [ K_text; K_int; K_bool; K_date; K_datetime ] in
          let name = "v" ^ string_of_int (List.length env) in
          let* bound = sub bound_kind in
          let* body = gen ((name, bound_kind) :: env) kind (depth - 1) in
          pure (Let (name, bound, body))
        in
        let common =
          [
            (3, leaf);
            (1, map3 (fun c a b -> If (c, a, b)) (sub K_bool) (sub kind) (sub kind));
            (1, let_);
            (1, map (fun args -> Call ("coalesce", args)) (many kind 1 3));
          ]
        in
        let specific =
          match kind with
          | K_text ->
              [
                (2, map (fun args -> Call ("concat", args)) (many K_text 1 3));
                (1, call "substring" [ sub K_text; sub K_int; sub K_int ]);
                (1, oneof_list [ "left"; "right" ] >>= fun name -> call name [ sub K_text; sub K_int ]);
                ( 1,
                  oneof_list [ "upper"; "lower"; "trim"; "shape"; "email"; "hash"; "reverse"; "title"; "translit"; "only-digits"; "keyed-uuid"; "luhn" ]
                  >>= fun name -> call name [ sub K_text ] );
                (1, call "replace" [ sub K_text; sub K_text; short ]);
                ( 1,
                  oneof_list [ "before"; "after"; "before-last"; "after-last" ] >>= fun name ->
                  call name [ sub K_text; sub K_text ] );
                (1, oneof_list [ "pad-left"; "pad-right" ] >>= fun name -> call name [ sub K_text; small; sub K_text ]);
                (1, call "str" [ sub K_int ]);
                (1, call "digits" [ sub K_text; small ]);
                (1, map2 (fun seed cs -> Call ("pick", seed :: cs)) (sub K_text) (many K_text 1 3));
                (1, oneof_list [ "min"; "max" ] >>= fun name -> map (fun args -> Call (name, args)) (many K_text 1 3));
                (1, call "split-part" [ sub K_text; short; small ]);
                (1, call "repeat" [ short; small ]);
                (1, call "redact" [ sub K_text; small; small ]);
                (1, map (fun args -> Call ("format", args)) (many K_text 1 4));
                (1, call "date->text" [ sub K_date ]);
                (1, call "datetime->text" [ sub K_datetime ]);
              ]
          | K_int ->
              [
                (1, call "length" [ sub K_text ]);
                (1, call "int" [ sub K_text ]);
                (1, oneof_list [ "+"; "*" ] >>= fun name -> map (fun args -> Call (name, args)) (many K_int 2 3));
                (1, map (fun args -> Call ("-", args)) (many K_int 1 2));
                (1, oneof_list [ "/"; "mod"; "round-to"; "floor-to"; "keyed-noise" ] >>= fun name -> call name [ sub K_int; sub K_int ]);
                (1, call "abs" [ sub K_int ]);
                (1, call "keyed-int" [ sub K_text; sub K_int; sub K_int ]);
                (1, call "clamp" [ sub K_int; sub K_int; sub K_int ]);
                (1, call "hash-int" [ sub K_int ]);
                (1, call "index-of" [ sub K_text; sub K_text ]);
                (1, map2 (fun seed cs -> Call ("pick", seed :: cs)) (sub K_text) (many K_int 1 3));
                (1, oneof_list [ "min"; "max" ] >>= fun name -> map (fun args -> Call (name, args)) (many K_int 1 3));
                (1, oneof_list [ "year"; "month"; "day" ] >>= fun name -> call name [ sub K_date ]);
                (1, call "days-between" [ sub K_date; sub K_date ]);
                (1, call "seconds-of-day" [ sub K_datetime ]);
              ]
          | K_bool ->
              [
                ( 2,
                  let* name = oneof_list [ "="; "<>"; "<"; "<="; ">"; ">=" ]
                  and* k = oneof_list [ K_text; K_int; K_date; K_datetime ] in
                  call name [ sub k; sub k ] );
                (1, oneof_list [ "and"; "or" ] >>= fun name -> map (fun args -> Call (name, args)) (many K_bool 1 3));
                (1, call "not" [ sub K_bool ]);
                (1, oneof_list [ K_text; K_int; K_bool ] >>= fun k -> call "is-null" [ sub k ]);
                ( 1,
                  oneof_list [ "starts-with"; "ends-with"; "contains" ] >>= fun name -> call name [ sub K_text; sub K_text ] );
                (1, call "like" [ sub K_text; map (fun t -> Lit_text t) (oneof_list pattern_pool) ]);
              ]
          | K_date ->
              [
                (1, call "add-days" [ sub K_date; sub K_int ]);
                (1, call "make-date" [ map (fun n -> Lit_int (Int64.of_int n)) (int_range 1990 2030); small; small ]);
                (1, call "keyed-date" [ sub K_text; sub K_date; sub K_date ]);
                (1, call "date-of" [ sub K_datetime ]);
                (1, oneof_list [ "min"; "max" ] >>= fun name -> map (fun args -> Call (name, args)) (many K_date 1 3));
              ]
          | K_datetime ->
              [
                (1, call "make-datetime" [ sub K_date; oneof [ small; map (fun n -> Lit_int (Int64.of_int n)) (int_range 0 86_400) ] ]);
                (1, call "add-seconds" [ sub K_datetime; sub K_int ]);
              ]
        in
        oneof_weighted (common @ specific)
    in
    let* result = oneof_list [ K_text; K_int; K_bool; K_date; K_datetime ] in
    let* depth = int_range 0 4 in
    let* e = gen [] result depth in
    pure (result, e))

let script_kind : kind -> Mask_script.kind = function
  | K_text -> Text
  | K_int -> Int
  | K_bool -> Bool
  | K_date -> Date
  | K_datetime -> Datetime

let to_script_value = function
  | Null -> Mask_script.Null
  | Text t -> Mask_script.Text t
  | Int n -> Mask_script.Int n
  | Bool b -> Mask_script.Bool b
  | Date d -> Mask_script.Date d
  | Datetime x -> Mask_script.Datetime x

let compiled_matches_reference =
  QCheck2.Test.make ~count:1000 ~name:"compiled expressions give what a plain interpreter of the language gives"
    ~print:(fun (value_kind, (result, e), value, row) ->
      Printf.sprintf "%s → %s: %s with value %s, row %s"
        (Sexplib0.Sexp.to_string (sexp_of_kind value_kind))
        (Sexplib0.Sexp.to_string (sexp_of_kind result))
        (printed e)
        (Sexplib0.Sexp.to_string (sexp_of_v value))
        (Sexplib0.Sexp.to_string ([%sexp_of: (string * v) list] row)))
    QCheck2.Gen.(
      let* value_kind = oneof_list [ K_text; K_int; K_date; K_datetime ] in
      let* program = generated ~value_kind in
      let texts = oneof [ map (fun t -> Text t) (oneof_list [ "Анна Смирнова"; "user@corp.ru"; ""; "42"; "a b c" ]); pure Null ] in
      let ints = oneof [ map (fun n -> Int (Int64.of_int n)) (int_range (-100) 100); pure Null ] in
      let dates = oneof [ map (fun n -> Date n) (int_range (-25_000) 25_000); pure Null ] in
      let datetimes = oneof [ map (fun n -> Datetime (Int64.mul (Int64.of_int n) 1_000_017L)) (int_range (-2_000_000_000) 2_000_000_000); pure Null ] in
      let of_kind = function K_text | K_bool -> texts | K_int -> ints | K_date -> dates | K_datetime -> datetimes in
      let* value = of_kind value_kind in
      let* a = texts and* n = ints and* d = dates and* t = datetimes in
      pure (value_kind, program, value, [ ("a", a); ("n", n); ("d", d); ("t", t) ]))
    (fun (value_kind, (result, e), value, row) ->
      match Mask_script.expression ~source:"test" (printed e) with
      | Error (`Mask_script error) -> QCheck2.Test.fail_reportf "parse: %s" (Mask_script.describe error)
      | Ok parsed -> (
          match
            Mask_script.compile Mask_script.empty parsed ~value:(script_kind value_kind)
              ~columns:(fun name : Mask_script.kind option ->
                match name with "a" -> Some Text | "n" -> Some Int | "d" -> Some Date | "t" -> Some Datetime | _ -> None)
              ~result:(script_kind result)
          with
          | Error (`Mask_script error) -> QCheck2.Test.fail_reportf "compile: %s" (Mask_script.describe error)
          | Ok compiled ->
              let given = Array.of_list (List.map (fun name -> to_script_value (List.assoc name row)) (Mask_script.reads compiled)) in
              let got = Mask_script.run compiled key ~value:(to_script_value value) ~row:given in
              let expected = to_script_value (eval ~value ~row ~env:[] e) in
              if got = expected then true
              else
                QCheck2.Test.fail_reportf "got %s, expected %s"
                  (Sexplib0.Sexp.to_string (Mask_script.sexp_of_value got))
                  (Sexplib0.Sexp.to_string (Mask_script.sexp_of_value expected))))

let refusals () =
  let told ?(script = "") expression =
    match
      Result.bind (Mask_script.parse ~source:"s" ~load:(fun _ -> Error "no files") script) (fun script ->
          Result.bind (Mask_script.expression ~source:"e" expression) (fun parsed ->
              Mask_script.compile script parsed ~value:Text
                ~columns:(fun name : Mask_script.kind option -> match name with "a" -> Some Text | _ -> None)
                ~result:Text))
    with
    | Ok _ -> "fine"
    | Error (`Mask_script error) -> Mask_script.describe error
  in
  let deep =
    String.concat "\n"
      (List.init 20 (fun i ->
           if i = 0 then "(define (f0 x) (concat x x))"
           else Printf.sprintf "(define (f%d x) (f%d (f%d x)))" i (i - 1) (i - 1)))
  in
  Alcotest.(check (list string))
    "refused"
    [
      "fine";
      "e:1:15: expected text, this gives an integer";
      "e:1:2: frob is no function or name here";
      "e:1:1: concat takes 1 or more arguments, not 0";
      "e:1:13: the table has no column b of text, integers, dates or times";
      "e:1:1: an expression is missing";
      "e:1:1: a list is never closed";
      "e:1:3: one expression, not several";
      "e:1:9: a text is never closed";
      "s:1:15: f is defined below: a function calls only those defined above it";
      "s:2:10: f is already defined";
      "s:1:10: concat is already defined";
      "e:1:1: cond ends without (else …)";
      "e:1:1: define cannot stand here";
      "e:1:9: if takes 3 arguments, not 2";
      "e:1:14: x is no function or name here";
      "s:1:1: (mask PATTERN BODY) is misplaced";
      "e:1:1: f19 grows";
      "fine";
      "e:1:17: expected an integer, this gives text";
    ]
    (List.map
       (fun (script, expression) ->
         let said = told ~script expression in
         if Base.String.is_substring said ~substring:"hundred thousand" then "e:1:1: f19 grows"
         else if Base.String.is_prefix said ~prefix:"s:1:1:" && Base.String.is_substring said ~substring:"mask" then
           "s:1:1: (mask PATTERN BODY) is misplaced"
         else said)
       [
         ("", "(concat value (row a))");
         ("", "(concat value 5)");
         ("", " (frob value)");
         ("", "(concat)");
         ("", "(concat \"a\" (row b))");
         ("", "");
         ("", "(concat (upper value)");
         ("", "a b");
         ("", "(concat \"abc)");
         ("(define (f x) (f x))", "(f value)");
         ("(define (f x) x)\n(define (f y) y)", "(f value)");
         ("(define (concat x) x)", "value");
         ("", "(cond ((= value \"a\") \"b\"))");
         ("", "(define (g) 1)");
         ("", "(concat (if true \"a\"))");
         ("", "(let ((y 1)) x)");
         ("(mask \"a.b.c\")", "value");
         (deep, "(f19 value)");
         ("; a comment\n(define (twice x) (concat x x)) ; and another\n", "(twice (let ((s (shape value))) s))");
         ("", "(str (if true 1 \"b\"))");
       ])

let examples () =
  let run expression =
    let value = Mask_script.Text "Иван Петров" in
    match
      Result.bind (Mask_script.expression ~source:"e" expression) (fun parsed ->
          Mask_script.compile Mask_script.empty parsed ~value:Text
            ~columns:(fun name : Mask_script.kind option ->
              match name with "a" -> Some Text | "n" -> Some Int | _ -> None)
            ~result:Text)
    with
    | Error (`Mask_script error) -> "refused: " ^ Mask_script.describe error
    | Ok compiled ->
        let row =
          Array.of_list
            (List.map
               (function "a" -> Mask_script.Text "Anna Smith" | _ -> Mask_script.Int 7L)
               (Mask_script.reads compiled))
        in
        match Mask_script.run compiled key ~value ~row with
        | Null -> "Null"
        | other -> Mask_script.shown other
  in
  let cases =
    [
      ("(upper value)", "ИВАН ПЕТРОВ");
      ("(lower \"ЁЛКА Tree\")", "ёлка tree");
      ("(upper \"ёж\")", "ЁЖ");
      ("(replace \"aXaXa\" \"a\" \"bb\")", "bbXbbXbb");
      ("(replace \"aaa\" \"aa\" \"b\")", "ba");
      ("(replace \"abc\" \"\" \"z\")", "abc");
      ("(before \"a.b.c\" \".\")", "a");
      ("(after \"a.b.c\" \".\")", "b.c");
      ("(before-last \"a.b.c\" \".\")", "a.b");
      ("(after-last \"a.b.c\" \".\")", "c");
      ("(before \"abc\" \"x\")", "abc");
      ("(after \"abc\" \"x\")", "");
      ("(before \"abc\" \"\")", "abc");
      ("(substring value 2 3)", "ан ");
      ("(substring value 9 100)", "ов");
      ("(substring value -3 2)", "Ив");
      ("(left value 3)", "Ива");
      ("(right value 3)", "ров");
      ("(right value 100)", "Иван Петров");
      ("(pad-left \"7\" 3 \"ж\")", "жж7");
      ("(pad-right \"7\" 3 \"0x\")", "700");
      ("(pad-left \"long\" 2 \"0\")", "long");
      ("(str (length value))", "11");
      ("(str (int \" 42 \"))", "42");
      ("(str (int \"+5\"))", "Null");
      ("(str (int \"4a\"))", "Null");
      ("(str (int \"99999999999999999999\"))", "Null");
      ("(str (int \"-12\"))", "-12");
      ("(str (/ 7 2))", "3");
      ("(str (/ -7 2))", "-3");
      ("(str (/ 7 0))", "Null");
      ("(str (mod 7 0))", "Null");
      ("(str (mod -7 3))", "-1");
      ("(str (- 5))", "-5");
      ("(str (- 5 7))", "-2");
      ("(str (* 2 3 4))", "24");
      ("(str (+ 1 null))", "Null");
      ("(str (abs -4))", "4");
      ("(str (< 3 3))", "refused: e:1:6: expected an integer, this gives true or false");
      ("(if (< 3 3) \"yes\" \"no\")", "no");
      ("(if (<= 3 3) \"yes\" \"no\")", "yes");
      ("(if (< \"a\" \"b\") \"yes\" \"no\")", "yes");
      ("(if (> \"b\" \"a\") \"yes\" \"no\")", "yes");
      ("(if (>= 2 3) \"yes\" \"no\")", "no");
      ("(if (<> 2 3) \"yes\" \"no\")", "yes");
      ("(if (= null 3) \"yes\" \"no\")", "no");
      ("(if (not (= null 3)) \"yes\" \"no\")", "no");
      ("(if (or false null true) \"yes\" \"no\")", "yes");
      ("(if (and true null) \"yes\" \"no\")", "no");
      ("(if (is-null (row a)) \"yes\" \"no\")", "no");
      ("(if (contains value \"\") \"yes\" \"no\")", "yes");
      ("(if (starts-with value \"Ив\") \"yes\" \"no\")", "yes");
      ("(if (ends-with value \"ов\") \"yes\" \"no\")", "yes");
      ("(min \"b\" \"a\" \"c\")", "a");
      ("(max \"b\" \"a\" \"c\")", "c");
      ("(str (min 3 1 2))", "1");
      ("(str (max 3 1 2))", "3");
      ("(min \"b\" null)", "Null");
      ("(coalesce null (row a) value)", "Anna Smith");
      ("(concat (row a) \"/\" (str (row n)))", "Anna Smith/7");
      ("(concat \"a\" null)", "Null");
      ("(let ((x (upper (row a))) (y (concat x \"!\"))) y)", "ANNA SMITH!");
      ("(cond ((= value \"x\") \"one\") ((starts-with value \"И\") \"two\") (else \"three\"))", "two");
      ("(trim \"  a b  \")", "a b");
      ("(split-part \"a,b,,c\" \",\" 2)", "b");
      ("(split-part \"a,b,,c\" \",\" 3)", "");
      ("(split-part \"a,b\" \",\" 5)", "");
      ("(split-part \"a,b\" \",\" 0)", "");
      ("(str (index-of value \"Пе\"))", "5");
      ("(str (index-of value \"zz\"))", "-1");
      ("(repeat \"ab\" 3)", "ababab");
      ("(reverse \"абв\")", "вба");
      ("(title \"иВАН петров-водкин o'brien\")", "Иван Петров-Водкин O'Brien");
      ("(translit \"Щука Ёжикова\")", "Shchuka Yozhikova");
      ("(only-digits \"+7 (912) 345\")", "7912345");
      ("(redact value 1 2)", "И*** ****ов");
      ("(redact \"a@b.cd\" 0 0)", "*@*.**");
      ("(if (like \"x@mail.ru\" \"%@%.ru\") \"yes\" \"no\")", "yes");
      ("(if (like \"a%b\" \"a\\\\%b\") \"yes\" \"no\")", "yes");
      ("(if (like \"aXb\" \"a\\\\%b\") \"yes\" \"no\")", "no");
      ("(if (like \"ab\" \"a_\") \"yes\" \"no\")", "yes");
      ("(if (like \"abc\" \"a_\") \"yes\" \"no\")", "no");
      ("(format \"{}-{}\" \"a\" \"b\")", "a-b");
      ("(format \"{} {}\" \"a\")", "a {}");
      ("(str (round-to 1499 1000))", "1000");
      ("(str (round-to 1500 1000))", "2000");
      ("(str (round-to -1500 1000))", "-2000");
      ("(str (floor-to -1 10))", "-10");
      ("(str (clamp 50 0 10))", "10");
      ("(str (clamp 5 10 0))", "Null");
      ("(str (clamp -5 0 10))", "0");
      ("(date->text (text->date \"2000-1-01\"))", "Null");
      ("(date->text (text->date \" 2000-01-02 \"))", "2000-01-02");
      ("(luhn \"7992739871\")", "3");
      ("(date->text (add-days (text->date \"2000-02-28\") 1))", "2000-02-29");
      ("(date->text (text->date \"2023-02-29\"))", "Null");
      ("(str (year (text->date \"1985-07-14\")))", "1985");
      ("(str (days-between (text->date \"2000-01-01\") (text->date \"2000-03-01\")))", "60");
      ("(date->text (make-date 2024 2 30))", "Null");
      ("(datetime->text (add-seconds (text->datetime \"2024-03-01 23:59:00.5\") 90))", "2024-03-02 00:00:30.500000");
      ("(str (seconds-of-day (text->datetime \"1969-12-31 23:59:59\")))", "86399");
      ("(date->text (date-of (text->datetime \"1969-12-31 23:59:59\")))", "1969-12-31");
      ("(datetime->text (make-datetime (text->date \"2024-01-01\") 86400))", "Null");
      ("(date->text (add-days (text->date \"9999-12-31\") 1))", "Null");
    ]
  in
  Alcotest.(check (list (pair string string)))
    "each function as specified"
    cases
    (List.map (fun (expression, _) -> (expression, run expression)) cases);
  Alcotest.(check (list string))
    "keyed functions are Keyed's under the run's key"
    [
      Keyed.shape key "Иван Петров";
      Keyed.email key "x@y.z";
      Keyed.hash_text key "Иван Петров";
      Keyed.digits key "Иван Петров" 4;
      Int64.to_string (Keyed.within key "Иван Петров" ~low:10L ~high:20L);
      Int64.to_string (Keyed.hash_int64 key 7L);
      List.nth [ "p"; "q"; "r" ] (Keyed.choice key "Иван Петров" 3);
      Int64.to_string (Int64.add 100000L (Keyed.within key "100000" ~low:(-10000L) ~high:10000L));
      (let h = Keyed.hex key "Иван Петров" 32 in
       Printf.sprintf "%s-%s-4%s-%c%s-%s" (String.sub h 0 8) (String.sub h 8 4) (String.sub h 13 3)
         "89ab".[Char.code h.[16] mod 4] (String.sub h 17 3) (String.sub h 20 12));
    ]
    (List.map run
       [
         "(shape value)";
         "(email \"x@y.z\")";
         "(hash value)";
         "(digits value 4)";
         "(str (keyed-int value 10 20))";
         "(str (hash-int (row n)))";
         "(pick value \"p\" \"q\" \"r\")";
         "(str (keyed-noise 100000 10))";
         "(keyed-uuid value)";
       ])

let rows_read_before_masking () =
  let table =
    Support.table
      [ ("id", Int64, Not_null); ("b", String, Nullable); ("c", String, Nullable); ("a", String, Nullable); ("n", Int32, Not_null) ]
  in
  let script =
    match
      Mask_script.parse ~source:"s" ~load:(fun _ -> Error "no files")
        "(define (tagged x) (concat x \"!\"))\n\
         (mask \"db.t.c\" (upper (row c)))\n\
         (mask \"db.t.a\" (concat (tagged (row b)) (row c)))\n\
         (mask \"db.t.n\" (+ value (length (coalesce (row b) \"\"))))"
    with
    | Ok script -> script
    | Error _ -> Alcotest.fail "script"
  in
  let rules =
    match (Mask.rule "db.t.b=shape", Mask.scripted script) with
    | Ok shape, Ok scripted -> shape :: scripted
    | _ -> Alcotest.fail "rules"
  in
  match Mask.resolve (Mask.make key ~script ~strict:[] rules) ~relations:(fun () -> Ok Relations.none) [ table ] with
  | Ok [ mask ] ->
      Test_mask.with_batch [ Int64; String; String; String; Int32 ]
        [
          [ Test_mask.Big 1L; Text "Hello"; Text "c1"; Text "x"; Int 10 ];
          [ Big 2L; Null; Text "c2"; Text "y"; Int 20 ];
        ]
        (fun sealed ->
          match Mask.Table.apply mask (borrow_ sealed) with
          | Error _ -> Alcotest.fail "apply"
          | Ok () ->
              Alcotest.(check (list string))
                "a takes b and c as read, b is shaped, c upper, n adds b's length"
                [ "Hello!c1"; "shaped"; "C1"; "15"; "null"; "shaped null"; "C2"; "20" ]
                (List.concat_map
                   (fun row ->
                     match row with
                     | [ _; b; c; a; Test_mask.Int n ] ->
                         let text = function Test_mask.Text t -> t | _ -> "null" in
                         let shaped =
                           match b with
                           | Test_mask.Text t when t <> "Hello" && String.length t = 5 -> "shaped"
                           | Test_mask.Null -> "shaped null"
                           | _ -> "not shaped"
                         in
                         [ text a; shaped; text c; string_of_int n ]
                     | _ -> [ "a row of another shape" ])
                   (Test_mask.cells_of sealed)))
  | Ok _ | Error _ -> Alcotest.fail "resolve"

let refused_against_tables () =
  let table =
    Support.table
      [ ("id", Int64, Not_null); ("s", String, Not_null); ("d", Date, Nullable); ("n", Int32, Nullable); ("f", Float64, Nullable) ]
  in
  let told rule =
    match Mask.rule rule with
    | Error _ -> "parse"
    | Ok rule -> (
        match Mask.resolve (Mask.make key ~script:Mask_script.empty ~strict:[] [ rule ]) ~relations:(fun () -> Ok Relations.none) [ table ] with
        | Ok _ -> "fine"
        | Error (`Mask (Scripted { error; _ })) -> Mask_script.describe error
        | Error (`Mask error) -> Sexplib0.Sexp.to_string (Mask.sexp_of_error error))
  in
  Alcotest.(check (list string))
    "refused"
    [
      "fine";
      "db.t.n=expr:(concat value \"x\"):1:9: expected text, this gives an integer";
      "fine";
      "db.t.s=expr:(row d):1:1: expected text, this gives a date";
      "fine";
      "(Unfit(table(db t))(column f)(action expr)(ltype Float64))";
      "db.t.s=expr:(row f):1:1: the table has no column f of text, integers, dates or times";
    ]
    (List.map told
       [
         "db.t.s=expr:(upper value)";
         "db.t.n=expr:(concat value \"x\")";
         "db.t.d=expr:value";
         "db.t.s=expr:(row d)";
         "db.t.n=expr:(+ value (length (row s)))";
         "db.t.f=expr:value";
         "db.t.s=expr:(row f)";
       ]);
  let null_into_not_null =
    match Mask.rule "db.t.s=expr:(if (= value \"b\") null value)" with
    | Error _ -> "parse"
    | Ok rule -> (
        match Mask.resolve (Mask.make key ~script:Mask_script.empty ~strict:[] [ rule ]) ~relations:(fun () -> Ok Relations.none) [ table ] with
        | Ok [ mask ] ->
            Test_mask.with_batch [ Int64; String; Date; Int32; Float64 ]
              [ [ Test_mask.Big 1L; Text "a"; Null; Null; Null ]; [ Big 2L; Text "b"; Null; Null; Null ] ]
              (fun sealed ->
                match Mask.Table.apply mask (borrow_ sealed) with
                | Ok () -> "went through"
                | Error (`Mask error) -> Sexplib0.Sexp.to_string (Mask.sexp_of_error error))
        | Ok _ | Error _ -> "resolve")
  in
  Alcotest.(check string) "NULL into NOT NULL" "(Null_from_expression(table(db t))(column s))" null_into_not_null

let configuration () =
  let table =
    Support.table
      [
        ("id", Int64, Not_null);
        ("name", String, Nullable);
        ("phone", String, Nullable);
        ("born", Date, Nullable);
        ("note", Bytes, Nullable);
        ("seen", Datetime (Support.get_ok (Ltype.Fsp.of_int 0)), Nullable);
      ]
  in
  let files = [ ("names.txt", "# имена\nАнна\n\nБорис\n  Вера  \n"); ("empty.txt", "# nothing\n\n") ] in
  let load path = Option.fold (List.assoc_opt path files) ~none:(Error "no such file") ~some:(fun text -> Ok text) in
  let resolved ?(strict = []) text =
    match Mask_script.parse ~source:"s" ~load text with
    | Error (`Mask_script error) -> Error (Mask_script.describe error)
    | Ok script -> (
        match Mask.scripted script with
        | Error _ -> Error "pattern"
        | Ok rules -> (
            let strict =
              List.map
                (fun (({ line; _ } : Mask_script.position), table) ->
                  match Pattern.table table with
                  | Ok pattern -> (Printf.sprintf "s:%d strict %s" line table, pattern)
                  | Error _ -> Alcotest.fail table)
                (Mask_script.strict script)
              @ strict
            in
            match Mask.resolve (Mask.make key ~script ~strict rules) ~relations:(fun () -> Ok Relations.none) [ table ] with
            | Ok [ mask ] -> Ok (script, mask)
            | Ok _ -> Error "tables"
            | Error (`Mask (Scripted { error; _ })) -> Error (Mask_script.describe error)
            | Error (`Mask error) -> Error (Sexplib0.Sexp.to_string (Mask.sexp_of_error error))))
  in
  let masked text =
    match resolved text with
    | Ok (_, mask) -> String.concat " " (List.map (fun ((c : Ident.t), a) -> (c :> string) ^ ":" ^ a) (Mask.Table.masked mask))
    | Error said -> said
  in
  Alcotest.(check (list string))
    "selectors, keep, strict and words"
    [
      "name:expr phone:expr";
      "name:expr";
      "(Unhandled(table(db t))(columns(phone born note seen)))";
      "name:expr";
      "(Unused\"s:1 strict db.zzz\")";
      "s:1:35: textt is no column type: text, binary, json, integer, decimal, float, date, datetime, timestamp, time, year, enum, set, geometry";
      "name:expr";
      "s:1:1: missing.txt: no such file";
      "s:1:1: empty.txt: no words in it";
      "s:1:36: nobody is no function or name here";
      "s:2:1: names is already defined";
      "born:expr seen:expr";
    ]
    [
      masked "(mask (where (table \"db.t\") (column \"*\") (type text)) (shape value))";
      masked "(keep \"db.t.phone\")\n(mask (where (type text)) (shape value))";
      masked "(strict \"db.t\")\n(keep \"db.t.id\")\n(mask \"db.t.name\" (shape value))";
      masked
        "(strict \"db.t\")\n(keep \"db.t.id\")\n(mask \"db.t.name\" (shape value))\n\
         (keep (where (table \"db.*\") (type date binary datetime)))\n(keep \"db.t.phone\")";
      masked "(strict \"db.zzz\")\n(mask \"db.t.name\" (shape value))";
      masked "(mask (where (table \"db.t\") (type textt)) (shape value))";
      masked "(words names \"names.txt\")\n(mask \"db.t.name\" (pick-word value names))";
      masked "(words names \"missing.txt\")";
      masked "(words names \"empty.txt\")";
      masked "(mask \"db.t.name\" (pick-word value nobody))";
      masked "(words names \"names.txt\")\n(words names \"names.txt\")";
      masked "(mask \"db.t.born\" (add-days value 1))\n(mask \"db.t.seen\" (add-seconds value 1))";
    ];
  (match Mask_script.parse ~source:"s" ~load "(after-load \"db.t\" \"UPDATE {table} SET a = 1\")\n(after-load db.u \"DELETE\")" with
  | Ok script ->
      Alcotest.(check (list (pair string string)))
        "after-load forms" [ ("db.t", "UPDATE {table} SET a = 1"); ("db.u", "DELETE") ]
        (List.map (fun (_, table, statement) -> (table, statement)) (Mask_script.after_load script))
  | Error _ -> Alcotest.fail "after-load");
  let applied text rows =
    match resolved text with
    | Error said -> [ [ said ] ]
    | Ok (_, mask) ->
        Test_mask.with_batch [ Int64; String; String; Date; Bytes; Datetime (Support.get_ok (Ltype.Fsp.of_int 0)) ] rows
          (fun sealed ->
            match Mask.Table.apply mask (borrow_ sealed) with
            | Error (`Mask error) -> [ [ Sexplib0.Sexp.to_string (Mask.sexp_of_error error) ] ]
            | Ok () ->
                List.map
                  (List.map (function
                    | Test_mask.Null -> "null"
                    | Text t -> t
                    | Int n -> string_of_int n
                    | Big n -> Int64.to_string n))
                  (Test_mask.cells_of sealed))
  in
  let row born seen = [ Test_mask.Big 1L; Text "Иван"; Text "+7 1"; born; Null; seen ] in
  Alcotest.(check (list (list string)))
    "dates and times through a batch"
    [ [ "1"; "Иван"; "+7 1"; "11"; "null"; "6000000" ]; [ "1"; "Иван"; "+7 1"; "null"; "null"; "null" ] ]
    (applied "(mask \"db.t.born\" (add-days value 1))\n(mask \"db.t.seen\" (add-seconds value 1))"
       [ row (Int 10) (Big 5_000_000L); row Null Null ]);
  Alcotest.(check (list (list string)))
    "a time finer than its column"
    [ [ "(Finer_than_column(table(db t))(column seen))" ] ]
    (applied "(mask \"db.t.seen\" (text->datetime \"2024-01-01 00:00:00.5\"))" [ row Null Null ]);
  Alcotest.(check (list (list string)))
    "words picked from the file"
    [ [ "true" ] ]
    (match resolved "(words names \"names.txt\")\n(mask \"db.t.name\" (pick-word value names))" with
    | Error said -> [ [ said ] ]
    | Ok (_, mask) ->
        Test_mask.with_batch [ Int64; String; String; Date; Bytes; Datetime (Support.get_ok (Ltype.Fsp.of_int 0)) ]
          (List.init 30 (fun i -> [ Test_mask.Big (Int64.of_int i); Text (string_of_int i); Null; Null; Null; Null ]))
          (fun sealed ->
            match Mask.Table.apply mask (borrow_ sealed) with
            | Error _ -> [ [ "apply" ] ]
            | Ok () ->
                let names = List.map (function [ _; Test_mask.Text t; _; _; _; _ ] -> t | _ -> "?") (Test_mask.cells_of sealed) in
                [
                  [
                    Bool.to_string
                      (List.for_all (fun n -> List.mem n [ "Анна"; "Борис"; "Вера" ]) names
                      && List.length (List.sort_uniq compare names) = 3);
                  ];
                ]))

let tests =
  [
    QCheck_alcotest.to_alcotest compiled_matches_reference;
    Alcotest.test_case "bad scripts are refused where they go wrong" `Quick refusals;
    Alcotest.test_case "each function of the language does what it says" `Quick examples;
    Alcotest.test_case "scripts select, keep, require and load what masks need" `Quick configuration;
    Alcotest.test_case "(row …) reads the row as read, before other columns are masked" `Quick rows_read_before_masking;
    Alcotest.test_case "expressions are typed against each column" `Quick refused_against_tables;
  ]
