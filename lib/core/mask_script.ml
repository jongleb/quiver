open Base.Result.Let_syntax

type position = { source : string; line : int; column : int } [@@deriving sexp_of]
type kind = Text | Int | Bool | Date | Datetime [@@deriving sexp_of]

type error =
  | Syntax of { at : position; reason : string }
  | Unknown of { at : position; name : string }
  | Arity of { at : position; name : string; expected : string; given : int }
  | Mistyped of { at : position; expected : kind; found : kind }
  | Unreadable_column of { at : position; column : string }
  | Defined_twice of { at : position; name : string }
  | Misplaced of { at : position; form : string }
  | Too_large of { at : position }
  | Called_before_defined of { at : position; name : string }
  | Unknown_type of { at : position; name : string }
  | Unloadable of { at : position; path : string; reason : string }
[@@deriving sexp_of]

type value = Null | Text of string | Int of int64 | Bool of bool | Date of int | Datetime of int64
[@@deriving sexp_of]

let describe error =
  let kind (kind : kind) =
    match kind with
    | Text -> "text"
    | Int -> "an integer"
    | Bool -> "true or false"
    | Date -> "a date"
    | Datetime -> "a date and time"
  in
  let at ({ source; line; column } : position) what = Printf.sprintf "%s:%d:%d: %s" source line column what in
  match error with
  | Syntax { at = where; reason } -> at where reason
  | Unknown { at = where; name } -> at where (Printf.sprintf "%s is no function or name here" name)
  | Arity { at = where; name; expected; given } ->
      let noun = if String.equal expected "1" then "argument" else "arguments" in
      at where (Printf.sprintf "%s takes %s %s, not %d" name expected noun given)
  | Mistyped { at = where; expected; found } ->
      at where (Printf.sprintf "expected %s, this gives %s" (kind expected) (kind found))
  | Unreadable_column { at = where; column } ->
      at where (Printf.sprintf "the table has no column %s of text, integers, dates or times" column)
  | Defined_twice { at = where; name } -> at where (Printf.sprintf "%s is already defined" name)
  | Misplaced { at = where; form } -> at where (Printf.sprintf "%s cannot stand here" form)
  | Too_large { at = where } -> at where "the expression grows past a hundred thousand steps"
  | Called_before_defined { at = where; name } ->
      at where (Printf.sprintf "%s is defined below: a function calls only those defined above it" name)
  | Unknown_type { at = where; name } ->
      at where
        (Printf.sprintf
           "%s is no column type: text, binary, json, integer, decimal, float, date, datetime, timestamp, time, year, enum, set, geometry"
           name)
  | Unloadable { at = where; path; reason } -> at where (Printf.sprintf "%s: %s" path reason)

(** A form as read: an atom, a quoted text, a number or a list. *)
type form =
  | Symbol of { at : position; name : string }
  | Quoted of { at : position; text : string }
  | Number of { at : position; value : int64 }
  | Items of { at : position; items : form list }

let read ~source text =
  let length = String.length text in
  let pos = ref 0 and line = ref 1 and column = ref 1 in
  let here () = { source; line = !line; column = !column } in
  let advance () =
    let () =
      if Char.equal text.[!pos] '\n' then (
        incr line;
        column := 1)
      else incr column
    in
    incr pos
  in
  let rec skip () =
    if !pos < length then
      match text.[!pos] with
      | ' ' | '\t' | '\n' | '\r' ->
          advance ();
          skip ()
      | ';' ->
          let rec line_end () =
            if !pos < length && not (Char.equal text.[!pos] '\n') then (
              advance ();
              line_end ())
          in
          line_end ();
          skip ()
      | _ -> ()
  in
  let ends_atom c =
    match c with ' ' | '\t' | '\n' | '\r' | '(' | ')' | '"' | ';' -> true | _ -> false
  in
  let rec form () =
    skip ();
    let at = here () in
    if !pos >= length then Error (Syntax { at; reason = "an expression is missing" })
    else
      match text.[!pos] with
      | '(' ->
          advance ();
          items at []
      | ')' -> Error (Syntax { at; reason = "a closing parenthesis with no opening one" })
      | '"' ->
          advance ();
          quoted at (Buffer.create 16)
      | _ -> atom at !pos
  and items at acc =
    skip ();
    if !pos >= length then Error (Syntax { at; reason = "a list is never closed" })
    else if Char.equal text.[!pos] ')' then (
      advance ();
      Ok (Items { at; items = List.rev acc }))
    else
      match form () with Ok item -> items at (item :: acc) | Error _ as error -> error
  and quoted at buffer =
    if !pos >= length then Error (Syntax { at; reason = "a text is never closed" })
    else
      match text.[!pos] with
      | '"' ->
          advance ();
          Ok (Quoted { at; text = Buffer.contents buffer })
      | '\\' -> (
          advance ();
          let escaped = if !pos < length then Some text.[!pos] else None in
          match escaped with
          | Some (('"' | '\\') as c) ->
              Buffer.add_char buffer c;
              advance ();
              quoted at buffer
          | Some 'n' ->
              Buffer.add_char buffer '\n';
              advance ();
              quoted at buffer
          | Some 't' ->
              Buffer.add_char buffer '\t';
              advance ();
              quoted at buffer
          | Some _ | None -> Error (Syntax { at = here (); reason = "an unknown escape in a text" }))
      | c ->
          Buffer.add_char buffer c;
          advance ();
          quoted at buffer
  and atom at start =
    let rec take () =
      if !pos < length && not (ends_atom text.[!pos]) then (
        advance ();
        take ())
    in
    take ();
    let name = String.sub text start (!pos - start) in
    let digits = Base.String.chop_prefix_if_exists name ~prefix:"-" in
    if (not (String.equal digits "")) && String.for_all (fun c -> c >= '0' && c <= '9') digits then
      Option.fold (Int64.of_string_opt name)
        ~none:(Error (Syntax { at; reason = "a number past 64 bits" }))
        ~some:(fun value -> Ok (Number { at; value }))
    else Ok (Symbol { at; name })
  in
  let rec all acc =
    skip ();
    if !pos >= length then Ok (List.rev acc)
    else match form () with Ok item -> all (item :: acc) | Error _ as error -> error
  in
  all []

type expression =
  | Literal of { at : position; value : value }
  | Current of { at : position }
  | Row of { at : position; column : string }
  | Name of { at : position; name : string }
  | If of { at : position; condition : expression; yes : expression; no : expression }
  | Let of { at : position; bindings : (position * string * expression) list; body : expression }
  | Call of { at : position; name : string; arguments : expression list }
[@@deriving sexp_of]

let reserved =
  [
    "if"; "cond"; "let"; "row"; "define"; "mask"; "else"; "value"; "null"; "true"; "false"; "where"; "keep";
    "strict"; "after-load"; "words"; "table"; "column"; "type"; "permute"; "references";
  ]

let column_types =
  [ "text"; "binary"; "json"; "integer"; "decimal"; "float"; "date"; "datetime"; "timestamp"; "time"; "year"; "enum"; "set"; "geometry" ]

type shape = Of of kind | Same | Words
type arity = Exactly | Optional_last | Repeated_last of int
type parameter = { name : string; shape : shape }
type example = { expression : string; given : (kind * string) option; gives : string }

type entry = {
  name : string;
  parameters : parameter list;
  arity : arity;
  result : shape;
  doc : string;
  examples : example list;
}

type section = { title : string; doc : string; entries : entry list }

let example_secret = "example"

let reference =
  let p name shape = { name; shape } in
  let text name = p name (Of Text) and int name = p name (Of Int) and date name = p name (Of Date) in
  let datetime name = p name (Of Datetime) and truth name = p name (Of Bool) and same name = p name Same in
  let on kind given expression gives = { expression; given = Some (kind, given); gives } in
  let alone expression gives = { expression; given = None; gives } in
  let fn ?(arity = Exactly) name parameters result doc examples = { name; parameters; arity; result; doc; examples } in
  [
    {
      title = "Text";
      doc = "Characters are code points: Cyrillic, accents and emoji count one each.";
      entries =
        [
          fn ~arity:(Repeated_last 1) "concat" [ text "text" ] (Of Text) "Joins its texts."
            [ alone {|(concat "user" "-" (str 42))|} "user-42" ];
          fn "length" [ text "text" ] (Of Int) "How many characters." [ on Text "Иван" "(length value)" "4" ];
          fn "substring" [ text "text"; int "start"; int "count" ] (Of Text)
            "COUNT characters from START, counted from 0; what lies outside is left out."
            [ on Text "Иван Петров" "(substring value 2 3)" "ан " ];
          fn "left" [ text "text"; int "count" ] (Of Text) "The first COUNT characters."
            [ on Text "Иван Петров" "(left value 3)" "Ива" ];
          fn "right" [ text "text"; int "count" ] (Of Text) "The last COUNT characters."
            [ on Text "+7 (912) 345-67-89" "(right value 5)" "67-89" ];
          fn "upper" [ text "text" ] (Of Text) "Latin and Cyrillic letters in upper case."
            [ on Text "ёлка Tree" "(upper value)" "ЁЛКА TREE" ];
          fn "lower" [ text "text" ] (Of Text) "Latin and Cyrillic letters in lower case."
            [ on Text "ЁЛКА Tree" "(lower value)" "ёлка tree" ];
          fn "title" [ text "text" ] (Of Text) "Each word's first letter in upper case, the others in lower."
            [ on Text "иВАН петров-водкин" "(title value)" "Иван Петров-Водкин" ];
          fn "trim" [ text "text" ] (Of Text) "Without the spaces at either end." [ on Text "  a b  " "(trim value)" "a b" ];
          fn "reverse" [ text "text" ] (Of Text) "The characters back to front." [ on Text "абв" "(reverse value)" "вба" ];
          fn "replace" [ text "text"; text "from"; text "to" ] (Of Text)
            "Every FROM, left to right, replaced by TO; an empty FROM changes nothing."
            [ on Text "345-67-89" {|(replace value "-" "")|} "3456789" ];
          fn "before" [ text "text"; text "separator" ] (Of Text)
            "What comes before the first SEPARATOR; the whole text when there is none."
            [ on Text "anna@mail.ru" {|(before value "@")|} "anna" ];
          fn "after" [ text "text"; text "separator" ] (Of Text)
            "What comes after the first SEPARATOR; empty when there is none."
            [ on Text "anna@mail.ru" {|(after value "@")|} "mail.ru" ];
          fn "before-last" [ text "text"; text "separator" ] (Of Text) "As before, at the last SEPARATOR."
            [ on Text "a.b.c" {|(before-last value ".")|} "a.b" ];
          fn "after-last" [ text "text"; text "separator" ] (Of Text) "As after, at the last SEPARATOR."
            [ on Text "a.b.c" {|(after-last value ".")|} "c" ];
          fn "split-part" [ text "text"; text "separator"; int "n" ] (Of Text)
            "The N-th field between SEPARATORs, from 1; empty past the last."
            [ on Text "a,b,c" {|(split-part value "," 2)|} "b" ];
          fn "index-of" [ text "text"; text "part" ] (Of Int)
            "Where PART first starts, in characters from 0; -1 when it is absent, 0 when it is empty."
            [ on Text "Иван Петров" {|(index-of value "Пе")|} "5" ];
          fn "pad-left" [ text "text"; int "width"; text "fill" ] (Of Text)
            "FILL's first character put in front until the text is WIDTH characters long (at most 65535)."
            [ on Text "42" {|(pad-left value 5 "0")|} "00042" ];
          fn "pad-right" [ text "text"; int "width"; text "fill" ] (Of Text) "As pad-left, at the end."
            [ on Text "42" {|(pad-right value 5 ".")|} "42..." ];
          fn "repeat" [ text "text"; int "n" ] (Of Text) "N copies, at most 65535; empty past one mebibyte."
            [ alone {|(repeat "ab" 3)|} "ababab" ];
          fn ~arity:(Repeated_last 0) "format" [ text "template"; text "text" ] (Of Text)
            "Each {} of TEMPLATE replaced by the next text, in order; the {} left over stay."
            [ alone {|(format "{}-{}" "a" "b")|} "a-b" ];
          fn "translit" [ text "text" ] (Of Text) "Cyrillic written in Latin letters, the rest kept."
            [ on Text "Щука Ёжикова" "(translit value)" "Shchuka Yozhikova" ];
          fn "only-digits" [ text "text" ] (Of Text) "Only the digits 0 to 9."
            [ on Text "+7 (912) 345" "(only-digits value)" "7912345" ];
          fn "redact" [ text "text"; int "left"; int "right" ] (Of Text)
            "Each letter and digit but the first LEFT and the last RIGHT characters becomes *; spaces and \
             punctuation stay."
            [ on Text "Иван Петров" "(redact value 2 2)" "Ив** ****ов" ];
          fn "str" [ int "number" ] (Of Text) "An integer written in decimal." [ on Int "41" "(str (+ value 1))" "42" ];
          fn "int" [ text "text" ] (Of Int)
            "Decimal digits with an optional leading -, spaces around allowed, as an integer; NULL otherwise."
            [ on Text " 42 " "(int value)" "42" ];
          fn "starts-with" [ text "text"; text "prefix" ] (Of Bool) "Whether the text starts with PREFIX."
            [ on Text "+7 912" {|(starts-with value "+7")|} "true" ];
          fn "ends-with" [ text "text"; text "suffix" ] (Of Bool) "Whether the text ends with SUFFIX."
            [ on Text "a@mail.ru" {|(ends-with value ".ru")|} "true" ];
          fn "contains" [ text "text"; text "part" ] (Of Bool) "Whether PART is in the text."
            [ on Text "a@b" {|(contains value "@")|} "true" ];
          fn "like" [ text "text"; text "pattern" ] (Of Bool)
            "SQL LIKE, with case: % any characters, _ one, a backslash makes the next one plain."
            [ on Text "a@mail.ru" {|(like value "%@%.ru")|} "true" ];
          fn "luhn" [ text "digits" ] (Of Text) "The Luhn check digit of the text's digits, which card numbers end with."
            [ on Text "7992739871" "(luhn value)" "3" ];
        ];
    };
    {
      title = "Numbers";
      doc = "Integers of 64 bits. A function given NULL gives NULL.";
      entries =
        [
          fn ~arity:(Repeated_last 1) "+" [ int "a"; int "b" ] (Of Int) "The sum." [ on Int "41" "(+ value 1)" "42" ];
          fn ~arity:Optional_last "-" [ int "a"; int "b" ] (Of Int) "A minus B, or minus A alone."
            [ on Int "49" "(- value 7)" "42"; on Int "42" "(- value)" "-42" ];
          fn ~arity:(Repeated_last 1) "*" [ int "a"; int "b" ] (Of Int) "The product." [ on Int "21" "(* value 2)" "42" ];
          fn "/" [ int "a"; int "b" ] (Of Int) "The quotient, toward zero; NULL when B is 0."
            [ on Int "85" "(/ value 2)" "42"; on Int "85" "(/ value 0)" "\\N" ];
          fn "mod" [ int "a"; int "b" ] (Of Int) "The remainder, with A's sign; NULL when B is 0."
            [ on Int "142" "(mod value 10)" "2" ];
          fn "abs" [ int "a" ] (Of Int) "Without its sign." [ on Int "-42" "(abs value)" "42" ];
          fn ~arity:(Repeated_last 1) "min" [ same "value" ] Same "The least of values of one kind."
            [ on Int "42" "(min value 100)" "42" ];
          fn ~arity:(Repeated_last 1) "max" [ same "value" ] Same "The greatest of values of one kind."
            [ on Int "-5" "(max value 0)" "0" ];
          fn "round-to" [ int "n"; int "step" ] (Of Int)
            "To the nearest multiple of STEP, halves away from zero; NULL when STEP is not positive."
            [ on Int "123456" "(round-to value 1000)" "123000" ];
          fn "floor-to" [ int "n"; int "step" ] (Of Int) "Down to a multiple of STEP." [ on Int "-1" "(floor-to value 10)" "-10" ];
          fn "clamp" [ int "n"; int "low"; int "high" ] (Of Int) "Kept between LOW and HIGH; NULL when LOW is above HIGH."
            [ on Int "150" "(clamp value 0 100)" "100" ];
        ];
    };
    {
      title = "Dates and times";
      doc =
        "A DATE column is a date, DATETIME and TIMESTAMP a date with a time. Years run from 0000 to 9999; \
         what would leave them is NULL. A time finer than its column stops the copy.";
      entries =
        [
          fn "year" [ date "date" ] (Of Int) "The year." [ on Date "1985-07-14" "(year value)" "1985" ];
          fn "month" [ date "date" ] (Of Int) "The month, from 1." [ on Date "1985-07-14" "(month value)" "7" ];
          fn "day" [ date "date" ] (Of Int) "The day of the month." [ on Date "1985-07-14" "(day value)" "14" ];
          fn "make-date" [ int "year"; int "month"; int "day" ] (Of Date) "The date; NULL for a day that does not exist."
            [ alone "(make-date 2024 2 29)" "2024-02-29"; alone "(make-date 2023 2 29)" "\\N" ];
          fn "add-days" [ date "date"; int "days" ] (Of Date) "So many days later, or earlier when negative."
            [ on Date "2000-02-28" "(add-days value 1)" "2000-02-29" ];
          fn "days-between" [ date "from"; date "to" ] (Of Int) "Days from FROM to TO."
            [ on Date "2000-01-01" "(days-between value (make-date 2000 3 1))" "60" ];
          fn "date->text" [ date "date" ] (Of Text) "YYYY-MM-DD." [ on Date "1985-07-14" "(date->text value)" "1985-07-14" ];
          fn "text->date" [ text "text" ] (Of Date) "A date from YYYY-MM-DD; NULL otherwise."
            [ on Text "1985-07-14" "(text->date value)" "1985-07-14" ];
          fn "date-of" [ datetime "time" ] (Of Date) "The day of a time."
            [ on Datetime "2024-03-01 13:45:10" "(date-of value)" "2024-03-01" ];
          fn "seconds-of-day" [ datetime "time" ] (Of Int) "Seconds since that day's midnight."
            [ on Datetime "2024-03-01 13:45:10" "(seconds-of-day value)" "49510" ];
          fn "make-datetime" [ date "date"; int "seconds" ] (Of Datetime)
            "That day, SECONDS after midnight, from 0 to 86399."
            [ on Date "2024-03-01" "(make-datetime value 3600)" "2024-03-01 01:00:00" ];
          fn "add-seconds" [ datetime "time"; int "seconds" ] (Of Datetime) "So many seconds later."
            [ on Datetime "2024-03-01 23:59:00" "(add-seconds value 90)" "2024-03-02 00:00:30" ];
          fn "datetime->text" [ datetime "time" ] (Of Text) "YYYY-MM-DD HH:MM:SS, then the fraction of a second if any."
            [ on Datetime "2024-03-01 13:45:10" "(datetime->text value)" "2024-03-01 13:45:10" ];
          fn "text->datetime" [ text "text" ] (Of Datetime)
            "A time from YYYY-MM-DD, HH:MM:SS and up to six fractional digits; NULL otherwise."
            [ on Text "2024-03-01 13:45:10.5" "(text->datetime value)" "2024-03-01 13:45:10.500000" ];
        ];
    };
    {
      title = "Logic";
      doc = "NULL compares to nothing; and, or and if take it as false.";
      entries =
        [
          fn "=" [ same "a"; same "b" ] (Of Bool) "Whether A equals B." [ on Text "a" {|(= value "a")|} "true" ];
          fn "<>" [ same "a"; same "b" ] (Of Bool) "Whether A differs from B." [ on Int "1" "(<> value 2)" "true" ];
          fn "<" [ same "a"; same "b" ] (Of Bool) "Whether A comes before B: text by bytes, numbers and dates by value."
            [ on Int "3" "(< value 3)" "false" ];
          fn "<=" [ same "a"; same "b" ] (Of Bool) "Whether A comes before B or equals it." [ on Int "3" "(<= value 3)" "true" ];
          fn ">" [ same "a"; same "b" ] (Of Bool) "Whether A comes after B." [ on Text "b" {|(> value "a")|} "true" ];
          fn ">=" [ same "a"; same "b" ] (Of Bool) "Whether A comes after B or equals it." [ on Int "2" "(>= value 3)" "false" ];
          fn ~arity:(Repeated_last 1) "and" [ truth "truth" ] (Of Bool) "Whether all are true."
            [ on Int "5" "(and (> value 0) (< value 10))" "true" ];
          fn ~arity:(Repeated_last 1) "or" [ truth "truth" ] (Of Bool) "Whether one is true."
            [ on Int "50" "(or (< value 0) (> value 10))" "true" ];
          fn "not" [ truth "truth" ] (Of Bool) "The opposite." [ on Text "x" "(not (is-null value))" "true" ];
          fn "is-null" [ same "value" ] (Of Bool) "Whether the value is NULL." [ { expression = "(is-null value)"; given = None; gives = "true" } ];
          fn ~arity:(Repeated_last 1) "coalesce" [ same "value" ] Same "The first value that is not NULL."
            [ alone {|(coalesce value "none")|} "none" ];
        ];
    };
    {
      title = "Keyed";
      doc =
        "Keyed by --mask-secret with HMAC-SHA256: under one key a seed always gives the same result, \
         in every column and every run, so masked keys still join; without the key the result tells \
         nothing of the seed. The examples are under the key of the secret \"example\".";
      entries =
        [
          fn "shape" [ text "text" ] (Of Text)
            "Each letter and digit replaced by one of its kind (Latin, Cyrillic, digit), any other letter by a \
             Latin one, the rest kept: as many characters, never more bytes."
            [ on Text "+7 (912) 345-67-89" "(shape value)" "+9 (331) 996-11-65" ];
          fn "email" [ text "text" ] (Of Text) "shape before the last @, the domain kept."
            [ on Text "anna.petrova@mail.ru" "(email value)" "utlr.icmqljt@mail.ru" ];
          fn "hash" [ text "text" ] (Of Text) "As many hex digits as the text has characters." [ on Text "Иван" "(hash value)" "39b4" ];
          fn "hash-bytes" [ text "bytes" ] (Of Text) "As many bytes as the value: for binary columns."
            [ on Text "abc" "(length (hash-bytes value))" "3" ];
          fn "hash-int" [ int "number" ] (Of Int) "Any 64-bit integer." [ on Int "42" "(hash-int value)" "-7709159480299872343" ];
          fn "keyed-int" [ text "seed"; int "low"; int "high" ] (Of Int) "An integer from LOW to HIGH."
            [ on Text "Иван" "(keyed-int value 18 90)" "73" ];
          fn "keyed-date" [ text "seed"; date "from"; date "to" ] (Of Date) "A date from FROM to TO."
            [ on Text "id-1" "(keyed-date value (make-date 1985 1 1) (make-date 1985 12 31))" "1985-03-29" ];
          fn "keyed-noise" [ int "n"; int "percent" ] (Of Int) "N moved by up to PERCENT of itself either way, PERCENT from 0 to 100."
            [ on Int "100000" "(keyed-noise value 10)" "92386" ];
          fn "keyed-uuid" [ text "seed" ] (Of Text) "A version 4 UUID." [ on Text "a" "(keyed-uuid value)" "33e6f143-b8f1-4bcc-ba78-9920cc303b7f" ];
          fn "digits" [ text "seed"; int "count" ] (Of Text) "COUNT decimal digits, at most 65535." [ on Text "Иван" "(digits value 4)" "1463" ];
          fn ~arity:(Repeated_last 1) "pick" [ text "seed"; same "choice" ] Same "One of the choices."
            [ on Text "x" {|(pick value "Анна" "Иван" "Олег")|} "Иван" ];
          fn "pick-word" [ text "seed"; p "words" Words ] (Of Text)
            "One line of a (words NAME \"FILE\") list, named here without quotes." [];
        ];
    };
  ]

let builtins = List.concat_map (fun { entries; _ } -> List.map (fun { name; _ } -> name) entries) reference

let arity at name expected given = Error (Arity { at; name; expected; given })

let rec expression_of form =
  match form with
  | Quoted { at; text } -> Ok (Literal { at; value = Text text })
  | Number { at; value } -> Ok (Literal { at; value = Int value })
  | Symbol { at; name = "true" } -> Ok (Literal { at; value = Bool true })
  | Symbol { at; name = "false" } -> Ok (Literal { at; value = Bool false })
  | Symbol { at; name = "null" } -> Ok (Literal { at; value = Null })
  | Symbol { at; name = "value" } -> Ok (Current { at })
  | Symbol { at; name } when List.mem name reserved -> Error (Misplaced { at; form = name })
  | Symbol { at; name } -> Ok (Name { at; name })
  | Items { at; items = [] } -> Error (Syntax { at; reason = "an empty list" })
  | Items { at; items = Symbol { name = "if"; _ } :: rest } -> (
      match rest with
      | [ condition; yes; no ] ->
          let%bind condition = expression_of condition in
          let%bind yes = expression_of yes in
          let%map no = expression_of no in
          If { at; condition; yes; no }
      | _ -> arity at "if" "3" (List.length rest))
  | Items { at; items = Symbol { name = "cond"; _ } :: clauses } ->
      let rec chain = function
        | [ Items { items = [ Symbol { name = "else"; _ }; otherwise ]; _ } ] -> expression_of otherwise
        | Items { at; items = [ condition; yes ] } :: rest ->
            let%bind condition = expression_of condition in
            let%bind yes = expression_of yes in
            let%map no = chain rest in
            If { at; condition; yes; no }
        | [] -> Error (Syntax { at; reason = "cond ends without (else …)" })
        | clause :: _ ->
            Error (Syntax { at = position_of_form clause; reason = "a cond clause is (CONDITION EXPRESSION)" })
      in
      chain clauses
  | Items { at; items = Symbol { name = "let"; _ } :: rest } -> (
      match rest with
      | [ Items { items = bindings; _ }; body ] ->
          let%bind bindings =
            Base.Result.all
              (List.map
                 (function
                   | Items { items = [ Symbol { at; name }; bound ]; _ }
                     when not (List.mem name reserved || List.mem name builtins) ->
                       let%map bound = expression_of bound in
                       (at, name, bound)
                   | binding ->
                       Error (Syntax { at = position_of_form binding; reason = "a let binding is (NAME EXPRESSION)" }))
                 bindings)
          in
          let%map body = expression_of body in
          Let { at; bindings; body }
      | _ -> Error (Syntax { at; reason = "let is (let ((NAME EXPRESSION) …) BODY)" }))
  | Items { at; items = Symbol { name = "row"; _ } :: rest } -> (
      match rest with
      | [ (Symbol { name = column; _ } | Quoted { text = column; _ }) ] -> Ok (Row { at; column })
      | _ -> Error (Syntax { at; reason = "row is (row COLUMN)" }))
  | Items { at; items = Symbol { name; _ } :: _ } when List.mem name reserved -> Error (Misplaced { at; form = name })
  | Items { at; items = Symbol { name; _ } :: arguments } ->
      let%map arguments = Base.Result.all (List.map expression_of arguments) in
      Call { at; name; arguments }
  | Items { at; items = _ :: _ } -> Error (Syntax { at; reason = "a call starts with a function's name" })

and position_of_form = function
  | Symbol { at; _ } | Quoted { at; _ } | Number { at; _ } | Items { at; _ } -> at

type definition = { name : string; parameters : string list; body : expression }
type selector = Named of string | Where of { table : string; column : string; types : string list }
type target = Masked of expression | Kept | Permuted of (int64 * int64) option
type rule = { at : position; selector : selector; target : target }

type t = {
  definitions : definition list;
  rules : rule list;
  strict : (position * string) list;
  after_load : (position * string * string) list;
  dictionaries : (string * string iarray) list;
  references : (position * (string * string * string) * (string * string * string)) list;
}

let empty = { definitions = []; rules = []; strict = []; after_load = []; dictionaries = []; references = [] }

let selector_of form =
  match form with
  | Quoted { text; _ } | Symbol { name = text; _ } when not (List.mem text reserved) -> Ok (Named text)
  | Items { items = Symbol { name = "where"; _ } :: clauses; _ } ->
      Base.List.fold_result clauses ~init:("*.*", "*", []) ~f:(fun (table, column, types) clause ->
          match clause with
          | Items { items = [ Symbol { name = "table"; _ }; (Quoted { text; _ } | Symbol { name = text; _ }) ]; _ } ->
              Ok (text, column, types)
          | Items { items = [ Symbol { name = "column"; _ }; (Quoted { text; _ } | Symbol { name = text; _ }) ]; _ } ->
              Ok (table, text, types)
          | Items { items = Symbol { name = "type"; _ } :: (_ :: _ as names); _ } ->
              let%map named =
                Base.Result.all
                  (List.map
                     (function
                       | Symbol { name; _ } when List.mem name column_types -> Ok name
                       | Symbol { at; name } -> Error (Unknown_type { at; name })
                       | other -> Error (Syntax { at = position_of_form other; reason = "a type is a plain name" }))
                     names)
              in
              (table, column, types @ named)
          | other ->
              Error
                (Syntax
                   {
                     at = position_of_form other;
                     reason = "a where clause is (table \"DB.TABLE\"), (column \"NAME\") or (type NAME …)";
                   }))
      |> Result.map (fun (table, column, types) -> Where { table; column; types })
  | other ->
      Error
        (Syntax
           {
             at = position_of_form other;
             reason = "a selector is \"DB.TABLE.COLUMN\" or (where (table …) (column …) (type …))";
           })

let parse ~source ?(within = empty) ~load text =
  Result.map_error
    (fun error -> `Mask_script error)
    (let%bind forms = read ~source text in
     Base.List.fold_result forms ~init:within ~f:(fun script form ->
         match form with
         | Items
             {
               items =
                 [ Symbol { name = "define"; _ }; Items { items = Symbol { at; name } :: parameters; _ }; body ];
               _;
             } ->
             let%bind () =
               if
                 List.mem name reserved || List.mem name builtins
                 || List.exists (fun (definition : definition) -> String.equal definition.name name) script.definitions
               then Error (Defined_twice { at; name })
               else Ok ()
             in
             let%bind parameters =
               Base.Result.all
                 (List.map
                    (function
                      | Symbol { name; _ } when not (List.mem name reserved || List.mem name builtins) -> Ok name
                      | parameter ->
                          Error (Syntax { at = position_of_form parameter; reason = "a parameter is a plain name" }))
                    parameters)
             in
             let%bind () =
               match Base.List.find_a_dup parameters ~compare:String.compare with
               | Some name -> Error (Defined_twice { at; name })
               | None -> Ok ()
             in
             let%map body = expression_of body in
             { script with definitions = script.definitions @ [ { name; parameters; body } ] }
         | Items { at; items = [ Symbol { name = "mask"; _ }; selector; body ] } ->
             let%bind selector = selector_of selector in
             let%map body = expression_of body in
             { script with rules = script.rules @ [ { at; selector; target = Masked body } ] }
         | Items { at; items = [ Symbol { name = "keep"; _ }; selector ] } ->
             let%map selector = selector_of selector in
             { script with rules = script.rules @ [ { at; selector; target = Kept } ] }
         | Items { at; items = [ Symbol { name = "permute"; _ }; selector ] } ->
             let%map selector = selector_of selector in
             { script with rules = script.rules @ [ { at; selector; target = Permuted None } ] }
         | Items { at; items = [ Symbol { name = "permute"; _ }; selector; Number { value = low; _ }; Number { value = high; _ } ] }
           ->
             let%bind selector = selector_of selector in
             if Int64.compare low high > 0 then Error (Syntax { at; reason = "a permutation's LOW is above its HIGH" })
             else Ok { script with rules = script.rules @ [ { at; selector; target = Permuted (Some (low, high)) } ] }
         | Items
             {
               at;
               items =
                 [ Symbol { name = "references"; _ }; (Quoted { text = child; _ } | Symbol { name = child; _ });
                   (Quoted { text = parent; _ } | Symbol { name = parent; _ }) ];
             } -> (
             let column text =
               match String.split_on_char '.' text with
               | [ database; table; column ] when not (List.mem "" [ database; table; column ]) -> Some (database, table, column)
               | _ -> None
             in
             match (column child, column parent) with
             | Some child, Some parent -> Ok { script with references = script.references @ [ (at, child, parent) ] }
             | _ -> Error (Syntax { at; reason = "references names two columns: \"DB.TABLE.COLUMN\" \"DB.TABLE.COLUMN\"" }))
         | Items { at; items = Symbol { name = "strict"; _ } :: (_ :: _ as tables) } ->
             let%map tables =
               Base.Result.all
                 (List.map
                    (function
                      | Quoted { text; _ } | Symbol { name = text; _ } -> Ok (at, text)
                      | other -> Error (Syntax { at = position_of_form other; reason = "strict names tables: \"DB.TABLE\"" }))
                    tables)
             in
             { script with strict = script.strict @ tables }
         | Items { at; items = [ Symbol { name = "after-load"; _ }; (Quoted { text = table; _ } | Symbol { name = table; _ }); Quoted { text = statement; _ } ] } ->
             Ok { script with after_load = script.after_load @ [ (at, table, statement) ] }
         | Items { at; items = [ Symbol { name = "words"; _ }; Symbol { name; _ }; Quoted { text = path; _ } ] } -> (
             let%bind () =
               if List.mem_assoc name script.dictionaries || List.mem name reserved || List.mem name builtins then
                 Error (Defined_twice { at; name })
               else Ok ()
             in
             match load path with
             | Error reason -> Error (Unloadable { at; path; reason })
             | Ok text -> (
                 let words =
                   List.filter
                     (fun line -> (not (String.equal line "")) && not (Char.equal line.[0] '#'))
                     (List.map String.trim (String.split_on_char '\n' text))
                 in
                 match words with
                 | [] -> Error (Unloadable { at; path; reason = "no words in it" })
                 | _ :: _ -> Ok { script with dictionaries = script.dictionaries @ [ (name, Iarray.of_list words) ] }))
         | Items
             {
               at;
               items =
                 Symbol { name = ("define" | "mask" | "keep" | "strict" | "after-load" | "words" | "permute" | "references") as name; _ }
                 :: _;
             } ->
             Error
               (Syntax
                  {
                    at;
                    reason =
                      name
                      ^ " is one of (define (NAME PARAMETER …) BODY), (mask SELECTOR BODY), (keep SELECTOR), \
                         (permute SELECTOR [LOW HIGH]), (references \"DB.TABLE.COLUMN\" \"DB.TABLE.COLUMN\"), \
                         (strict \"DB.TABLE\" …), (after-load \"DB.TABLE\" \"STATEMENT\"), (words NAME \"FILE\")";
                  })
         | Items { at; items = Symbol { name; _ } :: _ } -> Error (Misplaced { at; form = name })
         | form -> Error (Misplaced { at = position_of_form form; form = "an expression" })))

let expression ~source text =
  let parsed =
    match%bind read ~source text with
    | [ form ] -> expression_of form
    | [] -> Error (Syntax { at = { source; line = 1; column = 1 }; reason = "an expression is missing" })
    | _ :: second :: _ -> Error (Syntax { at = position_of_form second; reason = "one expression, not several" })
  in
  Result.map_error (fun error -> `Mask_script error) parsed

let rules { rules; _ } = rules
let strict { strict; _ } = strict
let after_load { after_load; _ } = after_load
let references { references; _ } = List.map (fun (_, child, parent) -> (child, parent)) references

(** What running a compiled expression sees. *)
type frame = { key : Keyed.t; current : value; row : value array; locals : value array }

(** Typed code: the kind it gives, [None] for a NULL alone. *)
type code = { at : position; kind : kind option; eval : frame -> value @@ portable }

(** A local's slot and the code that sets it. *)
type binding = { index : int; bound : frame -> value @@ portable }

type compiled = { reads : string list; slots : int; code : frame -> value @@ portable }

let reads { reads; _ } = reads

let run { slots; code; _ } key ~value ~row =
  code { key; current = value; row; locals = Array.make slots Null }

let micros_per_day = 86_400_000_000L

let floor_div a b =
  let q = Int64.div a b in
  if Int64.compare (Int64.rem a b) 0L < 0 then Int64.pred q else q

let civil days =
  let z = days + 719468 in
  let shifted = if z >= 0 then z else z - 146096 in
  let era = shifted / 146097 in
  let doe = z - (era * 146097) in
  let yoe = (doe - (doe / 1460) + (doe / 36524) - (doe / 146096)) / 365 in
  let doy = doe - ((365 * yoe) + (yoe / 4) - (yoe / 100)) in
  let mp = ((5 * doy) + 2) / 153 in
  let day = doy - (((153 * mp) + 2) / 5) + 1 in
  let month = if mp < 10 then mp + 3 else mp - 9 in
  let year = yoe + (era * 400) + if month <= 2 then 1 else 0 in
  (year, month, day)

let date_of ~year ~month ~day : value =
  match Temporal.days_of_date ~year ~month ~day with Null -> Null | This days -> Date days

let date_text days =
  let year, month, day = civil days in
  Printf.sprintf "%04d-%02d-%02d" year month day

let numbers text =
  Base.Option.all
    (List.map (fun part -> if part <> "" && String.for_all (fun c -> c >= '0' && c <= '9') part then int_of_string_opt part else None)
       text)

let parsed_date text : value =
  match numbers (String.split_on_char '-' text) with
  | Some [ year; month; day ] when String.length text = 10 -> date_of ~year ~month ~day
  | Some _ | None -> Null

let datetime_text x =
  let day = floor_div x micros_per_day in
  let within = Int64.to_int (Int64.sub x (Int64.mul day micros_per_day)) in
  let seconds = within / 1_000_000 and fraction = within mod 1_000_000 in
  let clock = Printf.sprintf "%02d:%02d:%02d" (seconds / 3600) (seconds / 60 mod 60) (seconds mod 60) in
  let fraction = if fraction = 0 then "" else Printf.sprintf ".%06d" fraction in
  date_text (Int64.to_int day) ^ " " ^ clock ^ fraction

let parsed_datetime text : value =
  let text = String.trim text in
  let date, clock =
    match String.index_opt text ' ' with
    | Some at -> (String.sub text 0 at, String.sub text (at + 1) (String.length text - at - 1))
    | None -> (text, "00:00:00")
  in
  let seconds, fraction =
    match String.index_opt clock '.' with
    | Some at -> (String.sub clock 0 at, String.sub clock (at + 1) (String.length clock - at - 1))
    | None -> (clock, "")
  in
  let micros =
    if String.length fraction > 6 then None
    else if String.equal fraction "" then Some 0
    else
      Option.map
        (fun f -> f * int_of_float (10. ** float_of_int (6 - String.length fraction)))
        (Option.bind (numbers [ fraction ]) (function [ f ] -> Some f | _ -> None))
  in
  match (parsed_date date, numbers (String.split_on_char ':' seconds), micros) with
  | Date d, Some [ hours; minutes; secs ], Some micros
    when String.length seconds = 8 && hours < 24 && minutes < 60 && secs < 60 ->
      Datetime
        (Int64.add
           (Int64.mul (Int64.of_int d) micros_per_day)
           (Int64.of_int ((((((hours * 60) + minutes) * 60) + secs) * 1_000_000) + micros)))
  | _ -> Null

let shown = function
  | Null -> "\\N"
  | Text text -> text
  | Int number -> Int64.to_string number
  | Bool truth -> Bool.to_string truth
  | Date days -> date_text days
  | Datetime micros -> datetime_text micros

let read_value (kind : kind) text : value =
  match kind with
  | Text -> Text text
  | Int | Bool -> Option.fold (Int64.of_string_opt text) ~none:Null ~some:(fun number -> Int number)
  | Date -> parsed_date (String.trim text)
  | Datetime -> parsed_datetime text

let compiled script expression ~value ~columns ~result =
  let position_of = function
    | Literal { at; _ } | Current { at } | Row { at; _ } | Name { at; _ } | If { at; _ } | Let { at; _ }
    | Call { at; _ } ->
        at
  in
  let cased case text =
    let out = Buffer.create (String.length text) in
    let rec go pos =
      if pos < String.length text then (
        let width, code = Utf8.decode text pos in
        let mapped =
          match case with
          | `Upper ->
              if code >= Char.code 'a' && code <= Char.code 'z' then code - 32
              else if code >= 0x430 && code <= 0x44F then code - 32
              else if code = 0x451 then 0x401
              else code
          | `Lower ->
              if code >= Char.code 'A' && code <= Char.code 'Z' then code + 32
              else if code >= 0x410 && code <= 0x42F then code + 32
              else if code = 0x401 then 0x451
              else code
        in
        let () =
          if mapped < 0 then Buffer.add_string out (String.sub text pos width)
          else Buffer.add_utf_8_uchar out (Uchar.of_int mapped)
        in
        go (pos + width))
    in
    go 0;
    Buffer.contents out
  in
  let find_first text pattern =
    if String.equal pattern "" then None else Base.String.substr_index text ~pattern
  in
  let find_last text pattern =
    let n = String.length text and m = String.length pattern in
    let rec back i =
      if i < 0 then None else if String.equal (String.sub text i m) pattern then Some i else back (i - 1)
    in
    if m = 0 || m > n then None else back (n - m)
  in
  let widest = 65535 in
  let micros_per_second = 1_000_000L in
  let within_years days : value =
    let year, _, _ = civil days in
    if year >= 0 && year <= 9999 then Date days else Null
  in
  let order a b =
    match (a, b) with
    | Text x, Text y -> String.compare x y
    | Int x, Int y | Datetime x, Datetime y -> Int64.compare x y
    | Date x, Date y -> Int.compare x y
    | Bool x, Bool y -> Bool.compare x y
    | _ -> 0
  in
  let points text =
    let rec go pos acc =
      if pos >= String.length text then List.rev acc
      else
        let width, code = Utf8.decode text pos in
        go (pos + width) ((String.sub text pos width, code) :: acc)
    in
    go 0 []
  in
  let letter code =
    (code >= Char.code 'a' && code <= Char.code 'z')
    || (code >= Char.code 'A' && code <= Char.code 'Z')
    || code >= 0x80
  in
  let slots = ref 0 and reads = ref [] and steps = ref 0 in
  let step at =
    incr steps;
    if !steps > 100_000 then Error (Too_large { at }) else Ok ()
  in
  let slot () =
    let taken = !slots in
    incr slots;
    taken
  in
  let read column =
    let lower = String.lowercase_ascii column in
    match Base.List.findi !reads ~f:(fun _ name -> String.equal (String.lowercase_ascii name) lower) with
    | Some (i, _) -> i
    | None ->
        reads := !reads @ [ column ];
        List.length !reads - 1
  in
  let expect expected (code : code) =
    match code.kind with
    | None -> Ok ()
    | Some kind when kind = expected -> Ok ()
    | Some found -> Error (Mistyped { at = code.at; expected; found })
  in
  let unify (codes : code list) =
    Base.List.fold_result codes ~init:None ~f:(fun known (code : code) ->
        match (known, code.kind) with
        | None, kind | kind, None -> Ok kind
        | Some expected, Some found when expected = found -> Ok known
        | Some expected, Some found -> Error (Mistyped { at = code.at; expected; found }))
  in
  let rec compile ~(locals : (string * (int * kind option)) list) ~(visible : definition list) expression =
    let at = position_of expression in
    let%bind () = step at in
    match expression with
    | Literal { value; _ } ->
        let kind : kind option =
          match value with
          | Null -> None
          | Text _ -> Some Text
          | Int _ -> Some Int
          | Bool _ -> Some Bool
          | Date _ -> Some Date
          | Datetime _ -> Some Datetime
        in
        Ok { at; kind; eval = (fun _ -> value) }
    | Current _ -> Ok { at; kind = Some value; eval = (fun frame -> frame.current) }
    | Row { column; _ } ->
        Option.fold (columns column)
          ~none:(Error (Unreadable_column { at; column }))
          ~some:(fun kind ->
            let index = read column in
            Ok { at; kind = Some kind; eval = (fun frame -> frame.row.(index)) })
    | Name { name; _ } ->
        Option.fold (List.assoc_opt name locals)
          ~none:(Error (Unknown { at; name }))
          ~some:(fun ((index : int), kind) -> Ok { at; kind; eval = (fun frame -> frame.locals.(index)) })
    | If { condition; yes; no; _ } ->
        let%bind condition = compile ~locals ~visible condition in
        let%bind () = expect Bool condition in
        let%bind yes = compile ~locals ~visible yes in
        let%bind no = compile ~locals ~visible no in
        let%map kind = unify [ yes; no ] in
        let test = condition.eval and yes = yes.eval and no = no.eval in
        { at; kind; eval = (fun frame -> match test frame with Bool true -> yes frame | _ -> no frame) }
    | Let { bindings; body; _ } ->
        let%bind locals, sets =
          Base.List.fold_result bindings ~init:(locals, []) ~f:(fun (locals, sets) (_, name, bound) ->
              let%map code = compile ~locals ~visible bound in
              let index = slot () in
              ((name, (index, code.kind)) :: locals, { index; bound = code.eval } :: sets))
        in
        let%map body = compile ~locals ~visible body in
        let sets = List.rev sets and eval = body.eval in
        {
          at;
          kind = body.kind;
          eval =
            (fun frame ->
              List.iter (fun { index; bound } -> frame.locals.(index) <- bound frame) sets;
              eval frame);
        }
    | Call { name = "pick-word"; arguments = [ seed; Name { name = dictionary; at = named } ]; _ } -> (
        let%bind seed = compile ~locals ~visible seed in
        let%bind () = expect Text seed in
        match List.assoc_opt dictionary script.dictionaries with
        | None -> Error (Unknown { at = named; name = dictionary })
        | Some words ->
            let seed = seed.eval and count = Iarray.length words in
            Ok
              {
                at;
                kind = Some Text;
                eval =
                  (fun frame ->
                    match seed frame with
                    | Text seed -> Text (Iarray.get words (Keyed.choice frame.key seed count))
                    | _ -> Null);
              })
    | Call { name = "pick-word"; arguments; _ } ->
        arity at "pick-word" "2 (a seed and the name of words)" (List.length arguments)
    | Call { name; arguments; _ } -> (
        match Base.List.findi visible ~f:(fun _ (definition : definition) -> String.equal definition.name name) with
        | Some (place, definition) ->
            let given = List.length arguments and expected = List.length definition.parameters in
            if given <> expected then arity at name (Int.to_string expected) given
            else
              let%bind arguments = Base.Result.all (List.map (compile ~locals ~visible) arguments) in
              let indexes = List.map (fun _ -> slot ()) arguments in
              let%map body =
                compile
                  ~locals:
                    (List.map2
                       (fun parameter (index, (argument : code)) -> (parameter, (index, argument.kind)))
                       definition.parameters (List.combine indexes arguments))
                  ~visible:(Base.List.take visible place)
                  definition.body
              in
              let sets =
                List.map2 (fun index (argument : code) -> { index; bound = argument.eval }) indexes arguments
              and eval = body.eval in
              {
                at;
                kind = body.kind;
                eval =
                  (fun frame ->
                    List.iter (fun { index; bound } -> frame.locals.(index) <- bound frame) sets;
                    eval frame);
              }
        | None ->
            if List.mem name builtins then
              let%bind arguments = Base.Result.all (List.map (compile ~locals ~visible) arguments) in
              builtin at name arguments
            else if List.exists (fun (definition : definition) -> String.equal definition.name name) script.definitions
            then Error (Called_before_defined { at; name })
            else Error (Unknown { at; name }))
  and builtin at name (arguments : code list) =
    let given = List.length arguments in
    let typed kinds =
      if List.length kinds <> given then arity at name (Int.to_string (List.length kinds)) given
      else Base.Result.all_unit (List.map2 (fun kind argument -> expect kind argument) kinds arguments)
    in
    let all kind ~least =
      if given < least then arity at name (Printf.sprintf "%d or more" least) given
      else Base.Result.all_unit (List.map (expect kind) arguments)
    in
    let one () = (List.nth arguments 0).eval
    and two () = (List.nth arguments 1).eval
    and three () = (List.nth arguments 2).eval in
    let texts1 f =
      let%map () = typed [ Text ] in
      let a = one () in
      { at; kind = Some Text; eval = (fun frame -> match a frame with Text x -> Text (f frame.key x) | _ -> Null) }
    in
    let text_text kind f =
      let%map () = typed [ Text; Text ] in
      let a = one () and b = two () in
      { at; kind = Some kind; eval = (fun frame -> match (a frame, b frame) with Text x, Text y -> f x y | _ -> Null) }
    in
    let text_int f =
      let%map () = typed [ Text; Int ] in
      let a = one () and b = two () in
      {
        at;
        kind = Some Text;
        eval = (fun frame -> match (a frame, b frame) with Text x, Int n -> Text (f frame.key x n) | _ -> Null);
      }
    in
    let ints2 f =
      let%map () = typed [ Int; Int ] in
      let a = one () and b = two () in
      { at; kind = Some Int; eval = (fun frame -> match (a frame, b frame) with Int x, Int y -> f x y | _ -> Null) }
    in
    let fold_ints f =
      let%map () = all Int ~least:2 in
      {
        at;
        kind = Some Int;
        eval =
          (fun frame ->
            let rec go acc = function
              | [] -> Int acc
              | (code : code) :: rest -> ( match code.eval frame with Int x -> go (f acc x) rest | _ -> Null)
            in
            match arguments with
            | first :: rest -> ( match first.eval frame with Int x -> go x rest | _ -> Null)
            | [] -> Null);
      }
    in
    let compared test =
      let%bind () = if given = 2 then Ok () else arity at name "2" given in
      let%map (_ : kind option) = unify arguments in
      let a = one () and b = two () in
      let order x y =
        match (x, y) with
        | Text x, Text y -> Some (String.compare x y)
        | Int x, Int y | Datetime x, Datetime y -> Some (Int64.compare x y)
        | Bool x, Bool y -> Some (Bool.compare x y)
        | Date x, Date y -> Some (Int.compare x y)
        | _ -> None
      in
      {
        at;
        kind = Some Bool;
        eval =
          (fun frame ->
            Option.fold (order (a frame) (b frame)) ~none:Null ~some:(fun order -> Bool (test (Int.compare order 0))));
      }
    in
    let chosen pick =
      let%bind kind = unify arguments in
      let%map () = if given >= 1 then Ok () else arity at name "1 or more" given in
      {
        at;
        kind;
        eval =
          (fun frame ->
            let rec go best = function
              | [] -> best
              | (code : code) :: rest -> (
                  match (best, code.eval frame) with
                  | Null, _ | _, Null -> Null
                  | best, next -> go (pick best next) rest)
            in
            match arguments with first :: rest -> go (first.eval frame) rest | [] -> Null);
      }
    in
    let padded side =
      let%map () = typed [ Text; Int; Text ] in
      let a = one () and b = two () and c = three () in
      {
        at;
        kind = Some Text;
        eval =
          (fun frame ->
            match (a frame, b frame, c frame) with
            | Text text, Int width, Text fill ->
                let width = Int64.to_int (Int64.min (Int64.of_int widest) width) in
                let missing = width - Utf8.points text in
                if missing <= 0 || String.equal fill "" then Text text
                else
                  let fill = String.sub fill 0 (fst (Utf8.decode fill 0)) in
                  let padding = String.concat "" (List.init missing (fun _ -> fill)) in
                  let padded = match side with `Left -> padding ^ text | `Right -> text ^ padding in
                  Text padded
            | _ -> Null);
      }
    in
    match name with
    | "concat" ->
        let%map () = all Text ~least:1 in
        {
          at;
          kind = Some Text;
          eval =
            (fun frame ->
              let rec go acc = function
                | [] -> Text (String.concat "" (List.rev acc))
                | (code : code) :: rest -> ( match code.eval frame with Text x -> go (x :: acc) rest | _ -> Null)
              in
              go [] arguments);
        }
    | "length" ->
        let%map () = typed [ Text ] in
        let a = one () in
        { at; kind = Some Int; eval = (fun frame -> match a frame with Text x -> Int (Int64.of_int (Utf8.points x)) | _ -> Null) }
    | "substring" ->
        let%map () = typed [ Text; Int; Int ] in
        let a = one () and b = two () and c = three () in
        {
          at;
          kind = Some Text;
          eval =
            (fun frame ->
              match (a frame, b frame, c frame) with
              | Text x, Int start, Int count ->
                  let start = Int64.to_int (Int64.max 0L (Int64.min start (Int64.of_int max_int))) in
                  let count = Int64.to_int (Int64.max 0L (Int64.min count (Int64.of_int max_int))) in
                  let from = Utf8.offset x start in
                  let past = if count > max_int - start then max_int else start + count in
                  let until = Utf8.offset x past in
                  Text (String.sub x from (until - from))
              | _ -> Null);
        }
    | "left" ->
        text_int (fun _ x n ->
            String.sub x 0 (Utf8.offset x (Int64.to_int (Int64.max 0L (Int64.min n (Int64.of_int max_int))))))
    | "right" ->
        text_int (fun _ x n ->
            let total = Utf8.points x in
            let keep = Int64.to_int (Int64.max 0L (Int64.min n (Int64.of_int total))) in
            let from = Utf8.offset x (total - keep) in
            String.sub x from (String.length x - from))
    | "upper" -> texts1 (fun _ x -> cased `Upper x)
    | "lower" -> texts1 (fun _ x -> cased `Lower x)
    | "trim" -> texts1 (fun _ x -> String.trim x)
    | "replace" ->
        let%map () = typed [ Text; Text; Text ] in
        let a = one () and b = two () and c = three () in
        {
          at;
          kind = Some Text;
          eval =
            (fun frame ->
              match (a frame, b frame, c frame) with
              | Text x, Text "", Text _ -> Text x
              | Text x, Text pattern, Text with_ -> Text (Base.String.substr_replace_all x ~pattern ~with_)
              | _ -> Null);
        }
    | "before" ->
        text_text Text (fun x sep ->
            Text (Option.fold (find_first x sep) ~none:x ~some:(fun at -> String.sub x 0 at)))
    | "after" ->
        text_text Text (fun x sep ->
            Text
              (Option.fold (find_first x sep) ~none:"" ~some:(fun at ->
                   let from = at + String.length sep in
                   String.sub x from (String.length x - from))))
    | "before-last" ->
        text_text Text (fun x sep -> Text (Option.fold (find_last x sep) ~none:x ~some:(fun at -> String.sub x 0 at)))
    | "after-last" ->
        text_text Text (fun x sep ->
            Text
              (Option.fold (find_last x sep) ~none:"" ~some:(fun at ->
                   let from = at + String.length sep in
                   String.sub x from (String.length x - from))))
    | "pad-left" -> padded `Left
    | "pad-right" -> padded `Right
    | "str" ->
        let%map () = typed [ Int ] in
        let a = one () in
        { at; kind = Some Text; eval = (fun frame -> match a frame with Int x -> Text (Int64.to_string x) | _ -> Null) }
    | "int" ->
        let%map () = typed [ Text ] in
        let a = one () in
        {
          at;
          kind = Some Int;
          eval =
            (fun frame ->
              match a frame with
              | Text x ->
                  let x = String.trim x in
                  let digits = Base.String.chop_prefix_if_exists x ~prefix:"-" in
                  if (not (String.equal digits "")) && String.for_all (fun c -> c >= '0' && c <= '9') digits then
                    Option.fold (Int64.of_string_opt x) ~none:Null ~some:(fun x -> Int x)
                  else Null
              | _ -> Null);
        }
    | "starts-with" -> text_text Bool (fun x prefix -> Bool (String.starts_with ~prefix x))
    | "ends-with" -> text_text Bool (fun x suffix -> Bool (String.ends_with ~suffix x))
    | "contains" -> text_text Bool (fun x pattern -> Bool (Option.is_some (Base.String.substr_index x ~pattern)))
    | "shape" -> texts1 Keyed.shape
    | "email" -> texts1 Keyed.email
    | "hash" -> texts1 Keyed.hash_text
    | "hash-bytes" -> texts1 Keyed.hash_bytes
    | "hash-int" ->
        let%map () = typed [ Int ] in
        let a = one () in
        { at; kind = Some Int; eval = (fun frame -> match a frame with Int x -> Int (Keyed.hash_int64 frame.key x) | _ -> Null) }
    | "keyed-int" ->
        let%map () = typed [ Text; Int; Int ] in
        let a = one () and b = two () and c = three () in
        {
          at;
          kind = Some Int;
          eval =
            (fun frame ->
              match (a frame, b frame, c frame) with
              | Text seed, Int low, Int high -> Int (Keyed.within frame.key seed ~low ~high)
              | _ -> Null);
        }
    | "digits" -> text_int (fun key x n -> Keyed.digits key x (Int64.to_int (Int64.max 0L (Int64.min n (Int64.of_int widest)))))
    | "pick" -> (
        match arguments with
        | seed :: (_ :: _ as candidates) ->
            let%bind () = expect Text seed in
            let%map kind = unify candidates in
            let seed = seed.eval and count = List.length candidates in
            {
              at;
              kind;
              eval =
                (fun frame ->
                  match seed frame with
                  | Text seed -> (List.nth candidates (Keyed.choice frame.key seed count)).eval frame
                  | _ -> Null);
            }
        | _ -> arity at name "2 or more" given)
    | "+" -> fold_ints Int64.add
    | "*" -> fold_ints Int64.mul
    | "-" -> (
        match arguments with
        | [ { eval = a; _ } ] ->
            let%map () = typed [ Int ] in
            { at; kind = Some Int; eval = (fun frame -> match a frame with Int x -> Int (Int64.neg x) | _ -> Null) }
        | _ -> ints2 (fun x y -> Int (Int64.sub x y)))
    | "/" -> ints2 (fun x y -> if Int64.equal y 0L then Null else Int (Int64.div x y))
    | "mod" -> ints2 (fun x y -> if Int64.equal y 0L then Null else Int (Int64.rem x y))
    | "abs" ->
        let%map () = typed [ Int ] in
        let a = one () in
        { at; kind = Some Int; eval = (fun frame -> match a frame with Int x -> Int (Int64.abs x) | _ -> Null) }
    | "min" -> chosen (fun best next -> if order best next > 0 then next else best)
    | "max" -> chosen (fun best next -> if order best next < 0 then next else best)
    | "=" -> compared (fun order -> order = 0)
    | "<>" -> compared (fun order -> order <> 0)
    | "<" -> compared (fun order -> order < 0)
    | "<=" -> compared (fun order -> order <= 0)
    | ">" -> compared (fun order -> order > 0)
    | ">=" -> compared (fun order -> order >= 0)
    | "and" ->
        let%map () = all Bool ~least:1 in
        {
          at;
          kind = Some Bool;
          eval = (fun frame -> Bool (List.for_all (fun (code : code) -> code.eval frame = Bool true) arguments));
        }
    | "or" ->
        let%map () = all Bool ~least:1 in
        {
          at;
          kind = Some Bool;
          eval = (fun frame -> Bool (List.exists (fun (code : code) -> code.eval frame = Bool true) arguments));
        }
    | "not" ->
        let%map () = typed [ Bool ] in
        let a = one () in
        { at; kind = Some Bool; eval = (fun frame -> match a frame with Bool x -> Bool (not x) | _ -> Null) }
    | "is-null" ->
        let%map () = if given = 1 then Ok () else arity at name "1" given in
        let a = one () in
        { at; kind = Some Bool; eval = (fun frame -> match a frame with Null -> Bool true | _ -> Bool false) }
    | "coalesce" ->
        let%bind kind = unify arguments in
        let%map () = if given >= 1 then Ok () else arity at name "1 or more" given in
        {
          at;
          kind;
          eval =
            (fun frame ->
              let rec go = function
                | [] -> Null
                | (code : code) :: rest -> ( match code.eval frame with Null -> go rest | x -> x)
              in
              go arguments);
        }
    | "split-part" ->
        let%map () = typed [ Text; Text; Int ] in
        let a = one () and b = two () and c = three () in
        {
          at;
          kind = Some Text;
          eval =
            (fun frame ->
              match (a frame, b frame, c frame) with
              | Text x, Text sep, Int n ->
                  let rec split text acc =
                    match find_first text sep with
                    | None -> List.rev (text :: acc)
                    | Some at ->
                        let rest = at + String.length sep in
                        split (String.sub text rest (String.length text - rest)) (String.sub text 0 at :: acc)
                  in
                  let n = Int64.to_int (Int64.max 0L (Int64.min n 1_000_000L)) in
                  let field = if n < 1 then "" else Option.value (List.nth_opt (split x []) (n - 1)) ~default:"" in
                  Text field
              | _ -> Null);
        }
    | "index-of" ->
        text_text Int (fun x sub ->
            if String.equal sub "" then Int 0L
            else Option.fold (find_first x sub) ~none:(Int (-1L)) ~some:(fun at -> Int (Int64.of_int (Utf8.points (String.sub x 0 at)))))
    | "repeat" ->
        text_int (fun _ x n ->
            let n = Int64.to_int (Int64.max 0L (Int64.min n (Int64.of_int widest))) in
            if String.length x * n > 1_048_576 then "" else String.concat "" (List.init n (fun _ -> x)))
    | "reverse" -> texts1 (fun _ x -> String.concat "" (List.rev_map fst (points x)))
    | "title" ->
        texts1 (fun _ x ->
            let rec go previous acc = function
              | [] -> String.concat "" (List.rev acc)
              | (piece, code) :: rest ->
                  let case = if letter previous then `Lower else `Upper in
                  go code (cased case piece :: acc) rest
            in
            go (-1) [] (points x))
    | "translit" ->
        texts1 (fun _ x ->
            let latin code =
              match code with
              | 0x430 -> "a" | 0x431 -> "b" | 0x432 -> "v" | 0x433 -> "g" | 0x434 -> "d" | 0x435 -> "e"
              | 0x451 -> "yo" | 0x436 -> "zh" | 0x437 -> "z" | 0x438 -> "i" | 0x439 -> "y" | 0x43A -> "k"
              | 0x43B -> "l" | 0x43C -> "m" | 0x43D -> "n" | 0x43E -> "o" | 0x43F -> "p" | 0x440 -> "r"
              | 0x441 -> "s" | 0x442 -> "t" | 0x443 -> "u" | 0x444 -> "f" | 0x445 -> "kh" | 0x446 -> "ts"
              | 0x447 -> "ch" | 0x448 -> "sh" | 0x449 -> "shch" | 0x44A -> "" | 0x44B -> "y" | 0x44C -> ""
              | 0x44D -> "e" | 0x44E -> "yu" | 0x44F -> "ya" | _ -> ""
            in
            String.concat ""
              (List.map
                 (fun (piece, code) ->
                   if (code >= 0x430 && code <= 0x44F) || code = 0x451 then latin code
                   else if (code >= 0x410 && code <= 0x42F) || code = 0x401 then
                     let lower = if code = 0x401 then 0x451 else code + 32 in
                     String.capitalize_ascii (latin lower)
                   else piece)
                 (points x)))
    | "only-digits" -> texts1 (fun _ x -> String.of_seq (Seq.filter (fun c -> c >= '0' && c <= '9') (String.to_seq x)))
    | "redact" ->
        let%map () = typed [ Text; Int; Int ] in
        let a = one () and b = two () and c = three () in
        {
          at;
          kind = Some Text;
          eval =
            (fun frame ->
              match (a frame, b frame, c frame) with
              | Text x, Int left, Int right ->
                  let all = points x in
                  let total = List.length all in
                  let left = Int64.to_int (Int64.max 0L (Int64.min left (Int64.of_int total)))
                  and right = Int64.to_int (Int64.max 0L (Int64.min right (Int64.of_int total))) in
                  Text
                    (String.concat ""
                       (List.mapi
                          (fun i (piece, code) ->
                            if i < left || i >= total - right then piece
                            else if letter code || (code >= Char.code '0' && code <= Char.code '9') then "*"
                            else piece)
                          all))
              | _ -> Null);
        }
    | "like" ->
        text_text Bool (fun x pattern ->
            let text = Array.of_list (List.map fst (points x)) and wanted = Array.of_list (List.map fst (points pattern)) in
            let n = Array.length text and m = Array.length wanted in
            let rec fits i j =
              if j = m then i = n
              else
                match wanted.(j) with
                | "%" -> fits i (j + 1) || (i < n && fits (i + 1) j)
                | "_" -> i < n && fits (i + 1) (j + 1)
                | "\\" when j + 1 < m -> i < n && String.equal text.(i) wanted.(j + 1) && fits (i + 1) (j + 2)
                | piece -> i < n && String.equal text.(i) piece && fits (i + 1) (j + 1)
            in
            Bool (fits 0 0))
    | "format" ->
        let%map () = all Text ~least:1 in
        {
          at;
          kind = Some Text;
          eval =
            (fun frame ->
              let values = List.map (fun (code : code) -> code.eval frame) arguments in
              match values with
              | Text template :: rest when List.for_all (function Text _ -> true | _ -> false) rest ->
                  let rec fill text acc fillers =
                    match (find_first text "{}", fillers) with
                    | Some at, Text filler :: more ->
                        let after = String.sub text (at + 2) (String.length text - at - 2) in
                        fill after (filler :: String.sub text 0 at :: acc) more
                    | Some _, _ | None, _ -> String.concat "" (List.rev (text :: acc))
                  in
                  Text (fill template [] rest)
              | _ -> Null);
        }
    | "round-to" ->
        ints2 (fun x step ->
            if Int64.compare step 0L <= 0 then Null
            else
              let half = Int64.div step 2L in
              let shifted = if Int64.compare x 0L >= 0 then Int64.add x half else Int64.sub x half in
              Int (Int64.mul (Int64.div shifted step) step))
    | "floor-to" -> ints2 (fun x step -> if Int64.compare step 0L <= 0 then Null else Int (Int64.mul (floor_div x step) step))
    | "clamp" ->
        let%map () = typed [ Int; Int; Int ] in
        let a = one () and b = two () and c = three () in
        {
          at;
          kind = Some Int;
          eval =
            (fun frame ->
              match (a frame, b frame, c frame) with
              | Int x, Int low, Int high when Int64.compare low high <= 0 -> Int (Int64.max low (Int64.min high x))
              | _ -> Null);
        }
    | "keyed-noise" ->
        let%map () = typed [ Int; Int ] in
        let a = one () and b = two () in
        {
          at;
          kind = Some Int;
          eval =
            (fun frame ->
              match (a frame, b frame) with
              | Int x, Int percent ->
                  let percent = Int64.max 0L (Int64.min 100L percent) in
                  let spread = Int64.div (Int64.mul (Int64.abs (Int64.max x (-4_000_000_000_000_000L))) percent) 100L in
                  Int (Int64.add x (Keyed.within frame.key (Int64.to_string x) ~low:(Int64.neg spread) ~high:spread))
              | _ -> Null);
        }
    | "keyed-uuid" ->
        texts1 (fun key seed ->
            let hex = Keyed.hex key seed 32 in
            let variant = "89ab".[Char.code hex.[16] mod 4] in
            String.concat "-"
              [
                String.sub hex 0 8;
                String.sub hex 8 4;
                "4" ^ String.sub hex 13 3;
                String.make 1 variant ^ String.sub hex 17 3;
                String.sub hex 20 12;
              ])
    | "luhn" ->
        texts1 (fun _ x ->
            let digits = List.rev (List.filter_map (fun c -> if c >= '0' && c <= '9' then Some (Char.code c - 48) else None) (List.of_seq (String.to_seq x))) in
            let weighted i d =
              if i mod 2 = 1 then d
              else
                let doubled = d * 2 in
                if doubled > 9 then doubled - 9 else doubled
            in
            let sum = List.fold_left ( + ) 0 (List.mapi weighted digits) in
            string_of_int ((10 - (sum mod 10)) mod 10))
    | "year" | "month" | "day" ->
        let%map () = typed [ Date ] in
        let a = one () in
        let part (year, month, day) = match name with "year" -> year | "month" -> month | _ -> day in
        { at; kind = Some Int; eval = (fun frame -> match a frame with Date d -> Int (Int64.of_int (part (civil d))) | _ -> Null) }
    | "make-date" ->
        let%map () = typed [ Int; Int; Int ] in
        let a = one () and b = two () and c = three () in
        {
          at;
          kind = Some Date;
          eval =
            (fun frame ->
              match (a frame, b frame, c frame) with
              | Int year, Int month, Int day when Int64.compare year 0L >= 0 && Int64.compare year 9999L <= 0 ->
                  if Int64.compare month 1L < 0 || Int64.compare month 12L > 0 || Int64.compare day 1L < 0 || Int64.compare day 31L > 0 then Null
                  else date_of ~year:(Int64.to_int year) ~month:(Int64.to_int month) ~day:(Int64.to_int day)
              | _ -> Null);
        }
    | "add-days" ->
        let%map () = typed [ Date; Int ] in
        let a = one () and b = two () in
        {
          at;
          kind = Some Date;
          eval =
            (fun frame ->
              match (a frame, b frame) with
              | Date d, Int n when Int64.compare (Int64.abs n) 4_000_000L <= 0 -> within_years (d + Int64.to_int n)
              | _ -> Null);
        }
    | "days-between" ->
        let%map () = typed [ Date; Date ] in
        let a = one () and b = two () in
        { at; kind = Some Int; eval = (fun frame -> match (a frame, b frame) with Date x, Date y -> Int (Int64.of_int (y - x)) | _ -> Null) }
    | "date->text" ->
        let%map () = typed [ Date ] in
        let a = one () in
        { at; kind = Some Text; eval = (fun frame -> match a frame with Date d -> Text (date_text d) | _ -> Null) }
    | "text->date" ->
        let%map () = typed [ Text ] in
        let a = one () in
        { at; kind = Some Date; eval = (fun frame -> match a frame with Text x -> parsed_date (String.trim x) | _ -> Null) }
    | "keyed-date" ->
        let%map () = typed [ Text; Date; Date ] in
        let a = one () and b = two () and c = three () in
        {
          at;
          kind = Some Date;
          eval =
            (fun frame ->
              match (a frame, b frame, c frame) with
              | Text seed, Date low, Date high when low <= high ->
                  Date (Int64.to_int (Keyed.within frame.key seed ~low:(Int64.of_int low) ~high:(Int64.of_int high)))
              | _ -> Null);
        }
    | "date-of" ->
        let%map () = typed [ Datetime ] in
        let a = one () in
        { at; kind = Some Date; eval = (fun frame -> match a frame with Datetime x -> Date (Int64.to_int (floor_div x micros_per_day)) | _ -> Null) }
    | "seconds-of-day" ->
        let%map () = typed [ Datetime ] in
        let a = one () in
        {
          at;
          kind = Some Int;
          eval =
            (fun frame ->
              match a frame with
              | Datetime x ->
                  let midnight = Int64.mul (floor_div x micros_per_day) micros_per_day in
                  Int (Int64.div (Int64.sub x midnight) micros_per_second)
              | _ -> Null);
        }
    | "make-datetime" ->
        let%map () = typed [ Date; Int ] in
        let a = one () and b = two () in
        {
          at;
          kind = Some Datetime;
          eval =
            (fun frame ->
              match (a frame, b frame) with
              | Date d, Int seconds when Int64.compare seconds 0L >= 0 && Int64.compare seconds 86_399L <= 0 ->
                  Datetime (Int64.add (Int64.mul (Int64.of_int d) micros_per_day) (Int64.mul seconds micros_per_second))
              | _ -> Null);
        }
    | "add-seconds" ->
        let%map () = typed [ Datetime; Int ] in
        let a = one () and b = two () in
        {
          at;
          kind = Some Datetime;
          eval =
            (fun frame ->
              match (a frame, b frame) with
              | Datetime x, Int n when Int64.compare (Int64.abs n) 400_000_000_000L <= 0 -> (
                  let moved = Int64.add x (Int64.mul n micros_per_second) in
                  match within_years (Int64.to_int (floor_div moved micros_per_day)) with
                  | Date _ -> Datetime moved
                  | _ -> Null)
              | _ -> Null);
        }
    | "datetime->text" ->
        let%map () = typed [ Datetime ] in
        let a = one () in
        { at; kind = Some Text; eval = (fun frame -> match a frame with Datetime x -> Text (datetime_text x) | _ -> Null) }
    | "text->datetime" ->
        let%map () = typed [ Text ] in
        let a = one () in
        { at; kind = Some Datetime; eval = (fun frame -> match a frame with Text x -> parsed_datetime x | _ -> Null) }
    | _ -> Error (Unknown { at; name })
  in
  Result.map_error
    (fun error -> `Mask_script error)
    (let%bind code = compile ~locals:[] ~visible:script.definitions expression in
     let%map () = Option.fold result ~none:(Ok ()) ~some:(fun result -> expect result code) in
     { reads = !reads; slots = !slots; code = code.eval })

let compile script expression ~value ~columns ~result =
  compiled script expression ~value ~columns ~result:(Some result)

let evaluate key text ~given =
  let value, kind =
    match given with
    | Some (kind, text) -> (read_value kind text, kind)
    | None -> (Null, (Text : kind))
  in
  let%bind expression = expression ~source:"example" text in
  let%map compiled = compiled empty expression ~value:kind ~columns:(fun _ -> None) ~result:None in
  shown (run compiled key ~value ~row:[||])
