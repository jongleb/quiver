@@ portable

(** A language of S-expressions for masking a column: each row's value, the other columns of
    its row as read, constants, text, number and date functions, words from files and the keyed
    functions of {!Keyed}. A script holds [(define (NAME PARAMETER …) BODY)], [(mask SELECTOR
    BODY)], [(keep SELECTOR)], [(permute SELECTOR [LOW HIGH])], [(references "DB.TABLE.COLUMN"
    "DB.TABLE.COLUMN")], [(strict "DATABASE.TABLE" …)], [(after-load "DATABASE.TABLE"
    "STATEMENT")] and [(words NAME "FILE")] forms. Programs are typed before anything is read and
    always end: a definition calls only those before it, and nothing loops. NULL goes through
    functions as in SQL. *)

type position = { source : string; line : int; column : int } [@@deriving sexp_of]
(** [source]: the file, or the option an expression came in. *)

(** What an expression gives. *)
type kind = Text | Int | Bool | Date | Datetime [@@deriving sexp_of]

type error =
  | Syntax of { at : position; reason : string }
  | Unknown of { at : position; name : string }  (** No such function or name here. *)
  | Arity of { at : position; name : string; expected : string; given : int }
  | Mistyped of { at : position; expected : kind; found : kind }
  | Unreadable_column of { at : position; column : string }
      (** [(row COLUMN)] names no column of the table that holds text or an integer. *)
  | Defined_twice of { at : position; name : string }  (** Or named as a function of the language. *)
  | Misplaced of { at : position; form : string }
      (** A top-level form other than [define] and [mask], or a reserved word out of place. *)
  | Too_large of { at : position }  (** More than a hundred thousand steps once definitions are expanded. *)
  | Called_before_defined of { at : position; name : string }
      (** A definition calls itself or one defined after it. *)
  | Unknown_type of { at : position; name : string }  (** In [(type …)] of a selector. *)
  | Unloadable of { at : position; path : string; reason : string }  (** A words file. *)
[@@deriving sexp_of]

val describe : error -> string
(** [SOURCE:LINE:COLUMN: what is wrong], for people. *)

type expression
(** An expression as written, not typed yet. *)

(** {2 The reference of the language, which its documentation is made of} *)

(** What a parameter or a result is. *)
type shape =
  | Of of kind
  | Same  (** Any one kind, the same at every [Same] of the function. *)
  | Words  (** The name of a [(words …)] list. *)

(** How many arguments a function takes. *)
type arity =
  | Exactly
  | Optional_last  (** All, or all but the last. *)
  | Repeated_last of int  (** The last parameter at least so many times. *)

type parameter = { name : string; shape : shape }

type example = { expression : string; given : (kind * string) option; gives : string }
(** An expression, the column's value as {!read_value} reads it ([None]: NULL, as text), and what
    {!evaluate} gives, as {!shown} prints it. *)

type entry = {
  name : string;
  parameters : parameter list;
  arity : arity;
  result : shape;
  doc : string;
  examples : example list;
}

type section = { title : string; doc : string; entries : entry list }

val reference : section list
(** Every function of the language: no other name is one. *)

val example_secret : string
(** The [--mask-secret] of the examples of keyed functions. *)

type t
(** Definitions and rules of scripts, in order. *)

val empty : t

val parse :
  source:string ->
  ?within:t ->
  load:(string -> (string, string) result) ->
  string ->
  (t, [> `Mask_script of error ]) result
(** A script after [within]: it may call [within]'s definitions, its rules come after; [load]
    reads the files of [(words …)]. *)

val expression : source:string -> string -> (expression, [> `Mask_script of error ]) result

(** Which columns a rule takes. *)
type selector =
  | Named of string  (** [DATABASE.TABLE.COLUMN] with [*] and [?]. *)
  | Where of { table : string; column : string; types : string list }
      (** [(where (table …) (column …) (type …))]: patterns, and the column types it takes, any when empty. *)

(** What a rule does with its columns. *)
type target =
  | Masked of expression
  | Kept
  | Permuted of (int64 * int64) option  (** [(permute SELECTOR)], or with its domain [LOW HIGH]. *)

type rule = { at : position; selector : selector; target : target }

val rules : t -> rule list
(** Each [(mask …)] and [(keep …)] form, in order. *)

val strict : t -> (position * string) list
(** Tables whose every column some rule must take. *)

val after_load : t -> (position * string * string) list
(** Table and statement of each [(after-load …)] form. *)

val references : t -> ((string * string * string) * (string * string * string)) list
(** Each [(references "DB.TABLE.COLUMN" "DB.TABLE.COLUMN")] form: a column that refers to
    another though no foreign key says so, as database, table and column. *)

val sexp_of_expression : expression -> Sexplib0.Sexp.t

(** A value an expression reads or gives. *)
type value = Null | Text of string | Int of int64 | Bool of bool | Date of int | Datetime of int64
[@@deriving sexp_of]
(** [Date]: days since 1970-01-01; [Datetime]: microseconds since then. *)

val shown : value -> string
(** As [quiver mask] prints it: [\\N] for NULL, dates as [YYYY-MM-DD], times as
    [YYYY-MM-DD HH:MM:SS[.ffffff]]. *)

val read_value : kind -> string -> value
(** A value of the kind from its text, NULL when it is not one. *)

type compiled : value mod contended portable
(** An expression typed for one column and turned into code. *)

val compile :
  t ->
  expression ->
  value:kind ->
  columns:(string -> kind option) ->
  result:kind ->
  (compiled, [> `Mask_script of error ]) result
(** Typed with [value] as the column's kind, [columns] as the kinds of the row's columns by name
    (any letter case) and [result] as the kind the column takes. *)

val reads : compiled -> string list
(** The columns [(row …)] reads, in the order {!run} takes their values. *)

val run : compiled -> Keyed.t -> value:value -> row:value array -> value

val evaluate :
  Keyed.t -> string -> given:(kind * string) option -> (string, [> `Mask_script of error ]) result
(** An expression with no definitions and no other columns on one value, as {!shown} prints what
    it gives, whatever kind that is. *)
