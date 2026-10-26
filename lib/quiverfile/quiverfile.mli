(** A quiverfile: defaults for quiver's command-line options, in YAML, by their
    long names — at the top for every command, under a command's name for that
    command alone. The command line and the environment win over it. *)

type error =
  | Unreadable of string  (** Not YAML, more than one document, or an ambiguous scalar: the parser's message. *)
  | Not_settings of string  (** Not a map of options and commands: where. *)
  | Unusable of string  (** An option whose value is neither a scalar nor a list of scalars. *)
  | Scoped of string  (** An option at the top that means different things to different commands. *)
[@@deriving sexp_of]

(** An option's value as the command line would spell it. *)
type value =
  | Text of string
  | Texts of string list  (** For an option given once per item. *)
[@@deriving sexp_of]

type t

val of_string :
  ?file:string -> commands:string list -> scoped:string list -> string -> (t, [> `Quiverfile of error ]) result
(** [commands] name the sections; every other key is an option; [scoped] options are
    only allowed in a section. *)

val settings : t -> command:string -> (string * value) list
(** The options for [command]: its own, then the common ones it does not set. *)
