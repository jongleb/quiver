(** quiver's documentation as a static website, made from what the program knows of itself: the
    help of its commands and the reference of the masking language, so it cannot drift from them. *)

val escape : string -> string
(** Text for HTML: ampersands, angle brackets and both quotes as entities. *)

val slug : string -> string
(** An HTML id for a name: ASCII letters, digits and [-] kept, every other byte as [_XX]. *)

val signature : Quiver_core.Mask_script.entry -> string
(** [(NAME PARAMETER …) → KIND], as people read it. *)

type command = { name : string; summary : string; help : string }

val command : name:string -> help:string -> command
(** The summary is what follows [ - ] on the NAME line of the plain help. *)

val options : command -> string list
(** The long names of the options of the plain help, without their dashes. *)

val undocumented : command -> string list
(** The options of the plain help that say nothing of themselves, as their first lines. *)

val site : version:string -> commands:command list -> (string * string) list
(** Each file of the site, by its path, and its content: [index.html], [cli.html],
    [masking.html], [style.css]. *)
