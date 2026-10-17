(** Whether a backslash escapes the next character inside quotes, as the
    object's [sql_mode] decides. *)
type escapes = Backslash_escapes | No_backslash_escapes

val escapes : string option -> escapes @@ portable
val statement : escapes -> string -> string option @@ portable
