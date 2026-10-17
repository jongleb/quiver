@@ portable

(** Guesses where to look for chunk boundaries between a key column's minimum
    and maximum. Only a hint for balance: the provider turns each guess into an
    existing key with an index lookup and lets the server order them. *)

val between : Key.value -> Key.value -> count:int -> Key.value list
