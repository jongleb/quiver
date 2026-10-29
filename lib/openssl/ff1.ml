external permute_int64 : string -> int64 -> int64 -> int64 @@ portable = "quiver_permute_int64"

external permute_digits : string -> string -> string -> string -> string option @@ portable
  = "quiver_permute_digits"

external aes_block : string -> string -> string option @@ portable = "quiver_aes_block"

let below ~key ~limit value = permute_int64 key limit value
let digits ~key ~tweak ~radixes digits = permute_digits key tweak radixes digits
let aes ~key text = aes_block key text
