@@ portable

(** Keyed permutations by FF1 of NIST SP 800-38G (radix 2, ten rounds of AES) with cycle walking:
    each is a bijection of its domain, the same under the same key. *)

val below : key:string -> limit:int64 -> int64 -> int64
(** The permutation of the integers from [0] to [limit], both read unsigned, under a 32-byte
    key; the value must be one of them. *)

val digits : key:string -> tweak:string -> radixes:string -> string -> string option
(** The permutation of the strings of digits whose [i]-th digit is below the [i]-th byte of
    [radixes] (the byte 0 standing for 256), each digit a byte: [None] when there are more than
    [2^8192] such strings, a radix is 1 or a digit not below its radix, or the tweak is longer
    than 64 bytes. *)

val aes : key:string -> string -> string option
(** One block of AES under a 16-, 24- or 32-byte key. *)
