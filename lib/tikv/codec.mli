@@ portable

(** Keys as TiDB lays them out in TiKV. *)

type error =
  | Short  (** A group of fewer than nine bytes. *)
  | Marker of int  (** A group's last byte says more than eight bytes of padding. *)
  | Padding  (** Padding bytes that are not zero. *)
[@@deriving sexp_of ~portable]

val encode_bytes : string -> string
(** The order-keeping form region bounds are given in: groups of eight bytes, the last padded with
    zeros, each followed by 255 less its padding. *)

val decode_bytes : string -> (string, [> `Codec of error ]) result
(** The bytes an encoded form starts with; what follows it is ignored, as TiDB's client does. *)

val records : int64 -> string * string
(** The first key of the table's rows and the first key past them. *)

val table : int64 -> string * string
(** The first key of the table's rows and indexes and the first key past them. *)

val retabled : into:int64 -> string -> string
(** A key of {!table}'s range as the same key of another table; not the key past the range,
    which is the next table's first. *)

val kind_end : string -> string option
(** The first key past the index, or the rows, of the table this key is of: what keys of one kind
    span. *)
