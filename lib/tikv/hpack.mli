@@ portable

(** HPACK, the header compression of HTTP/2 (RFC 7541). *)

type error =
  | Truncated  (** The block ends inside a representation. *)
  | Integer_overflow  (** An integer past 2{^28}. *)
  | Index of int  (** No table entry at this index. *)
  | Huffman  (** EOS in a string, or padding that is not up to 7 bits of EOS. *)
  | Table_size of int  (** A size update above the limit this side announced. *)
  | Late_size_update  (** A size update after a header field. *)
[@@deriving sexp_of ~portable]

type decoder
(** The dynamic table of one direction of a connection. *)

val decoder : limit:int -> decoder
(** [limit]: the table size this side announced in its settings. *)

val decode : decoder -> string -> ((string * string) list, [> `Hpack of error ]) result
(** One whole header block, the fields in order. *)

val encode : Buffer.t -> (string * string) list -> unit
(** A header block that leaves the peer's table empty: a size update to 0, then
    each field as a literal without indexing, without Huffman coding. *)
