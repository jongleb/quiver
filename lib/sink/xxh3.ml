type t

external create : unit -> t @@ portable = "quiver_xxh3_create"

external unchecked_add : t -> Bytes.t -> int -> int -> unit @@ portable
  = "quiver_xxh3_add"
[@@noalloc]

external value : t -> (int64[@unboxed]) @@ portable
  = "quiver_xxh3_value_byte" "quiver_xxh3_value"
[@@noalloc]

external unchecked_digest :
  Bytes.t -> (int[@untagged]) -> (int[@untagged]) -> (int64[@unboxed])
  @@ portable = "quiver_xxh3_digest_byte" "quiver_xxh3_digest"
[@@noalloc]

let add t bytes ~pos ~len =
  Base.Ordered_collection_common.check_pos_len_exn ~pos ~len
    ~total_length:(Bytes.length bytes);
  unchecked_add t bytes pos len

let digest bytes ~pos ~len =
  Base.Ordered_collection_common.check_pos_len_exn ~pos ~len
    ~total_length:(Bytes.length bytes);
  unchecked_digest bytes pos len
