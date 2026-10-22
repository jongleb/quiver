external unchecked_digest :
  Bytes.t -> (int[@untagged]) -> (int[@untagged]) -> (int[@untagged]) @@ portable
  = "quiver_crc32c_byte" "quiver_crc32c"
[@@noalloc]

let digest bytes ~pos ~len =
  Base.Ordered_collection_common.check_pos_len_exn ~pos ~len
    ~total_length:(Bytes.length bytes);
  unchecked_digest bytes pos len
