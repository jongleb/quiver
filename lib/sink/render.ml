external write_double :
  Bytes.t -> (int[@untagged]) -> float# -> (int[@untagged])
  @@ portable
  = "quiver_write_double_byte" "quiver_write_double"
[@@noalloc]

let[@inline] float (out : Quiver_core.Byte_buffer.t) value =
  Quiver_core.Byte_buffer.ensure_free out 34;
  out.len <- out.len + write_double out.bs out.len value

let text (out : Quiver_core.Byte_buffer.t) column row =
  Quiver_core.Byte_buffer.append_subbytes out
    (Quiver_core.Column.text_bytes column)
    (Quiver_core.Column.text_offset column row)
    (Quiver_core.Column.text_length column row)

let hex (out : Quiver_core.Byte_buffer.t) column row =
  let digits = "0123456789ABCDEF"
  and bytes = Quiver_core.Column.text_bytes column
  and off = Quiver_core.Column.text_offset column row
  and len = Quiver_core.Column.text_length column row in
  Quiver_core.Byte_buffer.ensure_free out (2 * len);
  for i = 0 to len - 1 do
    let code = Char.code (Bytes.unsafe_get bytes (off + i)) in
    Bytes.unsafe_set out.bs
      (out.len + (2 * i))
      (String.unsafe_get digits (code lsr 4));
    Bytes.unsafe_set out.bs
      (out.len + (2 * i) + 1)
      (String.unsafe_get digits (code land 15))
  done;
  out.len <- out.len + (2 * len)
