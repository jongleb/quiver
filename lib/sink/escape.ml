type t = { flags : string; replacements : string iarray }

let make replace =
  {
    flags =
      String.init 256 (fun code ->
          Option.fold ~none:'\000'
            ~some:(fun _ -> '\001')
            (replace (Char.chr code)));
    replacements =
      Iarray.init 256 (fun code ->
          Option.value ~default:"" (replace (Char.chr code)));
  }

let escape { flags; replacements } bytes ~off ~len out =
  let start = ref off in
  for i = off to off + len - 1 do
    let code = Char.code (Bytes.unsafe_get bytes i) in
    if String.unsafe_get flags code <> '\000' then (
      Quiver_core.Byte_buffer.append_subbytes out bytes !start (i - !start);
      Quiver_core.Byte_buffer.append_string out
        (Iarray.unsafe_get replacements code);
      start := i + 1)
  done;
  Quiver_core.Byte_buffer.append_subbytes out bytes !start (off + len - !start)

let add t column row out =
  escape t
    (Quiver_core.Column.text_bytes column)
    ~off:(Quiver_core.Column.text_offset column row)
    ~len:(Quiver_core.Column.text_length column row)
    out

let add_string t text out =
  escape t (Bytes.unsafe_of_string text) ~off:0 ~len:(String.length text) out
