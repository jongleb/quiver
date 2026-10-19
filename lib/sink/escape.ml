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
  let stop = off + len in
  let rec go i start =
    if i = stop then Quiver_core.Byte_buffer.append_subbytes out bytes start (stop - start)
    else
      let code = Char.code (Bytes.unsafe_get bytes i) in
      if String.unsafe_get flags code = '\000' then go (i + 1) start
      else
        let () = Quiver_core.Byte_buffer.append_subbytes out bytes start (i - start) in
        let () = Quiver_core.Byte_buffer.append_string out (Iarray.unsafe_get replacements code) in
        go (i + 1) (i + 1)
  in
  go off off [@nontail]

let add t column row out =
  escape t
    (Quiver_core.Column.text_bytes column)
    ~off:(Quiver_core.Column.text_offset column row)
    ~len:(Quiver_core.Column.text_length column row)
    out

let add_string t text out =
  escape t (Bytes.unsafe_of_string text) ~off:0 ~len:(String.length text) out
