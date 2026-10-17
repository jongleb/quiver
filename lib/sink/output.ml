open Base.Result.Let_syntax

type error =
  | Io of { file : File_path.t @@ aliased; message : string @@ aliased }
  | Codec of string @@ aliased
[@@deriving sexp_of ~portable]

type frame = {
  buffer : Quiver_core.Byte_buffer.t;
  plain : int;
  owner : Quiver_core.Buffer_pool.t @@ aliased many;
}

type t = {
  file : File_path.t;
  compression : Compression.t;
  fd : Unix.file_descr;
  hash : Xxh3.t;
  mutable plain : int;
  mutable stored : int;
}

let io file f =
  match f () with
  | value -> Ok value
  | exception Unix.Unix_error (error, _, _) ->
      Error (`Output (Io { file; message = Unix.error_message error }))

let start buffers = Quiver_core.Buffer_pool.take buffers

let seal buffers (compression : Compression.t)
    (source : Quiver_core.Byte_buffer.t @ unique) =
  let plain = Quiver_core.Byte_buffer.length (borrow_ source) in
  match compression with
  | Plain -> Ok { buffer = source; plain; owner = buffers }
  | Zstd level -> (
      let target = Quiver_core.Buffer_pool.take buffers in
      let compressed =
        Compression.zstd level (borrow_ source) ~into:(borrow_ target)
      in
      Quiver_core.Buffer_pool.give buffers source;
      match compressed with
      | Ok () -> Ok { buffer = target; plain; owner = buffers }
      | Error message ->
          Quiver_core.Buffer_pool.give buffers target;
          Error (`Output (Codec message)))

let create file compression =
  io file (fun () ->
      {
        file;
        compression;
        fd =
          Unix.openfile (File_path.to_string file)
            [ O_WRONLY; O_CREAT; O_TRUNC; O_CLOEXEC ]
            0o644;
        hash = Xxh3.create ();
        plain = 0;
        stored = 0;
      })

let append t ({ buffer; plain; owner } : frame @ once unique) =
  let written =
    let { Quiver_core.Byte_buffer.bs; len } = borrow_ buffer in
    io t.file (fun () ->
        let rec write pos =
          match pos < len with
          | true -> write (pos + Unix.write t.fd bs pos (len - pos))
          | false -> ()
        in
        write 0;
        Xxh3.add t.hash bs ~pos:0 ~len;
        t.stored <- t.stored + len;
        t.plain <- t.plain + plain)
  in
  Quiver_core.Buffer_pool.give owner buffer;
  written

let emit t buffers buffer =
  match seal buffers t.compression buffer with
  | Ok frame -> append t frame
  | Error error -> Error error

let close t ~rows =
  let%bind () =
    match (t.compression, t.stored) with
    | Zstd _, 0 ->
        let buffers = Quiver_core.Buffer_pool.create () in
        emit t buffers (start buffers)
    | Zstd _, _ | Plain, _ -> Ok ()
  in
  io t.file (fun () ->
      Unix.close t.fd;
      {
        Quiver_core.Manifest.file = File_path.basename_exn t.file;
        rows;
        bytes = t.plain;
        stored_bytes = t.stored;
        checksum = Xxh3_64 (Xxh3.value t.hash);
      })

let discard t =
  match Unix.close t.fd with () -> () | exception Unix.Unix_error _ -> ()

let save file compression contents =
  let pool = Quiver_core.Buffer_pool.create () in
  let%bind output = create file compression in
  let buffer = start pool in
  Quiver_core.Byte_buffer.append_string (borrow_ buffer) contents;
  let%bind () = emit output pool buffer in
  let%map _ = close output ~rows:0 in
  ()
