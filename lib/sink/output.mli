type error =
  | Io of { file : File_path.t @@ aliased; message : string @@ aliased }
      (** [message] is the operating system's description. *)
  | Codec of string @@ aliased  (** The compressor failed. *)
[@@deriving sexp_of ~portable]

type frame : value mod portable
(** One piece of output encoded on its own — a zstd frame or plain text — so
    pieces can be encoded on any domain and concatenated in order. *)

type t
(** A file being assembled from frames, hashing the bytes it stores. *)

val start : Quiver_core.Buffer_pool.t -> Quiver_core.Byte_buffer.t @ unique @@ portable
(** A buffer to render a frame's text into. *)

val seal :
  Quiver_core.Buffer_pool.t ->
  Compression.t ->
  Quiver_core.Byte_buffer.t @ unique ->
  (frame, [> `Output of error ]) result @ unique @@ portable

val create : File_path.t -> Compression.t -> (t, [> `Output of error ]) result @@ portable
val append : t -> frame @ once unique -> (unit, [> `Output of error ]) result @@ portable

val emit :
  t ->
  Quiver_core.Buffer_pool.t ->
  Quiver_core.Byte_buffer.t @ unique ->
  (unit, [> `Output of error ]) result @@ portable
(** Seals a buffer with the file's compression and appends it. *)

val close :
  t -> rows:int -> (Quiver_core.Manifest.part, [> `Output of error ]) result @@ portable

val discard : t -> unit @@ portable

val save :
  File_path.t -> Compression.t -> string -> (unit, [> `Output of error ]) result @@ portable
