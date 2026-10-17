type error = Level_out_of_range of { level : int; min : int; max : int }
[@@deriving sexp_of ~portable]

(** A zstd compression level within the linked library's range. *)
module Level : sig
  type t = private int [@@deriving sexp_of ~portable]

  val of_int : int -> (t, [> `Compression of error ]) result
end

(** How output files are encoded. zstd frames carry checksums so the [zstd] tool
    can verify them. *)
type t = Plain | Zstd of Level.t [@@deriving sexp_of ~portable]

val extension : t -> string @@ portable

val zstd :
  Level.t ->
  Quiver_core.Byte_buffer.t @ local ->
  into:Quiver_core.Byte_buffer.t @ local ->
  (unit, string) result @@ portable

val unzstd :
  string -> into:Quiver_core.Byte_buffer.t @ local -> (unit, string) result
  @@ portable
(** Appends the decompressed frames of the text. *)

val inflate :
  (Bytes.t -> pos:int -> len:int -> (int, 'e) result) ->
  corrupt:(string -> 'e) ->
  (Bytes.t -> pos:int -> len:int -> (int, 'e) result)
  @@ portable
(** Reads the decompressed frames of what [read] gives, the same way: at most
    [len] bytes at [pos], 0 only at the end; a damaged or cut frame fails with
    [corrupt] of the codec's message. *)
(** Appends one zstd frame, with its checksum, of the source's bytes; the error
    is the codec's message. *)
