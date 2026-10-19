type error = Level_out_of_range of { level : int; min : int; max : int }
[@@deriving sexp_of ~portable]

external min_level : unit -> int @@ portable = "quiver_zstd_min_level"
external max_level : unit -> int @@ portable = "quiver_zstd_max_level"
external bound : int -> int @@ portable = "quiver_zstd_bound"
external message : int -> string @@ portable = "quiver_zstd_error_message"

module Level = struct
  type t = int [@@deriving sexp_of ~portable]

  let of_int level =
    let min = min_level () and max = max_level () in
    if level < min || level > max then
      Error (`Compression (Level_out_of_range { level; min; max }))
    else Ok level
end

type t = Plain | Zstd of Level.t [@@deriving sexp_of ~portable]

let extension = function Plain -> "" | Zstd _ -> ".zst"

external compress :
  Level.t -> Bytes.t -> int -> Bytes.t -> int -> int -> int @@ portable
  = "quiver_zstd_compress_byte" "quiver_zstd_compress"

let zstd level (source : Quiver_core.Byte_buffer.t @ local)
    ~(into : Quiver_core.Byte_buffer.t @ local) =
  let room = bound source.len in
  Quiver_core.Byte_buffer.ensure_free into room;
  match compress level source.bs source.len into.bs into.len room with
  | written when written >= 0 ->
      into.len <- into.len + written;
      Ok ()
  | code -> Error (message (-code))

type inflater

external inflater : unit -> inflater @@ portable = "quiver_zstd_inflater"

type progress = { mutable consumed : int; mutable produced : int }
[@@warning "-69"]
(** Where the last step left the source and the target; set by
    [quiver_zstd_inflate]. *)

external step :
  inflater -> Bytes.t -> int -> int -> Bytes.t -> int -> int -> progress -> int
  @@ portable = "quiver_zstd_inflate_byte" "quiver_zstd_inflate"

(** Whether the compressed source can still give bytes. *)
type source = Open | Drained

type window = {
  input : Bytes.t;
  mutable first : int;
  mutable last : int;
  mutable source : source;
  mutable hint : int;
}
(** The compressed bytes read but not yet inflated, and zstd's hint after the
    last step: 0 once every frame begun has ended. *)

external decompress :
  inflater -> Bytes.t -> int -> Bytes.t -> int -> int -> int @@ portable
  = "quiver_zstd_decompress_byte" "quiver_zstd_decompress"
[@@noalloc]

let unzstd inflater text ~length ~bound
    ~(into : Quiver_core.Byte_buffer.t @ local) =
  Quiver_core.Byte_buffer.ensure_free into bound;
  match
    decompress inflater (Bytes.unsafe_of_string text)
      (Int.min length (String.length text))
      into.bs into.len bound
  with
  | written when written >= 0 ->
      into.len <- into.len + written;
      Ok ()
  | code -> Error (message (-code))

let inflate read ~corrupt =
  let inflater = inflater () and progress = { consumed = 0; produced = 0 } in
  let window =
    { input = Bytes.create 131072; first = 0; last = 0; source = Open; hint = 1 }
  in
  let drained () =
    window.first = window.last && match window.source with Drained -> true | Open -> false
  in
  let refill () =
    if window.first <> window.last then Ok ()
    else
      match window.source with
      | Drained -> Ok ()
      | Open -> (
          match read window.input ~pos:0 ~len:(Bytes.length window.input) with
          | Error error -> Error error
          | Ok 0 ->
              window.source <- Drained;
              Ok ()
          | Ok length ->
              window.first <- 0;
              window.last <- length;
              Ok ())
  in
  let rec pull bytes ~pos ~len =
    match refill () with
    | Error error -> Error error
    | Ok () when drained () && window.hint = 0 -> Ok 0
    | Ok () ->
        let hint =
          step inflater window.input window.first window.last bytes pos (pos + len) progress
        in
        if hint < 0 then Error (corrupt (message (-hint)))
        else
          let () = window.first <- progress.consumed in
          let () = window.hint <- hint in
          let produced = progress.produced - pos in
          if produced > 0 then Ok produced
          else if drained () && hint <> 0 then Error (corrupt "truncated zstd frame")
          else pull bytes ~pos ~len
  in
  fun bytes ~pos ~len ->
    Base.Ordered_collection_common.check_pos_len_exn ~pos ~len
      ~total_length:(Bytes.length bytes);
    if len = 0 then Ok 0 else pull bytes ~pos ~len
