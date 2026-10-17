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

let pull inflater progress window ~refill ~corrupt bytes ~pos ~len =
  let rec go () =
    match refill () with
    | Error error -> Error error
    | Ok () -> (
        match (window.first = window.last, window.source, window.hint) with
        | true, Drained, 0 -> Ok 0
        | _ -> (
            match
              step inflater window.input window.first window.last bytes pos
                (pos + len) progress
            with
            | code when code < 0 -> Error (corrupt (message (-code)))
            | hint -> (
                window.first <- progress.consumed;
                window.hint <- hint;
                match
                  ( progress.produced - pos,
                    window.first = window.last,
                    window.source )
                with
                | 0, true, Drained when hint <> 0 ->
                    Error (corrupt "truncated zstd frame")
                | 0, _, _ -> go ()
                | produced, _, _ -> Ok produced)))
  in
  go ()

let unzstd text ~(into : Quiver_core.Byte_buffer.t @ local) =
  let window =
    {
      input = Bytes.unsafe_of_string text;
      first = 0;
      last = String.length text;
      source = Drained;
      hint = 1;
    }
  in
  let read =
    pull (inflater ())
      { consumed = 0; produced = 0 }
      window
      ~refill:(fun () -> Ok ())
      ~corrupt:Fun.id
  in
  let rec go () =
    Quiver_core.Byte_buffer.ensure_free into 65536;
    match read into.bs ~pos:into.len ~len:(Bytes.length into.bs - into.len) with
    | Error message -> Error message
    | Ok 0 -> Ok ()
    | Ok length ->
        into.len <- into.len + length;
        go ()
  in
  match text with "" -> Ok () | _ -> go () [@nontail]

let inflate read ~corrupt =
  let window =
    { input = Bytes.create 131072; first = 0; last = 0; source = Open; hint = 1 }
  in
  let refill () =
    match (window.first = window.last, window.source) with
    | true, Open -> (
        match read window.input ~pos:0 ~len:(Bytes.length window.input) with
        | Error error -> Error error
        | Ok 0 ->
            window.source <- Drained;
            Ok ()
        | Ok length ->
            window.first <- 0;
            window.last <- length;
            Ok ())
    | _ -> Ok ()
  in
  let read =
    pull (inflater ()) { consumed = 0; produced = 0 } window ~refill ~corrupt
  in
  fun bytes ~pos ~len ->
    Base.Ordered_collection_common.check_pos_len_exn ~pos ~len
      ~total_length:(Bytes.length bytes);
    match len with 0 -> Ok 0 | _ -> read bytes ~pos ~len
