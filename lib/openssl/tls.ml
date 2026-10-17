type context : value mod contended portable
type trust = System_roots | Ca_file of string

(** The [Error] built by the C stubs. *)
type failed = Openssl [@@warning "-37"]

external error : unit -> string @@ portable = "quiver_tls_error"

external make_client : trust -> (context, failed) result @@ portable
  = "quiver_tls_client"

let client trust = Result.map_error (fun Openssl -> error ()) (make_client trust)

type unusable =
  | Context of string
  | Certificate of string
  | Key of string
  | Unmatching_keys
  | Peers of string

(** The [Error] built by [quiver_tls_mutual], one per refused part. *)
type refused =
  | Refused_context
  | Refused_certificate
  | Refused_key
  | Refused_pair
  | Refused_peers
[@@warning "-37"]

external make_mutual :
  string -> string -> string -> (context, refused) result @@ portable
  = "quiver_tls_mutual"

let mutual ~certificate ~key ~peers =
  Result.map_error
    (function
      | Refused_context -> Context (error ())
      | Refused_certificate -> Certificate (error ())
      | Refused_key -> Key (error ())
      | Refused_pair -> Unmatching_keys
      | Refused_peers -> Peers (error ()))
    (make_mutual certificate key peers)

type session : value mod contended portable

external make_session :
  context -> Unix.file_descr -> (session, failed) result @@ portable
  = "quiver_tls_session"

let session context fd =
  Result.map_error (fun Openssl -> error ()) (make_session context fd)

type expectation = Host | Ip | Server_name

type 'a outcome =
  | Done of 'a
  | Want_read
  | Want_write
  | Closed
  | Lost
  | Failed of string

let outcome code ~done_ =
  match code with
  | -1 -> Want_read
  | -2 -> Want_write
  | -3 -> Closed
  | -4 -> Lost
  | code when code < 0 -> Failed (error ())
  | code -> Done (done_ code)

let naming host =
  match Unix.inet_addr_of_string host with
  | _ -> Ip
  | exception Failure _ -> Host

external expect_code : session -> expectation -> string -> int @@ portable
  = "quiver_tls_expect"

let expect session expectation name =
  match expect_code session expectation name with
  | 0 -> Ok ()
  | _ -> Error (error ())

(** The handshake or shutdown step [quiver_tls_step] takes. *)
type step = Connect | Accept | Shutdown [@@warning "-37"]

external step : session -> step -> int @@ portable = "quiver_tls_step"

let connect session = outcome (step session Connect) ~done_:ignore
let accept session = outcome (step session Accept) ~done_:ignore
let shutdown session = outcome (step session Shutdown) ~done_:ignore

external read_code : session -> Bytes.t -> int -> int -> int @@ portable
  = "quiver_tls_read"

external read_bigstring_code :
  session -> Base_bigstring.t -> int -> int -> int @@ portable
  = "quiver_tls_read_bigstring"

external write_code : session -> string -> int -> int -> int @@ portable
  = "quiver_tls_write"

let read session bytes ~pos ~len =
  Base.Ordered_collection_common.check_pos_len_exn ~pos ~len
    ~total_length:(Bytes.length bytes);
  outcome (read_code session bytes pos len) ~done_:Fun.id

let read_bigstring session bigstring ~pos ~len =
  Base.Ordered_collection_common.check_pos_len_exn ~pos ~len
    ~total_length:(Base_bigstring.length bigstring);
  outcome (read_bigstring_code session bigstring pos len) ~done_:Fun.id

let write session text ~pos ~len =
  Base.Ordered_collection_common.check_pos_len_exn ~pos ~len
    ~total_length:(String.length text);
  outcome (write_code session text pos len) ~done_:Fun.id

external verify_result : session -> int @@ portable = "quiver_tls_verify_result"
external verify_message : int -> string @@ portable = "quiver_tls_verify_message"

let verified session =
  match verify_result session with
  | 0 -> Ok ()
  | code -> Error (verify_message code)

external peer_sha256 : session -> string @@ portable = "quiver_tls_peer_sha256"

let fingerprint session =
  match peer_sha256 session with "" -> None | digest -> Some digest

type validity = Within_dates | Outside_dates | No_certificate [@@warning "-37"]

external validity : session -> validity @@ portable = "quiver_tls_peer_validity"
