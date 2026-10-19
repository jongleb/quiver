(** One connection of quiver's wire protocol: one thread at a time reads
    frames, any thread writes them. *)

type error =
  | Closed  (** The peer closed the connection. *)
  | Io of string  (** The operating system's description. *)
  | Malformed of string  (** A message that does not decode: the reason. *)
  | Tls of Protection.error
[@@deriving sexp_of ~portable]

type peer = { host : string; address : Unix.sockaddr }
(** A node to connect to: the name its certificate must carry, and where it
    listens. *)

(** A frame received. *)
type incoming =
  | Message of string  (** S-expression text. *)
  | Acknowledged of int
      (** Batch frames this connection carried that the far end has. *)
  | Batch of {
      codec : Frame.codec;
      chunk : int;
      seq : int;
      raw : int;
      header : string;  (** As received, to pass the frame on unchanged. *)
      payload : string;
          (** Its first [length] bytes, valid until the channel's next read. *)
      length : int;
    }

type t : value mod contended portable

type listener : value mod contended portable
(** A socket accepting connections until it is stopped. *)

val connect : Protection.t -> peer -> (t, [> `Channel of error ]) result
  @@ portable
val listen : Unix.sockaddr -> (listener, [> `Channel of error ]) result
val listening_address : listener -> Unix.sockaddr @@ portable

val accept : listener -> Protection.t -> (t, [> `Channel of error ]) result
  @@ portable
(** Waits for the next connection; [Closed] once the listener is stopped. *)

val stop : listener -> unit @@ portable

val all :
  ('a : value mod contended portable) ('e : value mod contended portable).
  int ->
  (unit -> ('a, 'e) result) @ portable ->
  close:('a -> unit) ->
  ('a list, 'e) result
(** Opens so many connections at once, each on a thread of its own, so their
    handshakes overlap; after a failure the others are closed. *)

val write :
  t -> string -> length:int -> (unit, [> `Channel of error ]) result @@ portable
(** A whole frame, the first [length] bytes. *)

val send : t -> Sexplib0.Sexp.t -> (unit, [> `Channel of error ]) result
  @@ portable
(** A message in a frame of its own. *)

val acknowledge :
  t -> received:int -> (unit, [> `Channel of error ]) result @@ portable
(** Tells the sender so many batch frames of this connection arrived. *)

val read :
  t -> (incoming, [> `Channel of error | `Frame of Frame.error ]) result @@ portable

val receive :
  t ->
  (Sexplib0.Sexp.t -> 'a) ->
  ('a, [> `Channel of error | `Frame of Frame.error ]) result
  @@ portable
(** The next frame, which must be a message decoding as given. *)

val forward :
  t ->
  header:string ->
  payload:string ->
  length:int ->
  (unit, [> `Channel of error ]) result
  @@ portable
(** A frame received elsewhere, unchanged: the payload's first [length]
    bytes. *)

val splice : t -> t -> (unit, [> `Channel of error ]) result @@ portable
(** Copies what each side sends to the other until both have finished; a
    failure either way ends both connections. *)

val finish : t -> unit @@ portable
(** Tells the peer nothing more will be sent. *)

type handle : immutable_data
(** A way to end a connection from a thread that does not own it. *)

val handle : t -> handle @@ portable
val path : t -> Quiver_core.Tcp.path option @@ portable
val mark : t -> Quiver_core.Tcp.Class.t -> unit @@ portable
val sever : handle -> unit @@ portable
(** Ends both directions, waking the owner if it is blocked reading. *)

val abort : t -> unit @@ portable
(** Ends both directions, waking a thread blocked reading. *)

val close : t -> unit @@ portable
