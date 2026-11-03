@@ portable

(** gRPC calls over HTTP/2 with one response message, without compression. *)

type error =
  | Http of int  (** A [:status] other than 200. *)
  | Not_grpc of string option  (** The response's [content-type] is not gRPC's. *)
  | No_status  (** Neither the trailers nor the headers carry [grpc-status]. *)
  | Status of { code : int; message : string }  (** The call failed with this status. *)
  | Compressed  (** A compressed message, which this side never accepts. *)
  | Framing of int  (** The body is not one whole message; its length. *)
[@@deriving sexp_of ~portable]

val start :
  Http2.t ->
  path:string ->
  (Http2.stream, [> `Http2 of Http2.error | `Transport of Quiver_mysql.Transport.error ]) result
(** A call whose request is a stream of messages. *)

val send :
  Http2.stream ->
  string ->
  (unit, [> `Http2 of Http2.error | `Hpack of Hpack.error | `Transport of Quiver_mysql.Transport.error ]) result
(** One more request message, encoded protobuf. *)

val finish :
  Http2.stream ->
  ( string,
    [> `Grpc of error | `Http2 of Http2.error | `Hpack of Hpack.error | `Transport of Quiver_mysql.Transport.error ] )
  result
(** Ends the request and reads the one response message. *)

val call :
  Http2.t ->
  path:string ->
  string ->
  ( string,
    [> `Grpc of error | `Http2 of Http2.error | `Hpack of Hpack.error | `Transport of Quiver_mysql.Transport.error ] )
  result
(** The response message to one request message, both encoded protobuf. *)
