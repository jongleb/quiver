type t
(** A client connection with [utf8mb4] as its character set. *)

val connect :
  tls:Link.tls ->
  timeout:Core.Time_ns.Span.t ->
  host:string ->
  port:int ->
  user:string ->
  password:string option ->
  (t, [> `Client of Client_error.t ]) result @@ portable

val execute :
  t -> Statement.t -> (unit, [> `Client of Client_error.t ]) result @@ portable

val rows :
  t ->
  Query.t ->
  (string option array list, [> `Client of Client_error.t ]) result
  @@ portable

val link : t -> Link.t @@ portable
val close : t -> unit @@ portable
