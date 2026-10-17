(** What the client was doing when the failure happened. *)
type request =
  | Connect
  | Query of Query.t
  | Statement of Statement.t
  | Fetch of Query.t
[@@deriving sexp_of ~portable]

(** Why a connection could not carry the request. *)
type failure =
  | Link of Link.error
  | Wire of Wire.error
  | Transport of Transport.error
[@@deriving sexp_of ~portable]

type t =
  | Server of {
      request : request;
      code : int;
      sqlstate : string;
      message : string;
    }  (** An ERR packet from the server. *)
  | Connection of { request : request; failure : failure }
  | Unexpected_result of Query.t  (** The result has an unexpected shape. *)
[@@deriving sexp_of ~portable]

val of_failure :
  request ->
  [< `Link of Link.error | `Wire of Wire.error | `Transport of Transport.error ] ->
  t @@ portable
