type request =
  | Connect
  | Query of Query.t
  | Statement of Statement.t
  | Fetch of Query.t
[@@deriving sexp_of ~portable]

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
    }
  | Connection of { request : request; failure : failure }
  | Unexpected_result of Query.t
[@@deriving sexp_of ~portable]

let of_failure request = function
  | `Link (Link.Refused { code; sqlstate; message }) ->
      Server { request; code; sqlstate; message }
  | `Link error -> Connection { request; failure = Link error }
  | `Wire error -> Connection { request; failure = Wire error }
  | `Transport error -> Connection { request; failure = Transport error }
