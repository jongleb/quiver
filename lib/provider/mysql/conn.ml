type t = { link : Link.t }

let failure request error = Client_error.of_failure request error

let connect route ~timeouts ~user ~password =
  match
    Link.connect ~local_data:Without_local_data route ~timeouts ~user ~password
  with
  | Ok link -> Ok { link }
  | Error error -> Error (failure Connect error)

let execute t statement =
  Result.map_error
    (failure (Statement statement))
    (Link.run_statement t.link statement)

let rows t query =
  Result.map_error (failure (Query query)) (Link.run_query t.link query)

let link t = t.link
let close t = Link.close t.link
