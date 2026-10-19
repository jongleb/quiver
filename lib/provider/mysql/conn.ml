type t = { link : Link.t }

let failure request error = `Client (Client_error.of_failure request error)

let connect route ~timeouts ~user ~password =
  match
    Link.connect ~local_data:Without_local_data route ~timeouts ~user ~password
  with
  | Ok link -> Ok { link }
  | Error error -> Error (failure Connect error)

let execute t statement =
  Result.map_error
    (failure (Statement statement))
    (Link.execute t.link (Statement.sql statement))

let rows t query =
  Result.map_error (failure (Query query)) (Link.query t.link (Query.sql query))

let link t = t.link
let close t = Link.close t.link
