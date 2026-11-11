open Base.Result.Let_syntax

type t = { dial : Http2.dial; opened : (string, Http2.t) Hashtbl.t }

let create dial = { dial; opened = Hashtbl.create 4 }

let connection t address =
  match Hashtbl.find_opt t.opened address with
  | Some connection -> Ok connection
  | None -> (
      match Http2.address address with
      | None -> Error (`Region (Region.Malformed [ Some address ]))
      | Some (host, port) ->
          let%bind connection = Http2.connect t.dial ~host ~port in
          Hashtbl.replace t.opened address connection;
          Ok connection)

let close t =
  Hashtbl.iter (fun _ connection -> Http2.close connection) t.opened;
  Hashtbl.reset t.opened
