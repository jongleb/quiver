open Base.Result.Let_syntax

type t = { timeout : Core.Time_ns.Span.t; opened : (string, Http2.t) Hashtbl.t }

let create ~timeout = { timeout; opened = Hashtbl.create 4 }

let connection t address =
  match Hashtbl.find_opt t.opened address with
  | Some connection -> Ok connection
  | None -> (
      match String.rindex_opt address ':' with
      | None -> Error (`Region (Region.Malformed [ Some address ]))
      | Some colon -> (
          let host = String.sub address 0 colon in
          let host = if String.length host > 1 && host.[0] = '[' then String.sub host 1 (String.length host - 2) else host in
          match int_of_string_opt (String.sub address (colon + 1) (String.length address - colon - 1)) with
          | None -> Error (`Region (Region.Malformed [ Some address ]))
          | Some port ->
              let%bind connection = Http2.connect ~timeout:t.timeout ~host ~port in
              Hashtbl.replace t.opened address connection;
              Ok connection))

let close t =
  Hashtbl.iter (fun _ connection -> Http2.close connection) t.opened;
  Hashtbl.reset t.opened
