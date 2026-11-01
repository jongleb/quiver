open Base.Result.Let_syntax

type piece = { data : string; rows : int }

(** Where the reader is in the file. *)
type place = Header | Rows

let statements ~bytes ~skip each ~run ~commit =
  let index = ref 0 and size = ref 0 and pending = ref 0 in
  let%bind () =
    each (fun statement ->
        let at = !index in
        index := at + 1;
        if at < skip then Ok ()
        else
          let%bind () = run statement in
          size := !size + String.length statement;
          pending := !pending + 1;
          if !size < bytes then Ok ()
          else (
            size := 0;
            pending := 0;
            commit (at + 1)))
  in
  if !pending > 0 then commit !index else Ok ()

let cut (dialect : Quiver_core.Output_format.dialect) ~bytes ~skip read take =
  let chunk = Bytes.create 65536 in
  let piece = Buffer.create (bytes + 65536) and header = Buffer.create 256 in
  let rec go ~quoted ~place ~skipping ~rows =
    match read chunk ~pos:0 ~len:(Bytes.length chunk) with
    | Error _ as error -> error
    | Ok 0 ->
        let open_row = Buffer.length piece > 0 && Buffer.nth piece (Buffer.length piece - 1) <> '\n' in
        let rows = if open_row then rows + 1 else rows in
        if rows > 0 then take { data = Buffer.contents header ^ Buffer.contents piece; rows } else Ok ()
    | Ok length ->
        let rec scan i ~quoted ~place ~skipping ~rows =
          if i >= length then go ~quoted ~place ~skipping ~rows
          else
            let c = Bytes.get chunk i in
            let quoted =
              match dialect with Rfc4180 when c = '"' -> not quoted | Rfc4180 | Mysql_load_data -> quoted
            in
            let ends = c = '\n' && not quoted in
            match (place, skipping, ends) with
            | Header, _, true ->
                Buffer.add_char header c;
                scan (i + 1) ~quoted ~place:Rows ~skipping ~rows
            | Header, _, false ->
                Buffer.add_char header c;
                scan (i + 1) ~quoted ~place ~skipping ~rows
            | Rows, n, true when n > 0 -> scan (i + 1) ~quoted ~place ~skipping:(n - 1) ~rows
            | Rows, n, false when n > 0 -> scan (i + 1) ~quoted ~place ~skipping ~rows
            | Rows, _, false ->
                Buffer.add_char piece c;
                scan (i + 1) ~quoted ~place ~skipping ~rows
            | Rows, _, true when Buffer.length piece + 1 >= bytes -> (
                Buffer.add_char piece c;
                let data = Buffer.contents header ^ Buffer.contents piece in
                Buffer.clear piece;
                match take { data; rows = rows + 1 } with
                | Error _ as error -> error
                | Ok () -> scan (i + 1) ~quoted ~place ~skipping ~rows:0)
            | Rows, _, true ->
                Buffer.add_char piece c;
                scan (i + 1) ~quoted ~place ~skipping ~rows:(rows + 1)
        in
        scan 0 ~quoted ~place ~skipping ~rows
  in
  let place = match dialect with Rfc4180 -> Header | Mysql_load_data -> Rows in
  go ~quoted:false ~place ~skipping:skip ~rows:0
