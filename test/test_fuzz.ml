let raw = QCheck2.Gen.(string_size ~gen:char (int_range 0 300))

(** A packet with a valid header around random payload, so fuzzing gets past
    framing into the parsers. *)
let framed =
  QCheck2.Gen.(
    let+ payloads = list_size (int_range 1 4) raw in
    Test_wire.frames payloads)

let wire_input = QCheck2.Gen.(oneof [ raw; framed ])

(** One way of reading a field, applied in random order. *)
type read =
  | Uint8
  | Uint16
  | Uint24
  | Uint32
  | Int64
  | Lenenc_int
  | Lenenc_string
  | Lenenc_text
  | Null_terminated
  | Rest
  | Skip of int
  | Take of int

let reads =
  QCheck2.Gen.(
    list_size (int_range 0 12)
      (oneof
         [
           oneof_list
             [
               Uint8;
               Uint16;
               Uint24;
               Uint32;
               Int64;
               Lenenc_int;
               Lenenc_string;
               Lenenc_text;
               Null_terminated;
               Rest;
             ];
           map (fun n -> Skip n) (int_range (-2) 40);
           map (fun n -> Take n) (int_range (-2) 40);
         ]))

let no_exception name gen f =
  QCheck_alcotest.to_alcotest
    (QCheck2.Test.make ~count:1000 ~name gen (fun input ->
         match f input with
         | () -> true
         | exception exn ->
             QCheck2.Test.fail_reportf "raised %s" (Printexc.to_string exn)))

let wire =
  no_exception "Wire and Cursor never raise on any bytes"
    QCheck2.Gen.(
      triple wire_input reads (list_size (int_range 0 4) (int_range 1 64)))
    (fun (input, reads, pieces) ->
      let wire = Quiver_mysql.Wire.create (Test_wire.source input pieces)
      and cursor = Quiver_mysql.Wire.Cursor.create () in
      let rec packets left =
        if left > 0 then
          match Quiver_mysql.Wire.next wire cursor with
          | Error _ -> ()
          | Ok () ->
              ignore
                (Quiver_mysql.Wire.Cursor.decode cursor (fun reader ->
                     List.iter
                       (fun read ->
                         let open Quiver_mysql.Wire.Cursor in
                         match read with
                         | Uint8 -> ignore (uint8 reader)
                         | Uint16 -> ignore (uint16 reader)
                         | Uint24 -> ignore (uint24 reader)
                         | Uint32 -> ignore (uint32 reader)
                         | Int64 -> ignore (Int64_u.to_int64 (int64 reader))
                         | Lenenc_int -> ignore (lenenc_int reader)
                         | Lenenc_string -> ignore (lenenc_string reader)
                         | Lenenc_text -> ignore (lenenc_text reader)
                         | Null_terminated -> ignore (null_terminated reader)
                         | Rest -> ignore (rest reader)
                         | Skip n -> skip reader n
                         | Take n -> ignore (take reader n))
                       reads;
                     Ok ()));
              packets (left - 1)
      in
      packets 8)

(** Which [Rows.add_*] reads the next cell. *)
type cell =
  | Small
  | Big
  | Bits
  | Double
  | Day
  | Instant
  | Unchecked_day
  | Unchecked_instant
  | Duration
  | Text

let cell (Quiver_core.Column.Any column) reader kind =
  let dates = function
    | Error `Invalid_date -> Ok ()
    | (Ok () | Error (`Column _)) as result -> result
  in
  match (kind, Quiver_core.Column.repr column) with
  | Small, Int -> Quiver_mysql.Rows.add_integer reader W2 Signed column
  | Big, Int64 -> Quiver_mysql.Rows.add_int64 reader column
  | Bits, Int64 -> Quiver_mysql.Rows.add_bits reader column
  | Double, Float -> Quiver_mysql.Rows.add_float64 reader column
  | Day, Int -> dates (Quiver_mysql.Rows.add_date reader column)
  | Instant, Int64 -> dates (Quiver_mysql.Rows.add_datetime reader column)
  | Unchecked_day, Int ->
      dates (Quiver_mysql.Rows.add_unchecked_date reader column)
  | Unchecked_instant, Int64 ->
      dates (Quiver_mysql.Rows.add_unchecked_datetime reader column)
  | Duration, Int64 -> Quiver_mysql.Rows.add_time reader column
  | Text, Text -> Quiver_mysql.Rows.add_text reader column
  | _ -> Ok ()

let ltype : cell -> Quiver_core.Ltype.t = function
  | Small | Day -> Int32
  | Unchecked_day -> Unchecked_date
  | Unchecked_instant -> Unchecked_datetime Quiver_core.Ltype.Fsp.microseconds
  | Big | Bits | Instant | Duration -> Int64
  | Double -> Float64
  | Text -> String

let rows =
  no_exception "binary row cells never raise on any bytes"
    QCheck2.Gen.(
      pair wire_input
        (list_size (int_range 1 10)
           (oneof_list
              [
                Small;
                Big;
                Bits;
                Double;
                Day;
                Instant;
                Unchecked_day;
                Unchecked_instant;
                Duration;
                Text;
              ])))
    (fun (input, kinds) ->
      let wire = Quiver_mysql.Wire.create (Test_wire.source input [])
      and cursor = Quiver_mysql.Wire.Cursor.create () in
      match Quiver_mysql.Wire.next wire cursor with
      | Error _ -> ()
      | Ok () ->
          ignore
            (Quiver_mysql.Wire.Cursor.decode cursor (fun reader ->
                 Base.Result.all
                   (List.map
                      (fun kind ->
                        Result.map_error
                          (fun (`Column _) -> `Wire Quiver_mysql.Wire.Malformed)
                          (cell
                             (Quiver_core.Column.create (ltype kind))
                             reader kind))
                      kinds))))

let gtid =
  no_exception "GTID parsing never raises"
    QCheck2.Gen.(
      pair
        (oneof_list [ "8.0.46"; "8.4.11"; "11.8.9-MariaDB" ])
        (string_size
           ~gen:
             (oneof_list
                [ 'a'; 'f'; '0'; '9'; ':'; '-'; ','; ' '; '\n'; 'x'; '_' ])
           (int_range 0 80)))
    (fun (version, text) ->
      match Quiver_mysql.Server.of_version_string version with
      | Error _ -> ()
      | Ok server -> ignore (Quiver_mysql.Gtid.of_server_string server text))

let myloader =
  no_exception "myloader text lexer never raises" raw (fun text ->
      ignore (Quiver_sink.Myloader.statement Backslash_escapes text);
      ignore (Quiver_sink.Myloader.statement No_backslash_escapes text))

let greeting =
  no_exception "handshake against a server sending any bytes never raises"
    wire_input (fun reply ->
      ignore ();
      let listener = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
      Unix.setsockopt listener SO_REUSEADDR true;
      Unix.bind listener (ADDR_INET (Unix.inet_addr_loopback, 0));
      Unix.listen listener 1;
      let port =
        match Unix.getsockname listener with
        | ADDR_INET (_, port) -> port
        | ADDR_UNIX _ -> 0
      in
      let server =
        Thread.create
          (fun () ->
            let client, _ = Unix.accept listener in
            (try
               ignore
                 (Unix.write_substring client reply 0 (String.length reply))
             with Unix.Unix_error _ -> ());
            Unix.shutdown client SHUTDOWN_SEND;
            (try
               let scratch = Bytes.create 4096 in
               while Unix.read client scratch 0 4096 > 0 do
                 ()
               done
             with Unix.Unix_error _ -> ());
            Unix.close client)
          ()
      in
      ignore
        (Quiver_mysql.Link.connect ~local_data:Without_local_data ~tls:Plaintext
           ~timeout:(Core.Time_ns.Span.of_int_sec 5)
           ~host:"127.0.0.1" ~port ~user:"root" ~password:(Some "x"));
      Thread.join server;
      Unix.close listener)

let tests = [ wire; rows; gtid; myloader; greeting ]
