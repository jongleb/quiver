open Base.Result.Let_syntax

type tls = Verified_tls of Transport.authority | Plaintext

type route =
  | Tcp of { host : string; port : int; tls : tls }
  | Socket of File_path.t
type local_data = Without_local_data | With_local_data

type timeouts = {
  connecting : Core.Time_ns.Span.t;
  silence : Core.Time_ns.Span.t;
}

type capability =
  | Protocol_41
  | Secure_connection
  | Plugin_auth
  | Deprecate_eof
  | Ssl
  | Lenenc_auth_data
[@@deriving sexp_of ~portable]

type error =
  | Unsupported_protocol of int
  | Missing_capability of capability
  | Unsupported_server of string
  | Unsupported_auth of string
  | Untrusted_certificate of string
  | Cleartext_refused
  | Unusable_public_key
  | Short_scramble
  | Refused of { code : int; sqlstate : string; message : string }
  | Unexpected_packet of int
[@@deriving sexp_of ~portable]

type t = {
  transport : Transport.t;
  wire : Wire.t;
  cursor : Wire.Cursor.t;
  server : Server.t;
}

type greeting = {
  version : string;
  capabilities : int;
  scramble : string;
  plugin : string;
}
(** The server's first packet, protocol version 10. *)

let server t = t.server
let close t = Transport.close t.transport

let receive transport wire cursor decode =
  match Wire.next wire cursor with
  | Error (`Wire Wire.Closed) ->
      Option.fold
        ~none:(Error (`Wire Wire.Closed))
        ~some:(fun error -> Error (`Transport error))
        (Transport.failure transport)
  | Error error -> Error error
  | Ok () -> Wire.Cursor.decode cursor decode

let send transport wire payload =
  Transport.write transport (Wire.frame wire payload)

let refused reader =
  let code = Wire.Cursor.uint16 reader in
  let sqlstate =
    match Wire.Cursor.remaining reader with
    | 0 -> ""
    | _ -> (
        let sqlstate_length = 5 in
        match Char.chr (Wire.Cursor.uint8 reader) with
        | '#' -> Wire.Cursor.string reader sqlstate_length
        | _ -> "")
  in
  `Link (Refused { code; sqlstate; message = Wire.Cursor.rest reader })

let connect ~local_data route ~timeouts ~user ~password =
  let endpoint : Transport.endpoint =
    match route with Tcp { host; port; tls = _ } -> Tcp { host; port } | Socket path -> Socket path
  in
  let%bind transport = Transport.connect ~timeout:timeouts.connecting endpoint in
  let wire = Wire.create (Transport.read transport)
  and cursor = Wire.Cursor.create () in
  let receive decode = receive transport wire cursor decode
  and send payload = send transport wire payload
  and plugin_of_name = function
    | "mysql_native_password" -> Ok `Native
    | "caching_sha2_password" -> Ok `Caching_sha2
    | "sha256_password" -> Ok `Sha256
    | other -> Error (`Link (Unsupported_auth other))
  and without_nul text = Base.String.rstrip ~drop:(Char.equal '\000') text in
  let answer channel plugin scramble =
    let xor a b =
      String.init (String.length a) (fun i ->
          Char.chr (Char.code a.[i] lxor Char.code b.[i mod String.length b]))
    in
    match (plugin, password, channel) with
    | (`Native | `Caching_sha2), (None | Some ""), _ -> Ok (`Send "")
    | `Sha256, (None | Some ""), _ -> Ok (`Send "\000")
    | (`Native | `Caching_sha2), Some (_ : string), (`Secure | `Local | `Plaintext)
    | `Sha256, Some (_ : string), `Local
      when String.length scramble < 20 ->
        Error (`Link Short_scramble)
    | `Native, Some password, _ ->
        let once = Quiver_openssl.Sha.sha1 password in
        Ok (`Send (xor once (Quiver_openssl.Sha.sha1 (String.sub scramble 0 20 ^ Quiver_openssl.Sha.sha1 once))))
    | `Caching_sha2, Some password, _ ->
        let once = Quiver_openssl.Sha.sha256 password in
        Ok (`Send (xor once (Quiver_openssl.Sha.sha256 (Quiver_openssl.Sha.sha256 once ^ String.sub scramble 0 20))))
    | `Sha256, Some password, `Secure -> Ok (`Send (password ^ "\000"))
    | `Sha256, Some password, `Local ->
        Ok (`Ask_key (xor (password ^ "\000") (String.sub scramble 0 20)))
    | `Sha256, Some (_ : string), `Plaintext -> Error (`Link Cleartext_refused)
  and opening = function
    | `Send auth -> (auth, `Verdict)
    | `Ask_key masked -> ("\001", `Key masked)
  in
  let handshake () =
    let%bind greeting =
      receive (fun reader ->
          let protocol_version = 10
          and connection_id = 4
          and scramble_head = 8
          and filler = 1
          and charset_and_status = 3
          and reserved = 10
          and scramble_tail = 12
          and terminator = 1 in
          let first = Wire.Cursor.uint8 reader in
          match Wire.marker first with
          | Err_packet -> Error (refused reader)
          | (Ok_packet | Auth_more_data | Local_infile | Eof_packet | Data)
            when first = protocol_version ->
              let version = Wire.Cursor.null_terminated reader in
              Wire.Cursor.skip reader connection_id;
              let head = Wire.Cursor.string reader scramble_head in
              Wire.Cursor.skip reader filler;
              let lower = Wire.Cursor.uint16 reader in
              Wire.Cursor.skip reader charset_and_status;
              let upper = Wire.Cursor.uint16 reader in
              let data = Wire.Cursor.uint8 reader in
              Wire.Cursor.skip reader reserved;
              let tail =
                Wire.Cursor.string reader
                  (Int.max (scramble_tail + terminator) (data - scramble_head))
              in
              Ok
                {
                  version;
                  capabilities = lower lor (upper lsl 16);
                  scramble = head ^ String.sub tail 0 scramble_tail;
                  plugin = without_nul (Wire.Cursor.rest reader);
                }
          | Ok_packet | Auth_more_data | Local_infile | Eof_packet | Data ->
              Error (`Link (Unsupported_protocol first)))
    in
    let%bind server =
      Result.map_error
        (fun (`Unsupported version) -> `Link (Unsupported_server version))
        (Server.of_version_string
           (Option.value ~default:greeting.version
              (Base.String.chop_prefix greeting.version ~prefix:"5.5.5-")))
    in
    let long_password = 0x1
    and long_flag = 0x4
    and protocol_41 = 0x200
    and ssl = 0x800
    and transactions = 0x2000
    and secure_connection = 0x8000
    and multi_results = 0x20000
    and ps_multi_results = 0x40000
    and plugin_auth = 0x80000
    and lenenc_auth_data = 0x200000
    and deprecate_eof = 0x1000000
    and local_files = 0x80 in
    let capabilities =
      greeting.capabilities
      land (long_password lor long_flag lor protocol_41 lor transactions
          lor secure_connection lor multi_results lor ps_multi_results
          lor plugin_auth lor lenenc_auth_data lor deprecate_eof
           lor
           match local_data with
           | With_local_data -> local_files
           | Without_local_data -> 0)
    in
    let%bind () =
      Option.fold ~none:(Ok ())
        ~some:(fun (_, capability) ->
          Error (`Link (Missing_capability capability)))
        (List.find_opt
           (fun (flag, _) -> capabilities land flag = 0)
           [
             (protocol_41, Protocol_41);
             (secure_connection, Secure_connection);
             (plugin_auth, Plugin_auth);
             (deprecate_eof, Deprecate_eof);
             (lenenc_auth_data, Lenenc_auth_data);
           ])
    in
    let fixed flags =
      let max_packet = 0x01000000l
      and utf8mb4_general_ci = 45
      and reserved = 23 in
      let out = Buffer.create 128 in
      Buffer.add_int32_le out (Int32.of_int flags);
      Buffer.add_int32_le out max_packet;
      Buffer.add_uint8 out utf8mb4_general_ci;
      Buffer.add_string out (String.make reserved '\000');
      out
    in
    let%bind channel, capabilities =
      match route with
      | Socket (_ : File_path.t) -> Ok (`Local, capabilities)
      | Tcp { tls = Plaintext; _ } -> Ok (`Plaintext, capabilities)
      | Tcp { tls = Verified_tls authority; host; _ } -> (
          match greeting.capabilities land ssl with
          | 0 -> Error (`Link (Missing_capability Ssl))
          | _ -> (
              let%bind () =
                send (Buffer.contents (fixed (capabilities lor ssl)))
              in
              let%bind trust = Transport.start_tls transport ~host authority in
              match trust with
              | Trusted | Loopback -> Ok (`Secure, capabilities lor ssl)
              | Unproven reason -> Error (`Link (Untrusted_certificate reason)))
          )
    in
    let%bind plugin = plugin_of_name greeting.plugin in
    let%bind reply = answer channel plugin greeting.scramble in
    let auth, expecting = opening reply
    and response = fixed capabilities in
    Buffer.add_string response user;
    Buffer.add_char response '\000';
    let length = String.length auth in
    let () =
      if length < 0xFB then Buffer.add_uint8 response length
      else if length < 0x10000 then (
        Buffer.add_uint8 response 0xFC;
        Buffer.add_uint16_le response length)
      else (
        Buffer.add_uint8 response 0xFD;
        Buffer.add_uint16_le response (length land 0xFFFF);
        Buffer.add_uint8 response (length lsr 16))
    in
    Buffer.add_string response auth;
    Buffer.add_string response greeting.plugin;
    Buffer.add_char response '\000';
    let%bind () = send (Buffer.contents response) in
    let rec await expecting =
      let%bind step =
        receive (fun reader ->
            let first = Wire.Cursor.uint8 reader in
            match Wire.marker first with
            | Ok_packet -> Ok `Done
            | Err_packet -> Error (refused reader)
            | Eof_packet ->
                let name = Wire.Cursor.null_terminated reader in
                Ok (`Switch (name, without_nul (Wire.Cursor.rest reader)))
            | Auth_more_data -> Ok (`More (first, Wire.Cursor.rest reader))
            | Local_infile | Data -> Error (`Link (Unexpected_packet first)))
      in
      match (step, expecting) with
      | `Done, _ -> Ok ()
      | `Switch (name, scramble), _ ->
          let%bind plugin = plugin_of_name name in
          let%bind reply = answer channel plugin scramble in
          let auth, expecting = opening reply in
          let%bind () = send auth in
          await expecting
      | `More ((_ : int), public_key), `Key masked ->
          let%bind sealed =
            Option.to_result ~none:(`Link Unusable_public_key)
              (Quiver_openssl.Rsa.encrypt ~public_key masked)
          in
          let%bind () = send sealed in
          await `Verdict
      | `More ((_ : int), "\003"), `Verdict -> await `Verdict
      | `More ((_ : int), "\004"), `Verdict -> (
          match channel with
          | `Secure | `Local ->
              let%bind () = send (Option.value ~default:"" password ^ "\000") in
              await `Verdict
          | `Plaintext -> Error (`Link Cleartext_refused))
      | `More (first, (_ : string)), `Verdict ->
          Error (`Link (Unexpected_packet first))
    in
    let%map () = await expecting in
    Transport.set_timeout transport timeouts.silence;
    { transport; wire; cursor; server }
  in
  match handshake () with
  | Ok t -> Ok t
  | Error error ->
      Transport.close transport;
      Error error

let command t payload =
  Wire.restart t.wire;
  send t.transport t.wire payload

let receive_on t decode = receive t.transport t.wire t.cursor decode

let query t sql =
  let%bind () = command t (Wire.command Query ^ sql) in
  let receive decode = receive_on t decode in
  let%bind columns =
    receive (fun reader ->
        let first = Wire.Cursor.peek reader in
        match Wire.marker first with
        | Ok_packet -> Ok 0
        | Err_packet ->
            Wire.Cursor.skip reader 1;
            Error (refused reader)
        | Local_infile -> Error (`Link (Unexpected_packet first))
        | Auth_more_data | Eof_packet | Data ->
            Ok (Wire.Cursor.lenenc_int reader))
  in
  match columns with
  | 0 -> Ok []
  | columns ->
      let rec definitions left =
        if left = 0 then Ok ()
        else
          let%bind () = receive (fun _ -> Ok ()) in
          definitions (left - 1)
      in
      let%bind () = definitions columns in
      let rec rows acc =
        let%bind row =
          receive (fun reader ->
              match Wire.marker (Wire.Cursor.peek reader) with
              | Eof_packet when Wire.Cursor.remaining reader < Wire.frame_limit
                ->
                  Ok None
              | Err_packet ->
                  Wire.Cursor.skip reader 1;
                  Error (refused reader)
              | Ok_packet | Auth_more_data | Local_infile | Eof_packet | Data ->
                  Ok
                    (Some
                       (Array.init columns (fun _ ->
                            Wire.Cursor.lenenc_text reader))))
        in
        match row with
        | None -> Ok (List.rev acc)
        | Some row -> rows (row :: acc)
      in
      rows []

let execute t sql =
  let%map _ = query t sql in
  ()

let upload t sql =
  let%bind () = command t (Wire.command Query ^ sql) in
  receive_on t (fun reader ->
      let first = Wire.Cursor.uint8 reader in
      match Wire.marker first with
      | Local_infile -> Ok ()
      | Err_packet -> Error (refused reader)
      | Ok_packet | Auth_more_data | Eof_packet | Data ->
          Error (`Link (Unexpected_packet first)))

let send_data t data = send t.transport t.wire data

let complete t =
  let%bind () = send t.transport t.wire "" in
  receive_on t (fun reader ->
      let first = Wire.Cursor.uint8 reader in
      match Wire.marker first with
      | Ok_packet -> Ok ()
      | Err_packet -> Error (refused reader)
      | Local_infile | Auth_more_data | Eof_packet | Data ->
          Error (`Link (Unexpected_packet first)))

let load_local t sql read =
  let%bind () = upload t sql in
  let buffer = Bytes.create 65536 in
  let rec stream () =
    match read buffer ~pos:0 ~len:(Bytes.length buffer) with
    | Ok 0 -> complete t
    | Ok length ->
        let%bind () = send_data t (Bytes.sub_string buffer 0 length) in
        stream ()
    | Error error ->
        ignore (complete t);
        Error error
  in
  stream ()

let run_query t typed = query t (Query.sql typed)
let run_statement t statement = execute t (Statement.sql statement)
