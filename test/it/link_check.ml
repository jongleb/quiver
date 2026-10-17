let () =
  let port = int_of_string Sys.argv.(1)
  and tls =
    match Sys.argv.(2) with
    | "verified" -> Quiver_mysql.Link.Verified_tls System_roots
    | _ -> Quiver_mysql.Link.Plaintext
  in
  let failure = function
    | `Link error ->
        (fun value ->
          Sexplib0.Sexp.to_string_hum (Quiver_mysql.Link.sexp_of_error value))
          error
    | `Wire error ->
        (fun value ->
          Sexplib0.Sexp.to_string_hum (Quiver_mysql.Wire.sexp_of_error value))
          error
    | `Transport error ->
        (fun value ->
          Sexplib0.Sexp.to_string_hum
            (Quiver_mysql.Transport.sexp_of_error value))
          error
  in
  let row cells =
    String.concat "|"
      (Array.to_list (Array.map (Option.value ~default:"NULL") cells))
  in
  ignore ();
  match
    Quiver_mysql.Link.connect ~local_data:Without_local_data ~tls
      ~timeout:(Core.Time_ns.Span.of_int_sec 30)
      ~host:"127.0.0.1" ~port ~user:"root" ~password:(Some "quiver")
  with
  | Error error ->
      prerr_endline ("connect: " ^ failure error);
      exit 1
  | Ok link -> (
      Format.printf "%a@."
        (fun ppf value ->
          Sexplib0.Sexp.pp_hum ppf (Quiver_mysql.Server.sexp_of_t value))
        (Quiver_mysql.Link.server link);
      List.iter
        (fun sql ->
          match Quiver_mysql.Link.query link sql with
          | Ok rows -> List.iter (fun cells -> print_endline (row cells)) rows
          | Error error -> print_endline ("error: " ^ failure error))
        [
          "SELECT 1, NULL, 'x''y', REPEAT('z', 300) = REPEAT('z', 300)";
          "SELECT COUNT(*) > 0 FROM information_schema.schemata";
          "SET @a = 1";
          "SELECT no_such_column";
          "SELECT @a";
        ];
      Quiver_mysql.Link.close link;
      match
        Quiver_mysql.Link.connect ~local_data:Without_local_data ~tls
          ~timeout:(Core.Time_ns.Span.of_int_sec 30)
          ~host:"127.0.0.1" ~port ~user:"root" ~password:(Some "wrong")
      with
      | Ok _ -> print_endline "wrong password accepted"
      | Error error -> print_endline ("wrong password: " ^ failure error))
