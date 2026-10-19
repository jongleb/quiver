type column = { sql_type : string; literal : string QCheck2.Gen.t }
(** A column of a generated table: its SQL type and literals of its values. *)

(** Dates a generated table holds: calendar-valid ones only, or also MySQL's
    zero and invalid ones ([0000-00-00], [2020-02-30]). *)
type dates = Valid_dates | Any_dates

(** Where a generated column goes: into the key, or anywhere. *)
type place = Key | Anywhere

(** Who restores a dump: the server's own client or [quiver load]. *)
type restore = Client | Loader

(** A generated column [g]; with one, MariaDB's CHECKSUM TABLE depends on how
    rows were inserted, not only on them, so only rows are compared then. *)
type generated = No_generated | Virtual | Stored

type case = {
  create : string;
  rows : string list list;
  generated : generated;
  dates : dates;
  zero_dates : Quiver_mysql.Provider.zero_dates;
  format : Quiver_core.Output_format.t;
  restore : restore;
  flags : string list;
}
(** A generated table, its rows as SQL literals, and how it is dumped. *)

let column dates place =
  let open QCheck2.Gen in
  let text value =
    Printf.sprintf "_utf8mb4 X'%s'" (Hex_encode.to_hex ~case:`Lowercase value)
  and quote label =
    "'"
    ^ Base.String.substr_replace_all
        (Base.String.substr_replace_all label ~pattern:"\\" ~with_:"\\\\")
        ~pattern:"'" ~with_:"''"
    ^ "'"
  and digits n = string_size ~gen:(char_range '0' '9') (pure n)
  and utf8 =
    [
      "a";
      "Z";
      " ";
      "'";
      "\"";
      "\\";
      "\000";
      "\n";
      "\r";
      "\t";
      "\026";
      ",";
      "%";
      "é";
      "Ω";
      "✓";
      "😀";
    ]
  in
  let fraction fsp =
    match fsp with 0 -> pure "" | _ -> map (( ^ ) ".") (digits fsp)
  and date =
    oneof
      ((match dates with
         | Valid_dates -> []
         | Any_dates ->
             [
               oneof_list [ (0, 0, 0); (2020, 2, 30); (2020, 0, 15); (0, 1, 1) ];
               triple (int_range 0 9999) (int_range 0 12) (int_range 0 31);
             ])
      @ [
          oneof_list
            [
              (1000, 1, 1);
              (9999, 12, 31);
              (1970, 1, 1);
              (1969, 12, 31);
              (2000, 2, 29);
            ];
          (let* year = int_range 1000 9999
           and* month = int_range 1 12
           and* day = int_range 1 31 in
           pure
             ( year,
               month,
               Option.fold ~none:28
                 ~some:(fun _ -> day)
                 (Ptime.of_date (year, month, day)) ));
        ])
  and clock =
    oneof
      [
        oneof_list [ (0, 0, 0); (23, 59, 59) ];
        triple (int_range 0 23) (int_range 0 59) (int_range 0 59);
      ]
  and sized ~short ~long =
    [
      map
        (fun n -> (Printf.sprintf "%s(%d)" short n, int_range 0 n))
        (int_range 1 10);
      map
        (fun n -> (Printf.sprintf "%s(%d)" long n, int_range 0 n))
        (int_range 1 40);
    ]
  and huge low high =
    oneof_weighted [ (3, int_range 0 60); (1, int_range low high) ]
  in
  let instant (year, month, day) (hours, minutes, seconds) fraction =
    Printf.sprintf "'%04d-%02d-%02d %02d:%02d:%02d%s'" year month day hours
      minutes seconds fraction
  and json_string =
    map
      (fun pieces ->
        "\""
        ^ String.concat ""
            (List.map
               (function
                 | "\"" -> "\\\""
                 | "\\" -> "\\\\"
                 | "\n" -> "\\n"
                 | "\r" -> "\\r"
                 | "\t" -> "\\t"
                 | "\000" -> "\\u0000"
                 | "\026" -> "\\u001a"
                 | piece -> piece)
               pieces)
        ^ "\"")
      (list_size (int_range 0 6) (oneof_list utf8))
  and real sql_type edges round =
    {
      sql_type;
      literal =
        map (Printf.sprintf "%.17g")
          (oneof
             [
               oneof_list edges;
               map
                 (fun x ->
                   match Float.classify_float (round x) with
                   | FP_normal | FP_subnormal | FP_zero -> round x
                   | FP_infinite | FP_nan -> 0.5)
                 (oneof [ float; float_range (-1000.) 1000. ]);
             ]);
    }
  in
  let float32 x = Int32.float_of_bits (Int32.bits_of_float x) in
  let integer =
    let* name, bits =
      oneof_list
        [
          ("TINYINT", 8);
          ("SMALLINT", 16);
          ("MEDIUMINT", 24);
          ("INT", 32);
          ("BIGINT", 64);
        ]
    and* signedness = oneof_list [ `Signed; `Unsigned ] in
    let wide =
      oneof [ oneof_list [ Int64.min_int; Int64.max_int; 0L; 1L; -1L ]; int64 ]
    in
    let literal =
      match (bits, signedness) with
      | 64, `Signed -> map (Printf.sprintf "%Ld") wide
      | 64, `Unsigned -> map (Printf.sprintf "%Lu") wide
      | _, `Signed ->
          let high = (1 lsl (bits - 1)) - 1 in
          map string_of_int
            (oneof
               [
                 oneof_list [ -high - 1; high; 0; 1; -1 ];
                 int_range (-high - 1) high;
               ])
      | _, `Unsigned ->
          let high = (1 lsl bits) - 1 in
          map string_of_int
            (oneof [ oneof_list [ 0; 1; high ]; int_range 0 high ])
    in
    pure
      {
        sql_type =
          (match signedness with
          | `Signed -> name
          | `Unsigned -> name ^ " UNSIGNED");
        literal;
      }
  and decimal =
    let* precision = int_range 1 65 in
    let+ scale = int_range 0 (Int.min 30 precision) in
    {
      sql_type = Printf.sprintf "DECIMAL(%d,%d)" precision scale;
      literal =
        (let* sign = oneof_list [ ""; "-" ]
         and* whole = int_range 0 (precision - scale) in
         let* integral = digits whole and* fractional = digits scale in
         pure
           (sign
           ^ (match whole with 0 -> "0" | _ -> integral)
           ^ match scale with 0 -> "" | _ -> "." ^ fractional));
    }
  and characters =
    let* charset, pool =
      oneof_list
        [
          ("CHARACTER SET utf8mb4 COLLATE utf8mb4_bin", utf8);
          ("CHARACTER SET utf8mb4 COLLATE utf8mb4_general_ci", utf8);
          ("CHARACTER SET utf8mb4 COLLATE utf8mb4_unicode_ci", utf8);
          ( "CHARACTER SET latin1",
            [
              "a";
              "Z";
              " ";
              "'";
              "\"";
              "\\";
              "\000";
              "\n";
              "\r";
              "\t";
              "\026";
              ",";
              "%";
              "é";
              "ü";
              "å";
            ] );
        ]
    and* sql_type, length =
      oneof
        (match place with
        | Key -> sized ~short:"CHAR" ~long:"VARCHAR"
        | Anywhere ->
            oneof_list
              [
                ("TEXT", int_range 0 60);
                ("TINYTEXT", int_range 0 60);
                ("MEDIUMTEXT", huge 5000 40000);
              ]
            :: sized ~short:"CHAR" ~long:"VARCHAR")
    in
    pure
      {
        sql_type = sql_type ^ " " ^ charset;
        literal =
          map
            (fun pieces -> text (String.concat "" pieces))
            (list_size length (oneof_list pool));
      }
  and binary =
    let+ sql_type, length =
      oneof
        (match place with
        | Key -> sized ~short:"BINARY" ~long:"VARBINARY"
        | Anywhere ->
            oneof_list
              [ ("BLOB", int_range 0 60); ("MEDIUMBLOB", huge 10000 100000) ]
            :: sized ~short:"BINARY" ~long:"VARBINARY")
    in
    {
      sql_type;
      literal =
        map
          (fun value ->
            Printf.sprintf "X'%s'" (Hex_encode.to_hex ~case:`Lowercase value))
          (string_size
             ~gen:(oneof [ oneof_list [ '\000'; '\''; '\\'; '\255' ]; char ])
             length);
    }
  and day =
    pure
      {
        sql_type = "DATE";
        literal =
          map
            (fun (year, month, day) ->
              Printf.sprintf "'%04d-%02d-%02d'" year month day)
            date;
      }
  and datetime =
    let+ fsp = int_range 0 6 in
    {
      sql_type = Printf.sprintf "DATETIME(%d)" fsp;
      literal =
        (let* date = date and* clock = clock and* fraction = fraction fsp in
         pure (instant date clock fraction));
    }
  and timestamp =
    let+ fsp = int_range 0 6 in
    {
      sql_type = Printf.sprintf "TIMESTAMP(%d)" fsp;
      literal =
        (let* seconds =
           oneof [ oneof_list [ 1; 2147483647 ]; int_range 1 2147483647 ]
         and* fraction = fraction fsp in
         let* zero =
           match dates with
           | Valid_dates -> pure `Instant
           | Any_dates -> oneof_weighted [ (4, pure `Instant); (1, pure `Zero) ]
         in
         let date, ((hours, minutes, seconds), _) =
           Ptime.to_date_time
             (Option.get (Ptime.of_float_s (Float.of_int seconds)))
         in
         pure
           (match zero with
           | `Instant -> instant date (hours, minutes, seconds) fraction
           | `Zero -> "'0000-00-00 00:00:00'"));
    }
  and time =
    let+ fsp = int_range 0 6 in
    {
      sql_type = Printf.sprintf "TIME(%d)" fsp;
      literal =
        (let* sign = oneof_list [ ""; "-" ]
         and* magnitude =
           oneof [ oneof_list [ 0; 3020399 ]; int_range 0 3020399 ]
         in
         let+ fraction =
           match magnitude with
           | 3020399 ->
               pure (match fsp with 0 -> "" | _ -> "." ^ String.make fsp '0')
           | _ -> fraction fsp
         in
         Printf.sprintf "'%s%02d:%02d:%02d%s'" sign (magnitude / 3600)
           (magnitude / 60 mod 60)
           (magnitude mod 60) fraction);
    }
  and year =
    pure
      {
        sql_type = "YEAR";
        literal =
          map string_of_int
            (oneof [ oneof_list [ 0; 1901; 2155 ]; int_range 1901 2155 ]);
      }
  and bit =
    let+ width = int_range 1 64 in
    {
      sql_type = Printf.sprintf "BIT(%d)" width;
      literal =
        map
          (fun value ->
            Printf.sprintf "b'%s'"
              (String.init width (fun i ->
                   match
                     Int64.logand 1L
                       (Int64.shift_right_logical value (width - 1 - i))
                   with
                   | 0L -> '0'
                   | _ -> '1')))
          (oneof [ oneof_list [ 0L; -1L ]; int64 ]);
    }
  and enum =
    let+ members =
      map
        (List.sort_uniq String.compare)
        (list_size (int_range 1 5)
           (oneof_list [ "a"; "b'c"; "Ω"; "x y"; "q,r"; "back\\slash"; "1" ]))
    in
    {
      sql_type = "ENUM(" ^ String.concat "," (List.map quote members) ^ ")";
      literal =
        map text (oneof_weighted [ (9, oneof_list members); (1, pure "") ]);
    }
  and set =
    let+ members =
      map
        (List.sort_uniq String.compare)
        (list_size (int_range 1 5)
           (oneof_list [ "a"; "b'c"; "Ω"; "x y"; "back\\slash"; "1" ]))
    in
    {
      sql_type = "SET(" ^ String.concat "," (List.map quote members) ^ ")";
      literal =
        map
          (fun picked ->
            text
              (String.concat ","
                 (List.filter (fun member -> List.mem member picked) members)))
          (list_size (int_range 0 3) (oneof_list members));
    }
  and json =
    pure
      {
        sql_type = "JSON";
        literal =
          map text
            (sized_size (int_range 0 3)
               (fix (fun self depth ->
                    let leaf =
                      oneof
                        [
                          oneof_list
                            [
                              "null";
                              "true";
                              "false";
                              "0";
                              "-1";
                              "0.1";
                              "1e308";
                              "-2.5e-300";
                              "18446744073709551615";
                              "-9223372036854775808";
                            ];
                          map string_of_int int;
                          json_string;
                        ]
                    in
                    match depth with
                    | 0 -> leaf
                    | _ ->
                        oneof
                          [
                            leaf;
                            map
                              (fun items -> "[" ^ String.concat "," items ^ "]")
                              (list_size (int_range 0 3) (self (depth - 1)));
                            map
                              (fun members ->
                                "{"
                                ^ String.concat ","
                                    (List.map
                                       (fun (key, value) -> key ^ ":" ^ value)
                                       members)
                                ^ "}")
                              (list_size (int_range 0 3)
                                 (pair json_string (self (depth - 1))));
                          ])));
      }
  in
  let splittable =
    [ integer; decimal; characters; binary; day; datetime; timestamp; time ]
  and whole =
    [
      year;
      bit;
      enum;
      pure
        (real "FLOAT"
           [
             0.;
             -0.;
             3.4028234663852886e38;
             -3.4028234663852886e38;
             1.1754943508222875e-38;
             1.401298464324817e-45;
             float32 0.1;
           ]
           float32);
      pure
        (real "DOUBLE"
           [
             0.;
             -0.;
             Float.max_float;
             -.Float.max_float;
             Float.min_float;
             4.9406564584124654e-324;
             0.1;
           ]
           Fun.id);
    ]
  in
  match place with
  | Key -> oneof_weighted [ (9, oneof splittable); (1, oneof whole) ]
  | Anywhere -> oneof ((set :: json :: splittable) @ whole)

let case snapshots =
  let open QCheck2.Gen in
  let* dates = oneof_list [ Valid_dates; Any_dates ] in
  let* keyed =
    list_size
      (oneof_weighted [ (1, pure 0); (4, int_range 1 3) ])
      (column dates Key)
  in
  let* rest =
    list_size
      (int_range (match keyed with [] -> 1 | _ -> 0) 6)
      (column dates Anywhere)
  and* constraint_ = oneof_list [ "PRIMARY KEY"; "UNIQUE KEY" ]
  and* threads = int_range 1 4
  and* render_threads = int_range 1 2
  and* chunk_bytes =
    oneof_weighted [ (4, oneof_list [ 512; 1024; 4096 ]); (1, pure 1_000_000) ]
  and* snapshot = oneof_list snapshots
  and* compression =
    oneof_list [ []; [ "--compress"; "zstd"; "--zstd-level"; "1" ] ]
  and* format =
    oneof_list
      [ Quiver_core.Output_format.Sql; Csv Rfc4180; Csv Mysql_load_data ]
  and* restore = oneof_list [ Client; Loader ]
  and* zero_dates = oneof_list Quiver_mysql.Provider.all_of_zero_dates
  and* batch_bytes = oneof_list [ 1; 300; 4096; 262144 ]
  and* generated =
    oneof_weighted
      [ (6, pure No_generated); (1, pure Virtual); (1, pure Stored) ]
  in
  let name i = Printf.sprintf "c%d" i in
  let definitions =
    List.mapi
      (fun i { sql_type; _ } ->
        Printf.sprintf "%s %s NOT NULL" (name i) sql_type)
      keyed
    @ List.mapi
        (fun i { sql_type; _ } ->
          Printf.sprintf "%s %s NULL" (name (List.length keyed + i)) sql_type)
        rest
  and key =
    match keyed with
    | [] -> []
    | _ ->
        [
          Printf.sprintf "%s (%s)" constraint_
            (String.concat "," (List.mapi (fun i _ -> name i) keyed));
        ]
  in
  let+ rows =
    list_size (int_range 0 60)
      (flatten_list
         (List.map (fun { literal; _ } -> literal) keyed
         @ List.map
             (fun { literal; _ } ->
               map (Option.value ~default:"NULL") (option ~ratio:0.85 literal))
             rest))
  in
  {
    create =
      Printf.sprintf "CREATE TABLE quiver_pbt.t (%s)"
        (String.concat ", "
           (definitions
           @ (match generated with
             | No_generated -> []
             | Virtual -> [ "g INT AS (1 + 1) VIRTUAL" ]
             | Stored -> [ "g INT AS (1 + 1) STORED" ])
           @ key));
    rows;
    generated;
    dates;
    zero_dates;
    format;
    restore;
    flags =
      [
        "--zero-dates";
        Quiver_mysql.Provider.string_of_zero_dates zero_dates;
      ]
      @ (match format with
        | Sql -> [ "--format"; "sql" ]
        | Csv dialect ->
            [
              "--format";
              "csv";
              "--csv-dialect";
              Quiver_core.Output_format.string_of_dialect dialect;
            ])
      @ [
          "--batch-bytes";
          string_of_int batch_bytes;
          "--snapshot";
          snapshot;
          "-t";
          string_of_int threads;
          "--render-threads";
          string_of_int render_threads;
          "--chunk-bytes";
          string_of_int chunk_bytes;
        ]
      @ compression;
  }

let () =
  let quiver = Sys.argv.(1)
  and compose = Sys.argv.(2)
  and service = Sys.argv.(3)
  and client = Sys.argv.(4)
  and port = Sys.argv.(5)
  and tls = Sys.argv.(6)
  and snapshots = String.split_on_char ',' Sys.argv.(7)
  and count = int_of_string Sys.argv.(8)
  and out = Sys.argv.(9) in
  let failure error =
    Sexplib0.Sexp.to_string_hum
      ([%sexp_of:
         [ `Link of Quiver_mysql.Link.error
         | `Wire of Quiver_mysql.Wire.error
         | `Transport of Quiver_mysql.Transport.error ]]
         error)
  in
  let link =
    match
      Quiver_mysql.Link.connect ~local_data:Without_local_data
        (Tcp
           {
             host = "127.0.0.1";
             port = int_of_string port;
             tls =
               (match tls with
               | "verified" -> Verified_tls System_roots
               | _ -> Plaintext);
           })
        ~timeouts:
        { connecting = Core.Time_ns.Span.of_int_sec 30; silence = Core.Time_ns.Span.of_int_sec 30 }
        ~user:"root" ~password:(Some "quiver")
    with
    | Ok link -> link
    | Error error ->
        prerr_endline ("connect: " ^ failure error);
        exit 1
  in
  let query sql =
    match Quiver_mysql.Link.query link sql with
    | Ok rows -> rows
    | Error error -> QCheck2.Test.fail_reportf "%s@.%s" sql (failure error)
  in
  let run ~dir ?(stdin = "") args =
    let input = Filename.temp_file "quiver-it" ".in"
    and output = Filename.temp_file "quiver-it" ".out" in
    Out_channel.with_open_bin input (fun channel ->
        Out_channel.output_string channel stdin);
    let reading = Unix.openfile input [ O_RDONLY ] 0
    and writing = Unix.openfile output [ O_WRONLY; O_TRUNC ] 0 in
    let status =
      match
        Unix.create_process (List.hd args) (Array.of_list args) reading writing
          writing
      with
      | pid -> Some (snd (Unix.waitpid [] pid))
      | exception Unix.Unix_error _ -> None
    in
    Unix.close reading;
    Unix.close writing;
    let printed = In_channel.with_open_bin output In_channel.input_all in
    Sys.remove input;
    Sys.remove output;
    match status with
    | Some (WEXITED 0) -> Ok ()
    | Some _ | None ->
        Error
          (Printf.sprintf "%s\n%s\ndump in %s" (String.concat " " args) printed
             dir)
  in
  let must = function
    | Ok () -> ()
    | Error output -> QCheck2.Test.fail_reportf "%s" output
  in
  let state generated database =
    ( List.sort compare (query ("SELECT * FROM " ^ database ^ ".t")),
      match generated with
      | Virtual | Stored -> []
      | No_generated ->
          List.map
            (fun cells -> cells.(1))
            (query ("CHECKSUM TABLE " ^ database ^ ".t")) )
  in
  let print { create; rows; flags; restore; _ } =
    String.concat "\n"
      [
        create ^ ";";
        "INSERT IGNORE INTO quiver_pbt.t ("
        ^ String.concat ","
            (List.mapi
               (fun i _ -> Printf.sprintf "c%d" i)
               (Option.value ~default:[] (List.nth_opt rows 0)))
        ^ ") VALUES";
        String.concat ",\n"
          (List.map (fun row -> "(" ^ String.concat ", " row ^ ")") rows)
        ^ ";";
        "quiver dump " ^ String.concat " " flags;
        (match restore with
        | Client -> "restored by the server's client"
        | Loader -> "restored by quiver load");
      ]
  and show (rows, checksum) =
    String.concat "\n"
      (List.map
         (fun cells ->
           String.concat ", "
             (Array.to_list
                (Array.map
                   (Option.fold ~none:"NULL" ~some:(Printf.sprintf "%S"))
                   cells)))
         rows
      @ [ "checksum " ^ String.concat "" (List.filter_map Fun.id checksum) ])
  in
  Option.iter
    (fun seed -> QCheck_base_runner.set_seed (int_of_string seed))
    (Sys.getenv_opt "QUIVER_PBT_SEED");
  ignore (query "SET time_zone = '+00:00'");
  ignore (query "SET GLOBAL local_infile = 1");
  let index = ref 0 in
  let test =
    QCheck2.Test.make ~count ~print
      ~name:(service ^ ": random tables dump and restore unchanged")
      (case snapshots)
      (fun
        { create; rows; generated; dates; zero_dates; format; restore; flags }
      ->
        incr index;
        List.iter
          (fun sql -> ignore (query sql))
          ([
             "DROP DATABASE IF EXISTS quiver_pbt";
             "DROP DATABASE IF EXISTS quiver_pbt_back";
             "CREATE DATABASE quiver_pbt CHARACTER SET utf8mb4";
             create;
           ]
          @ (match rows with
            | [] -> []
            | row :: _ ->
                (match dates with
                  | Valid_dates -> []
                  | Any_dates ->
                      [ "SET SESSION sql_mode = 'ALLOW_INVALID_DATES'" ])
                @ [
                    "INSERT IGNORE INTO quiver_pbt.t ("
                    ^ String.concat ","
                        (List.mapi (fun i _ -> Printf.sprintf "c%d" i) row)
                    ^ ") VALUES "
                    ^ String.concat ","
                        (List.map
                           (fun row -> "(" ^ String.concat "," row ^ ")")
                           rows);
                    "SET SESSION sql_mode = DEFAULT";
                  ])
          @ [ "ANALYZE TABLE quiver_pbt.t" ]);
        let before = state generated "quiver_pbt" in
        let dir = Filename.concat out (Printf.sprintf "case-%d" !index) in
        must (run ~dir [ "rm"; "-rf"; dir ]);
        match
          run ~dir
            ([
               quiver;
               "dump";
               "--tls";
               tls;
               "-P";
               port;
               "-p";
               "quiver";
               "-B";
               "quiver_pbt";
               "-o";
               dir;
             ]
            @ flags)
        with
        | Error output -> (
            match (zero_dates, dates) with
            | Reject, Any_dates
              when Base.String.is_substring output ~substring:"Invalid_date" ->
                true
            | (Reject | Keep), (Valid_dates | Any_dates) ->
                QCheck2.Test.fail_reportf "%s" output)
        | Ok () ->
            (let manifest =
               Stdio.In_channel.read_all (Filename.concat dir "quiver.json")
             in
             match
               (String.is_valid_utf_8 manifest, Yojson.Safe.from_string manifest)
             with
             | true, _ -> ()
             | false, _ ->
                 QCheck2.Test.fail_reportf "quiver.json is not UTF-8 in %s" dir
             | exception Yojson.Json_error error ->
                 QCheck2.Test.fail_reportf "quiver.json in %s: %s" dir error);
            (match restore with
            | Loader -> ()
            | Client ->
                ignore
                  (query "CREATE DATABASE quiver_pbt_back CHARACTER SET utf8mb4"));
            let client ?stdin extra =
              must
              @@ run ~dir ?stdin
                   ([
                      "docker";
                      "compose";
                      "-p";
                      "quiver";
                      "-f";
                      compose;
                      "exec";
                      "-T";
                      "-e";
                      "MYSQL_PWD=quiver";
                      service;
                      client;
                      "-uroot";
                      "--default-character-set=utf8mb4";
                      "--local-infile=1";
                    ]
                   @ extra @ [ "quiver_pbt_back" ])
            and files =
              List.filter_map
                (fun file ->
                  let contents () =
                    let raw =
                      Stdio.In_channel.read_all (Filename.concat dir file)
                    in
                    match Filename.extension file with
                    | ".zst" ->
                        Bytesrw.Bytes.Reader.to_string
                          (Bytesrw_zstd.decompress_reads ~all_frames:true ()
                             (Bytesrw.Bytes.Reader.of_string raw))
                    | _ -> raw
                  in
                  match
                    ( Base.String.is_substring file ~substring:"-schema-create.",
                      Base.String.is_substring file ~substring:"-schema.",
                      Filename.extension
                        (match Filename.extension file with
                        | ".zst" -> Filename.remove_extension file
                        | _ -> file) )
                  with
                  | true, _, _ -> None
                  | false, true, _ -> Some (`Schema, contents ())
                  | false, false, (".sql" | ".csv") -> Some (`Data, contents ())
                  | false, false, _ -> None)
                (List.sort String.compare (Array.to_list (Sys.readdir dir)))
            in
            let data =
              List.filter_map
                (function
                  | `Data, contents -> Some contents | `Schema, _ -> None)
                files
            and schema =
              String.concat ""
                (List.filter_map
                   (function
                     | `Schema, contents -> Some contents | `Data, _ -> None)
                   files)
            in
            (match (restore, format) with
            | Client, Sql -> client ~stdin:(schema ^ String.concat "" data) []
            | Loader, (Sql | Csv _) ->
                must
                  (run ~dir
                     [
                       quiver;
                       "load";
                       "--tls";
                       tls;
                       "-P";
                       port;
                       "-p";
                       "quiver";
                       "-d";
                       dir;
                       "-B";
                       "quiver_pbt_back";
                       "-t";
                       "2";
                     ])
            | Client, Csv dialect ->
                client ~stdin:schema [];
                let value i =
                  match dialect with
                  | Rfc4180 -> Printf.sprintf {|NULLIF(@c%d, '\\N')|} i
                  | Mysql_load_data -> Printf.sprintf "@c%d" i
                in
                let columns =
                  List.mapi
                    (fun i cells ->
                      match cells with
                      | [| Some name; Some data_type |] ->
                          ( Printf.sprintf "@c%d" i,
                            Printf.sprintf "`%s` = %s" name
                              (match data_type with
                              | "binary" | "varbinary" | "tinyblob" | "blob"
                              | "mediumblob" | "longblob" ->
                                  "UNHEX(" ^ value i ^ ")"
                              | "bit" -> "CAST(" ^ value i ^ " AS UNSIGNED)"
                              | _ -> value i) )
                      | _ -> QCheck2.Test.fail_reportf "unexpected column row")
                    (query
                       "SELECT COLUMN_NAME, DATA_TYPE FROM \
                        information_schema.COLUMNS WHERE TABLE_SCHEMA = \
                        'quiver_pbt_back' AND TABLE_NAME = 't' AND EXTRA NOT \
                        LIKE '%GENERATED%' ORDER BY ORDINAL_POSITION")
                in
                let statement =
                  Printf.sprintf
                    "SET SESSION sql_mode = \
                     'NO_AUTO_VALUE_ON_ZERO,ALLOW_INVALID_DATES'; SET \
                     time_zone = '+00:00'; LOAD DATA LOCAL INFILE '/dev/stdin' \
                     INTO TABLE t CHARACTER SET utf8mb4 %s (%s) SET %s"
                    (match dialect with
                    | Rfc4180 ->
                        {|FIELDS TERMINATED BY ',' OPTIONALLY ENCLOSED BY '"' ESCAPED BY '' LINES TERMINATED BY '\r\n' IGNORE 1 LINES|}
                    | Mysql_load_data ->
                        {|FIELDS TERMINATED BY ',' ENCLOSED BY '"' ESCAPED BY '\\' LINES TERMINATED BY '\n'|})
                    (String.concat "," (List.map fst columns))
                    (String.concat ", " (List.map snd columns))
                in
                List.iter
                  (fun contents ->
                    match contents with
                    | "" -> ()
                    | _ -> client ~stdin:contents [ "-e"; statement ])
                  data);
            let after = state generated "quiver_pbt_back" in
            before = after
            || QCheck2.Test.fail_reportf "before:@.%s@.after:@.%s@.dump in %s"
                 (show before) (show after) dir)
  in
  exit (QCheck_base_runner.run_tests ~verbose:true [ test ])
