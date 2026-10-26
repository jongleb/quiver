let property ?(count = 1000) name ~print gen check =
  QCheck_alcotest.to_alcotest (QCheck2.Test.make ~count ~name ~print gen check)

let render f =
  let out = Quiver_core.Byte_buffer.create () in
  f out;
  Quiver_core.Byte_buffer.contents out

let server version =
  Result.get_ok (Quiver_mysql.Server.of_version_string version)

let uuid =
  QCheck2.Gen.(
    let+ hex =
      string_size
        ~gen:(oneof_list (Base.String.to_list "0123456789abcdef"))
        (pure 32)
    in
    String.concat "-"
      [
        String.sub hex 0 8;
        String.sub hex 8 4;
        String.sub hex 12 4;
        String.sub hex 16 4;
        String.sub hex 20 12;
      ])

let interval =
  QCheck2.Gen.(
    let* first = int_range 1 1_000_000
    and* width = oneof [ pure 0; int_range 0 1000 ] in
    pure
      (if width = 0 then string_of_int first
       else Printf.sprintf "%d-%d" first (first + width)))

let mysql_gtid =
  QCheck2.Gen.(
    let entry =
      let* uuid = uuid
      and* tag =
        option
          (let* first = oneof_list [ 'a'; 'z'; '_' ] in
           let+ rest =
             string_size
               ~gen:(oneof_list [ 'a'; 'k'; '0'; '9'; '_' ])
               (int_range 0 10)
           in
           String.make 1 first ^ rest)
      in
      let+ intervals = list_size (int_range 1 4) interval in
      String.concat ":" ((uuid :: Option.to_list tag) @ intervals)
    in
    map (String.concat ",") (list_size (int_range 0 4) entry))

let mariadb_gtid =
  QCheck2.Gen.(
    let entry =
      let* domain = int_range 0 0xFFFF and* server_id = int_range 0 0xFFFFFF in
      let+ sequence = int64 in
      Printf.sprintf "%d-%d-%Lu" domain server_id sequence
    in
    map (String.concat ",") (list_size (int_range 0 4) entry))

let gtid (name, version, gen) =
  property
    ("GTID " ^ name ^ " prints back what it parsed")
    ~print:Fun.id gen
    (fun text ->
      match Quiver_mysql.Gtid.of_server_string (server version) text with
      | Ok gtid -> String.equal (Quiver_mysql.Gtid.to_string gtid) text
      | Error (`Gtid error) ->
          QCheck2.Test.fail_reportf "%a"
            Sexplib0.Sexp.pp_hum
            (Quiver_mysql.Gtid.sexp_of_error error))

let date =
  QCheck2.Gen.(
    let* year = int_range 0 9999 and* month = int_range 1 12 in
    let+ day = int_range 1 28 in
    (year, month, day))

let clock =
  QCheck2.Gen.(
    let* hours = int_range 0 23 and* minutes = int_range 0 59 in
    let+ seconds = int_range 0 59 in
    (hours, minutes, seconds))

let instant =
  QCheck2.Gen.(
    let* d = date and* c = clock in
    let+ micros = int_range 0 999_999 in
    (d, c, micros))

let print_instant ((y, m, d), (h, mi, s), us) =
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d.%06d" y m d h mi s us

let datetime =
  property "datetime text equals Printf of its fields, for every fsp"
    ~print:(fun (moment, fsp) ->
      Printf.sprintf "%s fsp %d" (print_instant moment) fsp)
    QCheck2.Gen.(pair instant (int_range 0 6))
    (fun (((y, m, d), (h, mi, s), us), fsp) ->
      let days =
        match Quiver_core.Temporal.days_of_date ~year:y ~month:m ~day:d with
        | This days -> days
        | Null -> Alcotest.fail "a valid date refused"
      in
      let micros =
        Int64.of_int
          ((days * 86_400_000_000)
          + (((((h * 60) + mi) * 60) + s) * 1_000_000)
          + us)
      and fsp_t = Result.get_ok (Quiver_core.Ltype.Fsp.of_int fsp) in
      let ours =
        render (fun out ->
            ignore
              (Quiver_core.Temporal.datetime out fsp_t
                 ~micros:(Int64_u.of_int64 micros)))
      and reference =
        Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d" y m d h mi s
        ^
        if fsp = 0 then ""
        else "." ^ String.sub (Printf.sprintf "%06d" us) 0 fsp
      in
      String.equal ours reference
      && Option.equal Quiver_core.Moment.equal
           (Quiver_core.Temporal.parse_datetime
              (render (fun out ->
                   ignore
                     (Quiver_core.Temporal.datetime out
                        Quiver_core.Ltype.Fsp.microseconds
                        ~micros:(Int64_u.of_int64 micros)))))
           (Quiver_core.Moment.of_micros (Int64.to_int micros)))

let time =
  property "time text parses back to its microseconds" ~print:Int64.to_string
    QCheck2.Gen.(
      map Int64.of_int (int_range (-3_020_399_999_999) 3_020_399_999_999))
    (fun micros ->
      let text =
        render (fun out ->
            Quiver_core.Temporal.time out Quiver_core.Ltype.Fsp.microseconds
              ~micros:(Int64_u.of_int64 micros))
      in
      let magnitude = Int64.to_int (Int64.abs micros) in
      let seconds = magnitude / 1_000_000 in
      String.equal text
        (Printf.sprintf "%s%02d:%02d:%02d.%06d"
           (if Int64.compare micros 0L < 0 then "-" else "")
           (seconds / 3600)
           (seconds / 60 mod 60)
           (seconds mod 60) (magnitude mod 1_000_000))
      && Option.equal Int64.equal
           (Quiver_core.Temporal.parse_time text)
           (Some micros))

let digits =
  property "two digits and clock equal Printf"
    ~print:(fun (a, b, c) -> Printf.sprintf "%d %d %d" a b c)
    QCheck2.Gen.(triple (int_range 0 99) (int_range 0 99) (int_range 0 99))
    (fun (a, b, c) ->
      String.equal
        (render (fun out -> Quiver_core.Digits.two out a))
        (Printf.sprintf "%02d" a)
      && String.equal
           (render (fun out ->
                Quiver_core.Digits.clock out ~hours:a ~minutes:b ~seconds:c))
           (Printf.sprintf "%02d:%02d:%02d" a b c))

let decode name =
  let out = Buffer.create (String.length name) in
  let rec go i =
    if i < String.length name then
      match name.[i] with
      | '%' when i + 2 < String.length name + 0 ->
          Buffer.add_char out
            (Char.chr (int_of_string ("0x" ^ String.sub name (i + 1) 2)));
          go (i + 3)
      | c ->
          Buffer.add_char out c;
          go (i + 1)
  in
  go 0;
  Buffer.contents out

let file_names =
  property "file names are injective and use only safe bytes"
    ~print:(fun (a, b) -> Printf.sprintf "%S %S" a b)
    QCheck2.Gen.(
      let name =
        string_size
          ~gen:
            (oneof [ oneof_list [ '%'; '4'; '1'; 'A'; 'a'; '.'; '/' ]; char ])
          (int_range 1 12)
      in
      pair name name)
    (fun (a, b) ->
      match (Quiver_core.Ident.of_string a, Quiver_core.Ident.of_string b) with
      | Ok a', Ok b' ->
          let fa = Quiver_sink.Layout.file_name a'
          and fb = Quiver_sink.Layout.file_name b' in
          Base.String.for_all fa ~f:(function
            | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' | '$' | '-' | '%' ->
                true
            | _ -> false)
          && Bool.equal (String.equal a b) (String.equal fa fb)
          && String.equal (decode fa) a
      | Error _, _ | _, Error _ -> true)

let quoting =
  property "quoted identifiers unquote to themselves" ~print:Fun.id
    QCheck2.Gen.(string_size ~gen:(oneof [ pure '`'; char ]) (int_range 1 20))
    (fun text ->
      match Quiver_core.Ident.of_string text with
      | Error _ -> true
      | Ok name ->
          let quoted = Quiver_core.Ident.backquoted name in
          let inner = String.sub quoted 1 (String.length quoted - 2) in
          quoted.[0] = '`'
          && quoted.[String.length quoted - 1] = '`'
          && String.equal
               (Base.String.substr_replace_all inner ~pattern:"``" ~with_:"`")
               text
          && not
               (Base.String.is_substring
                  (Base.String.substr_replace_all inner ~pattern:"``" ~with_:"")
                  ~substring:"`"))

let unchecked =
  property "unchecked dates print their fields as stored"
    ~print:(fun ((y, m, d), (h, mi, sec), us, fsp) ->
      Printf.sprintf "%d-%d-%d %d:%d:%d.%d fsp %d" y m d h mi sec us fsp)
    QCheck2.Gen.(
      let* fields = triple (int_range 0 9999) (int_range 0 12) (int_range 0 31)
      and* clock = clock
      and* micros = int_range 0 999_999 in
      let+ fsp = int_range 0 6 in
      (fields, clock, micros, fsp))
    (fun ((year, month, day), (h, mi, sec), us, fsp) ->
      let date = Printf.sprintf "%04d-%02d-%02d" year month day
      and fsp_t = Result.get_ok (Quiver_core.Ltype.Fsp.of_int fsp) in
      String.equal
        (render (fun out ->
             Quiver_core.Temporal.unchecked_date out
               (Quiver_core.Temporal.pack_date ~year ~month ~day)))
        date
      && String.equal
           (render (fun out ->
                Quiver_core.Temporal.unchecked_datetime out fsp_t
                  (Quiver_core.Temporal.pack_datetime ~year ~month ~day
                     ~micros:((((((h * 60) + mi) * 60) + sec) * 1_000_000) + us))))
           (Printf.sprintf "%s %02d:%02d:%02d" date h mi sec
           ^
           if fsp = 0 then ""
           else "." ^ String.sub (Printf.sprintf "%06d" us) 0 fsp))

let fingerprint =
  property "fingerprints parse as openssl prints them and pin one digest"
    ~print:(fun (digest, other, cut) ->
      Printf.sprintf "%S %S %d" digest other cut)
    QCheck2.Gen.(
      triple (string_size (pure 32)) (string_size (pure 32)) (int_range 1 63))
    (fun (digest, other, cut) ->
      let hex = Hex_encode.to_hex ~case:`Lowercase digest in
      let colons =
        String.concat ":"
          (List.init 32 (fun i ->
               String.uppercase_ascii (String.sub hex (2 * i) 2)))
      in
      let pins text candidate =
        match Quiver_mysql.Fingerprint.of_string text with
        | Ok pinned -> (
            match Quiver_mysql.Fingerprint.check pinned ~digest:candidate with
            | `Pinned -> `Pinned
            | `Different -> `Different)
        | Error (`Fingerprint _) -> `Rejected
      in
      pins hex digest = `Pinned
      && pins colons digest = `Pinned
      && (pins hex other
         = if String.equal digest other then `Pinned else `Different)
      && pins (String.sub hex 0 cut) digest = `Rejected)

let checksum_json =
  property "checksums read back from the manifest unchanged"
    ~print:(Printf.sprintf "%Lx")
    QCheck2.Gen.(oneof [ int64; oneof_list [ 0L; -1L; Int64.min_int ] ])
    (fun bits ->
      Quiver_core.Codec.decode Quiver_core.Checksum.t_of_yojson
        (Quiver_core.Checksum.yojson_of_t (Xxh3_64 bits))
      = Ok (Xxh3_64 bits))

let unsigned_json =
  property "unsigned key bounds keep their value in the manifest"
    ~print:(Printf.sprintf "%Lu")
    QCheck2.Gen.(oneof [ int64; oneof_list [ -1L; Int64.min_int; 0L ] ])
    (fun bits ->
      Yojson.Safe.to_string (Quiver_core.Key.yojson_of_value (Unsigned bits))
      = Printf.sprintf {|["Unsigned",%Lu]|} bits)

let key_json =
  property "key bounds in the manifest are UTF-8 JSON and decode back"
    ~print:(fun (bytes, text) -> Printf.sprintf "%S %S" bytes text)
    QCheck2.Gen.(
      pair (string_size (int_range 0 20)) (string_size (int_range 0 20)))
    (fun (bytes, text) ->
      let json value =
        Yojson.Safe.to_string (Quiver_core.Key.yojson_of_value value)
      in
      String.is_valid_utf_8 (json (Binary bytes))
      && Yojson.Safe.from_string (json (Binary bytes))
         = `List
             [
               `String "Binary";
               `String (Hex_encode.to_hex ~case:`Lowercase bytes);
             ]
      && ((not (String.is_valid_utf_8 text))
         || String.is_valid_utf_8 (json (Text text))))

let table =
  QCheck2.Gen.(
    let fsp =
      map
        (fun n -> Support.get_ok (Quiver_core.Ltype.Fsp.of_int n))
        (int_range 0 6)
    in
    let ltype : Quiver_core.Ltype.t t =
      oneof
        [
          oneof_list
            [
              Quiver_core.Ltype.Bool;
              Int8;
              Int16;
              Int32;
              Int64;
              Uint8;
              Uint16;
              Uint32;
              Uint64;
              Float32;
              Float64;
              String;
              Bytes;
              Date;
              Year;
              Unchecked_date;
              Json;
              Geometry;
            ];
          map (fun f -> Quiver_core.Ltype.Time f) fsp;
          map (fun f -> Quiver_core.Ltype.Datetime f) fsp;
          map (fun f -> Quiver_core.Ltype.Timestamp f) fsp;
          map (fun f -> Quiver_core.Ltype.Unchecked_datetime f) fsp;
          (let* precision = int_range 1 65 in
           let+ scale = int_range 0 precision in
           Quiver_core.Ltype.Decimal
             (Support.get_ok
                (Quiver_core.Ltype.Decimal.make ~precision ~scale)));
          map
            (fun members -> Quiver_core.Ltype.Enum members)
            (list_small string_printable);
          map
            (fun members -> Quiver_core.Ltype.Set members)
            (list_small string_printable);
        ]
    in
    let* ltypes = list_size (int_range 1 6) ltype in
    let* written = int_range 1 (List.length ltypes) in
    let+ key = int_range 0 3 in
    let last = List.length ltypes - 1 in
    let fields =
      List.mapi
        (fun j ltype ->
          ( "c" ^ string_of_int j,
            ltype,
            if j = 0 || j = last then Quiver_core.Schema.Field.Not_null else Nullable ))
        ltypes
    in
    let #(fields, generated) = Base.List.split_n fields written in
    Support.table ~generated
      ~key:
        (match key with
        | 0 -> Primary ("c0", [])
        | 1 -> Unique { index = Support.ident "u"; fields = ("c0", []) }
        | 2 -> Primary ("c" ^ string_of_int last, [])
        | _ -> No_key)
      fields)

let table_json =
  property "table descriptions decode back from JSON, checked as built"
    ~print:(fun table ->
      Sexplib0.Sexp.to_string_hum (Quiver_core.Schema.Table.sexp_of_t table))
    table
    (fun table ->
      match
        Quiver_core.Codec.decode Quiver_core.Schema.Table.t_of_yojson
          (Quiver_core.Schema.Table.yojson_of_t table)
      with
      | Ok decoded ->
          Sexplib0.Sexp.equal
            (Quiver_core.Schema.Table.sexp_of_t decoded)
            (Quiver_core.Schema.Table.sexp_of_t table)
      | Error _ -> false)

let unchecked_json =
  Alcotest.test_case "JSON that no table or type could come from is refused"
    `Quick (fun () ->
      let decoded text =
        Result.is_ok
          (Quiver_core.Codec.decode Quiver_core.Schema.Table.t_of_yojson
             (Yojson.Safe.from_string text))
      in
      let table ltype nullability key =
        let field =
          Printf.sprintf {|{"name":"c","ltype":%s,"nullability":["%s"]}|} ltype
            nullability
        in
        Printf.sprintf
          {|{"name":{"database":"db","table":"t"},"fields":[%s,[]],"key":%s,"estimate":{"rows":0,"bytes":0}}|}
          field
          (match key with
          | `Primary -> Printf.sprintf {|["Primary",[%s,[]]]|} field
          | `No_key -> {|["No_key"]|})
      in
      Alcotest.(check bool)
        "valid" true
        (decoded (table {|["Time",6]|} "Not_null" `Primary));
      Alcotest.(check bool)
        "fsp 7" false
        (decoded (table {|["Time",7]|} "Not_null" `No_key));
      Alcotest.(check bool)
        "scale above precision" false
        (decoded
           (table {|["Decimal",{"precision":2,"scale":3}]|} "Not_null" `No_key));
      Alcotest.(check bool)
        "nullable key" false
        (decoded (table {|["Int8"]|} "Nullable" `Primary)))

let localized =
  property "a localized manifest holds the other sink's format and parts"
    ~print:(fun counts -> String.concat "," (List.map string_of_int counts))
    QCheck2.Gen.(list_size (int_range 1 4) (int_range 0 5))
    (fun counts ->
      let table n =
        Support.get_ok
          (Quiver_core.Schema.Table.make ~generated:[]
             ~name:
               {
                 database = Support.ident "db";
                 table = Support.ident ("t" ^ string_of_int n);
               }
             ~fields:
               [
                 {
                   name = Support.ident "id";
                   ltype = Int64;
                   nullability = Not_null;
                 };
               ]
             ~key:No_key ~estimate:{ rows = 0; bytes = 0 })
      in
      let tables = List.mapi (fun n _ -> table n) counts in
      let part (name : Quiver_core.Schema.Name.t) index ~mark =
        {
          Quiver_core.Manifest.file =
            File_path.Part.of_string
              (Printf.sprintf "%s.%d.%s" (name.table :> string) index mark);
          rows = index;
          bytes = 0;
          stored_bytes = 0;
          checksum = Xxh3_64 0L;
        }
      in
      let chunks =
        List.concat
          (List.map2
             (fun (table : Quiver_core.Schema.Table.t) count ->
               List.init count (fun index ->
                   ( index,
                     Quiver_core.Chunk.whole table,
                     Support.finished (part table.name index ~mark:"agent") )))
             tables counts)
      in
      let interleaved =
        List.map
          (fun (_, chunk, state) -> (chunk, state))
          (List.stable_sort (fun (a, _, _) (b, _, _) -> Int.compare a b) chunks)
      in
      let manifest =
        Quiver_core.Manifest.make ~level:(Any Snapshot) ~format:Sql
          ~position:`Null ~threads:{ sessions = 1; readers = 1; peak = 1; renderers = 1 }
          ~phases:
            {
              connected = Core.Time_ns.Span.zero;
              planned = Core.Time_ns.Span.zero;
              snapshot = Core.Time_ns.Span.zero;
              first_part = None;
              read = Core.Time_ns.Span.zero;
              committed = Core.Time_ns.Span.zero;
            }
          ~schema:(Support.get_ok (Quiver_core.Schema.make tables))
          interleaved
      in
      match
        Quiver_core.Manifest.localize
          (Quiver_core.Manifest.yojson_of_t manifest) ~format:(Csv Rfc4180)
          (fun name index -> Some (part name index ~mark:"here"))
      with
      | Error _ -> false
      | Ok json -> (
          match Quiver_core.Manifest.read json with
          | Error _ -> false
          | Ok { format; tables = read; parts } ->
              let seen = Hashtbl.create 8 in
              format = Csv Rfc4180
              && List.equal Quiver_core.Schema.Name.equal read
                   (List.map
                      (fun ({ name; _ } : Quiver_core.Schema.Table.t) -> name)
                      tables)
              && List.for_all
                   (fun ( (name : Quiver_core.Schema.Name.t),
                          (got : Quiver_core.Manifest.part) ) ->
                     let index =
                       Option.value (Hashtbl.find_opt seen name) ~default:0
                     in
                     Hashtbl.replace seen name (index + 1);
                     File_path.Part.equal got.file
                       (part name index ~mark:"here").file)
                   parts))

let calendar =
  property "dates convert to days and back as Ptime converts them"
    ~print:(fun ((year, month, day), days) ->
      Printf.sprintf "%04d-%02d-%02d, day %d" year month day days)
    QCheck2.Gen.(
      pair
        (triple (int_range (-1) 10_000) (int_range 0 13) (int_range 0 32))
        (oneof
           [
             int_range (-719_530) 2_932_898;
             oneof_list [ -719_529; -719_528; 2_932_896; 2_932_897; 0; -1 ];
           ]))
    (fun ((year, month, day), days) ->
      let reference_days =
        Option.map
          (fun moment -> fst (Ptime.Span.to_d_ps (Ptime.to_span moment)))
          (Ptime.of_date (year, month, day))
      and reference_text =
        Option.map
          (fun moment ->
            let year, month, day = Ptime.to_date moment in
            Printf.sprintf "%04d-%02d-%02d" year month day)
          (Option.bind (Ptime.Span.of_d_ps (days, 0L)) Ptime.of_span)
      in
      Option.equal Int.equal
        (match Quiver_core.Temporal.days_of_date ~year ~month ~day with
        | This days -> Some days
        | Null -> None)
        reference_days
      && Option.equal String.equal
           (let out = Quiver_core.Byte_buffer.create () in
            match Quiver_core.Temporal.date out ~days with
            | Ok () -> Some (Quiver_core.Byte_buffer.contents out)
            | Error `Unrepresentable_date -> None)
           reference_text)

let name =
  QCheck2.Gen.(
    map
      (fun pieces -> Support.ident (String.concat "" ("n" :: pieces)))
      (list_size (int_range 0 4)
         (oneof
            [
              oneof_list [ "("; ")"; "\""; " "; "."; "\\"; ";"; "#|"; "é"; "\n" ];
              string_printable;
            ])))

let messages =
  property ~count:500
    "every message reads back from the text of its S-expression"
    ~print:(fun sexp -> Sexplib0.Sexp.to_string_hum sexp)
    QCheck2.Gen.(
      let text = string_size ~gen:char (int_range 0 30)
      and count = int_range 0 1000 in
      let table_name =
        map2
          (fun database table ->
            { Quiver_core.Schema.Name.database; table })
          name name
      in
      let definition =
        let* database = name and* object_name = name and* body = text in
        let* kind =
          oneof
            [
              map2
                (fun first rest -> Quiver_core.Definition.View { columns = (first, rest) })
                name (list_small name);
              map (fun table -> Quiver_core.Definition.Trigger { table }) name;
              oneof_list [ Quiver_core.Definition.Procedure; Function; Event ];
            ]
        and* charset = name
        and* collation = name
        and* sql_mode = option text
        and* time_zone = option text in
        pure
          {
            Quiver_core.Definition.database;
            name = object_name;
            kind;
            context = { charset; collation; sql_mode; time_zone };
            text = body;
          }
      in
      let control : Quiver_net.Message.control t =
        oneof
          [
            (let* session = text and* streams = count and* chunks = count in
             let+ byte_order = oneof_list Quiver_net.Message.all_of_byte_order in
             Quiver_net.Message.Hello { session; byte_order; streams; chunks });
            map2 (fun name ddl -> Quiver_net.Message.Database { name; ddl }) name text;
            (let* table = table and* ddl = text in
             let+ parts = count in
             Quiver_net.Message.Table { table; ddl; parts });
            map (fun table -> Quiver_net.Message.Grown { table }) table_name;
            map2
              (fun database definitions -> Quiver_net.Message.Objects { database; definitions })
              name (list_small definition);
            (let* chunk = count and* table = table_name in
             let+ index = count in
             Quiver_net.Message.Begin { chunk; table; index });
            map2 (fun chunk frames -> Quiver_net.Message.End { chunk; frames }) count count;
            map (fun chunk -> Quiver_net.Message.Abort { chunk }) count;
            (let* source = text and* tables = list_small (pair table_name count) in
             let+ manifest = text in
             Quiver_net.Message.Finish { source; tables; manifest });
            map (fun replies -> Quiver_net.Message.Heartbeat { replies }) count;
            pure Quiver_net.Message.Leave;
          ]
      in
      let reply : Quiver_net.Message.reply t =
        oneof
          [
            map2
              (fun format columns -> Quiver_net.Message.Ready { format; columns })
              (oneof_list
                 [ Quiver_core.Output_format.Sql; Csv Rfc4180; Csv Mysql_load_data ])
              (oneof_list [ Quiver_net.Message.Written; Stored_too ]);
            oneof_list [ Quiver_net.Message.Done; Skipped ];
            map (fun reason -> Quiver_net.Message.Refused reason) text;
            (let* chunk = count
             and* file =
               map
                 (fun name -> File_path.Part.of_string ("p" ^ name))
                 (string_size ~gen:(char_range 'a' 'z') (int_range 0 8))
             and* rows = count
             and* bytes = count
             and* stored_bytes = count in
             let+ digest = oneof [ int64; oneof_list [ 0L; -1L; Int64.min_int ] ] in
             Quiver_net.Message.Committed
               {
                 chunk;
                 part = { file; rows; bytes; stored_bytes; checksum = Xxh3_64 digest };
               });
            map2
              (fun chunk reason -> Quiver_net.Message.Rejected { chunk; reason })
              count text;
            map (fun messages -> Quiver_net.Message.Heard { messages }) count;
            map (fun reason -> Quiver_net.Message.Failed reason) text;
          ]
      in
      oneof
        [
          map
            (fun message ->
              Sexplib0.Sexp.List
                [ Atom "control"; Quiver_net.Message.sexp_of_control message ])
            control;
          map
            (fun message ->
              Sexplib0.Sexp.List
                [ Atom "reply"; Quiver_net.Message.sexp_of_reply message ])
            reply;
          map
            (fun attach ->
              Sexplib0.Sexp.List
                [ Atom "attach"; Quiver_net.Message.sexp_of_attach attach ])
            (oneof
               [
                 map (fun session -> Quiver_net.Message.Data { session }) text;
                 map2
                   (fun session replies ->
                     Quiver_net.Message.Control { session; replies })
                   text count;
               ]);
        ])
    (fun tagged ->
      let back sexp decode encode =
        match
          Parsexp.Single.parse_string (Sexplib0.Sexp.to_string sexp)
        with
        | Error _ -> false
        | Ok parsed -> (
            match decode parsed with
            | decoded -> Sexplib0.Sexp.equal (encode decoded) sexp
            | exception Sexplib0.Sexp_conv.Of_sexp_error _ -> false)
      in
      match tagged with
      | List [ Atom "control"; sexp ] ->
          back sexp Quiver_net.Message.control_of_sexp
            Quiver_net.Message.sexp_of_control
      | List [ Atom "reply"; sexp ] ->
          back sexp Quiver_net.Message.reply_of_sexp
            Quiver_net.Message.sexp_of_reply
      | List [ Atom "attach"; sexp ] ->
          back sexp Quiver_net.Message.attach_of_sexp
            Quiver_net.Message.sexp_of_attach
      | _ -> false)

let unchecked_sexp =
  Alcotest.test_case
    "S-expressions that no message could come from are refused" `Quick
    (fun () ->
      let decoded text =
        match Parsexp.Single.parse_string text with
        | Error _ -> false
        | Ok sexp -> (
            match Quiver_net.Message.control_of_sexp sexp with
            | (_ : Quiver_net.Message.control) -> true
            | exception Sexplib0.Sexp_conv.Of_sexp_error _ -> false)
      and replied text =
        match Parsexp.Single.parse_string text with
        | Error _ -> false
        | Ok sexp -> (
            match Quiver_net.Message.reply_of_sexp sexp with
            | (_ : Quiver_net.Message.reply) -> true
            | exception Sexplib0.Sexp_conv.Of_sexp_error _ -> false)
      in
      let table ltype nullability key =
        let field =
          Printf.sprintf "((name c)(ltype %s)(nullability %s))" ltype nullability
        in
        Printf.sprintf
          "(Table(table((name(db t))(fields(%s()))(key %s)(estimate((rows 0)(bytes 0)))))(ddl \"\")(parts 1))"
          field
          (match key with
          | `Primary -> Printf.sprintf "(Primary(%s()))" field
          | `No_key -> "No_key")
      and committed digits =
        Printf.sprintf
          "(Committed(chunk 0)(part((file p)(rows 0)(bytes 0)(stored_bytes 0)(checksum(Xxh3_64 %s)))))"
          digits
      in
      List.iter
        (fun (label, expected, accepted) ->
          Alcotest.(check bool) label expected accepted)
        [
          ("valid", true, decoded (table "(Time 6)" "Not_null" `Primary));
          ("fsp 7", false, decoded (table "(Time 7)" "Not_null" `No_key));
          ( "scale above precision",
            false,
            decoded
              (table "(Decimal((precision 2)(scale 3)))" "Not_null" `No_key) );
          ("nullable key", false, decoded (table "Int8" "Nullable" `Primary));
          ("empty name", false, decoded "(Begin(chunk 0)(table(db \"\"))(index 0))");
          ("name of one", false, decoded "(Begin(chunk 0)(table(db))(index 0))");
          ("checksum", true, replied (committed "00000000000000ff"));
          ("short checksum", false, replied (committed "0ff"));
          ("signed checksum", false, replied (committed "+000000000000ff0"));
          ("underscored checksum", false, replied (committed "0000_00000000ff0"));
        ])

let tests =
  [
    messages;
    unchecked_sexp;
    calendar;
    localized;
    table_json;
    unchecked_json;
    gtid ("MySQL 8.4", "8.4.11", mysql_gtid);
    gtid ("MariaDB", "11.8.9-MariaDB", mariadb_gtid);
    datetime;
    time;
    digits;
    file_names;
    quoting;
    key_json;
    unsigned_json;
    checksum_json;
    unchecked;
    fingerprint;
  ]
