let micros_per_day = 86_400_000_000

let micros_of_ptime instant =
  let days, picos = Ptime.Span.to_d_ps (Ptime.to_span instant) in
  (days * micros_per_day) + Int64.to_int (Int64.div picos 1_000_000L)

let ptime_of_micros micros =
  let days = Base.Int.( /% ) micros micros_per_day in
  Ptime.of_span
    (Ptime.Span.v
       (days, Int64.mul (Int64.of_int (micros - (days * micros_per_day))) 1_000_000L))

let first = micros_of_ptime Ptime.min

let last = micros_of_ptime Ptime.max

let micros =
  QCheck2.Gen.(
    oneof_weighted
      [
        (3, int_range (first - (3 * micros_per_day)) (last + (3 * micros_per_day)));
        (1, map (( + ) first) (int_range (-2) 2));
        (1, map (( + ) last) (int_range (-2) 2));
        (1, map (( + ) 0) (int_range (-micros_per_day) micros_per_day));
      ])

let calendar =
  QCheck2.Test.make ~count:20_000
    ~name:"a moment exists exactly in Ptime's range, with Ptime's date and clock"
    ~print:string_of_int micros
    (fun micros ->
      match (Quiver_core.Moment.of_micros micros, ptime_of_micros micros) with
      | None, None -> true
      | Some moment, Some instant ->
          let (year, month, day), ((hours, minutes, seconds), _) =
            Ptime.to_date_time instant
          and fraction =
            Int64.to_int
              (Int64.div (snd (Ptime.Span.to_d_ps (Ptime.to_span instant))) 1_000_000L)
            mod 1_000_000
          in
          let date = Quiver_core.Moment.date moment in
          Core.Date.year date = year
          && Core.Month.to_int (Core.Date.month date) = month
          && Core.Date.day date = day
          && Quiver_core.Moment.micros_of_day moment
             = (((((hours * 60) + minutes) * 60) + seconds) * 1_000_000) + fraction
          && (Quiver_core.Moment.of_date date :> int)
             + Quiver_core.Moment.micros_of_day moment
             = micros
      | Some _, None | None, Some _ -> false)

let text =
  QCheck2.Gen.(
    oneof
      [
        string_size ~gen:printable (int_range 0 30);
        (let* year = int_range (-5) 10_050
         and* month = int_range 0 13
         and* day = int_range 0 32
         and* hours = int_range 0 25
         and* minutes = int_range 0 61
         and* seconds = int_range 0 61
         and* fraction = string_size ~gen:numeral (int_range 0 6) in
         let+ separator = oneof_list [ " "; "T"; "  "; "" ] in
         Printf.sprintf "%04d-%02d-%02d%s%02d:%02d:%02d%s" year month day
           separator hours minutes seconds
           (if String.length fraction = 0 then "" else "." ^ fraction));
      ])

let parse =
  QCheck2.Test.make ~count:20_000
    ~name:"parse never raises and agrees with Ptime's RFC 3339 reading"
    ~print:Fun.id text
    (fun text ->
      let reference =
        match String.index_opt text ' ' with
        | None -> None
        | Some space -> (
            let clock =
              String.sub text (space + 1) (String.length text - space - 1)
            in
            match
              Ptime.of_rfc3339 (String.sub text 0 space ^ "T" ^ clock ^ "Z")
            with
            | Ok (instant, _, _)
              when not (Base.String.is_substring clock ~substring:":60") ->
                Some (micros_of_ptime instant)
            | Ok _ | Error _ -> None)
      in
      match (Quiver_core.Temporal.parse_datetime text, reference) with
      | Some moment, Some micros -> (moment :> int) = micros
      | None, None -> true
      | Some moment, None ->
          QCheck2.Test.fail_reportf "parsed to %d, Ptime refuses" (moment :> int)
      | None, Some micros ->
          QCheck2.Test.fail_reportf "refused, Ptime reads %d" micros)

let padded width value = Printf.sprintf "%0*d" width value

let fraction =
  QCheck2.Gen.(
    let* digits =
      oneof
        [
          string_size ~gen:numeral (int_range 0 7);
          map (fun length -> String.make length '0') (int_range 0 7);
        ]
    in
    let+ dot = oneof_list [ "."; "."; "."; ","; "" ] in
    match digits with "" -> dot | _ -> dot ^ digits)

let sixty =
  QCheck2.Gen.(oneof_weighted [ (1, pure 0); (1, pure 59); (4, int_range 0 61) ])

let clock ~hours =
  QCheck2.Gen.(
    let* width = int_range 1 4
    and* minutes = sixty
    and* seconds = sixty
    and* fraction = fraction in
    let+ hours = hours in
    Printf.sprintf "%s:%s:%s%s" (padded width hours) (padded 2 minutes)
      (padded 2 seconds) fraction)

let fsp_of text =
  Result.get_ok
    (Quiver_core.Ltype.Fsp.of_int
       (Option.fold (String.index_opt text '.') ~none:0 ~some:(fun dot ->
            String.length text - dot - 1)))

let canonical name ~print ~within gen parse render =
  QCheck2.Test.make ~count:20_000
    ~name:(name ^ " accepts only the text it prints")
    ~print gen
    (fun text ->
      Option.fold (parse text) ~none:true ~some:(fun value ->
          let printed =
            let out = Quiver_core.Byte_buffer.create () in
            render out (fsp_of text) value;
            Quiver_core.Byte_buffer.contents out
          in
          (String.equal printed text
          || QCheck2.Test.fail_reportf "printed back as %s" printed)
          && within value))

let separator =
  QCheck2.Gen.oneof_weighted [ (6, QCheck2.Gen.pure "-"); (1, QCheck2.Gen.pure "/") ]

let canonical_date =
  canonical "a date" ~print:Fun.id
    ~within:(fun _ -> true)
    QCheck2.Gen.(
      let* year = int_range (-5) 10_050
      and* width = int_range 3 5
      and* month = int_range 0 13
      and* first = separator
      and* second = separator in
      let+ day = int_range 0 32 in
      Printf.sprintf "%s%s%s%s%s" (padded width year) first (padded 2 month)
        second (padded 2 day))
    Quiver_core.Temporal.parse_date
    (fun out _ date -> Quiver_core.Temporal.day out date)

let canonical_datetime =
  canonical "a datetime" ~print:Fun.id
    ~within:(fun _ -> true)
    QCheck2.Gen.(
      let* year = int_range (-5) 10_050
      and* month = int_range 0 13
      and* day = int_range 0 32
      and* separator = oneof_list [ " "; " "; "T"; "" ] in
      let+ clock = clock ~hours:(int_range 0 25) in
      Printf.sprintf "%s-%s-%s%s%s" (padded 4 year) (padded 2 month)
        (padded 2 day) separator clock)
    Quiver_core.Temporal.parse_datetime
    (fun out fsp moment ->
      ignore
        (Quiver_core.Temporal.datetime out fsp
           ~micros:(Int64_u.of_int (moment : Quiver_core.Moment.t :> int))))

let canonical_time =
  canonical "a time" ~print:Fun.id
    ~within:(fun micros -> Int64.to_int (Int64.abs micros) < 839 * 3600 * 1_000_000)
    QCheck2.Gen.(
      let* sign = oneof_list [ ""; ""; "-"; "+" ] in
      let+ clock =
        clock
          ~hours:
            (oneof_weighted
               [ (1, pure 0); (3, int_range 0 120); (1, int_range 836 841) ])
      in
      sign ^ clock)
    Quiver_core.Temporal.parse_time
    (fun out fsp micros ->
      Quiver_core.Temporal.time out fsp ~micros:(Int64_u.of_int64 micros))

let total =
  QCheck2.Test.make ~count:50_000 ~name:"no parser raises on clock-like text"
    ~print:Fun.id
    QCheck2.Gen.(
      string_size
        ~gen:(oneof_list (Base.String.to_list "0123456789-:. "))
        (int_range 0 40))
    (fun text ->
      ignore (Quiver_core.Temporal.parse_date text);
      ignore (Quiver_core.Temporal.parse_datetime text);
      ignore (Quiver_core.Temporal.parse_time text);
      true)

let tests =
  List.map QCheck_alcotest.to_alcotest
    [ calendar; parse; total; canonical_date; canonical_datetime; canonical_time ]
