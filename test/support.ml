let testable sexp_of equal =
  Alcotest.testable (fun ppf value -> Sexplib0.Sexp.pp_hum ppf (sexp_of value)) equal

let get_ok = function
  | Ok value -> value
  | Error (error : Quiver_core.Error.t) ->
      Alcotest.failf "%a"
        Sexplib0.Sexp.pp_hum
        (Quiver_core.Error.sexp_of_t error)

let ident name = get_ok (Quiver_core.Ident.of_string name)
let name = { Quiver_core.Schema.Name.database = ident "db"; table = ident "t" }

let table ?(key : string Quiver_core.Schema.Table.key = Primary ("id", []))
    ?(generated = []) fields =
  let pair (first, rest) = (ident first, List.map ident rest) in
  let field (field, ltype, nullability) =
    { Quiver_core.Schema.Field.name = ident field; ltype; nullability }
  in
  get_ok
    (Quiver_core.Schema.Table.make ~name
       ~fields:(List.map field fields)
       ~generated:(List.map field generated)
       ~key:
         (match key with
         | Primary fields -> Primary (pair fields)
         | Unique { index; fields } -> Unique { index; fields = pair fields }
         | Indexed { index; field } -> Indexed { index; field = ident field }
         | No_key -> No_key)
       ~estimate:{ rows = 0; bytes = 0 })

let protection ~node ~peers =
  match
    Quiver_net.Protection.mutual
      ~certificate:(File_path.of_string ("tls/" ^ node ^ ".crt"))
      ~key:(File_path.of_string ("tls/" ^ node ^ ".key"))
      ~peers:(File_path.of_string ("tls/" ^ peers ^ ".crt"))
  with
  | Ok protection -> protection
  | Error (`Protection error) ->
      Alcotest.failf "%a"
        Sexplib0.Sexp.pp_hum
        (Quiver_net.Protection.sexp_of_error error)

let int64s (batch : Quiver_core.Batch.sealed @ local) =
  let (Quiver_core.Column.Any column) =
    Iarray.get (Quiver_core.Batch.sealed_columns batch) 0
  in
  match Quiver_core.Column.repr column with
  | Int64 ->
      let values = ref [] in
      for row = Quiver_core.Batch.sealed_rows batch - 1 downto 0 do
        values :=
          Int64_u.to_int64 (Quiver_core.Column.get_int64 column row) :: !values
      done;
      Some !values
  | Bool | Int | Float | Text -> None

let add_int64 (columns : Quiver_core.Column.any Iarray.t @ local) value =
  let (Quiver_core.Column.Any column) = Iarray.get columns 0 in
  match Quiver_core.Column.repr column with
  | Int64 ->
      ignore (Quiver_core.Column.add_int64 column (Int64_u.of_int64 value))
  | Bool | Int | Float | Text -> ()

(** How a test's thread ended. *)
type 'a outcome = Returned of 'a | Raised of exn * Printexc.raw_backtrace

let returned = function
  | Returned value -> value
  | Raised (failure, backtrace) -> Printexc.raise_with_backtrace failure backtrace

let start work =
  let finished = Event.new_channel () in
  ignore
    (Thread.create
       (fun () ->
         Event.sync
           (Event.send finished
              (match work () with
              | value -> Returned value
              | exception failure -> Raised (failure, Printexc.get_raw_backtrace ()))))
       ());
  finished

let join finished = returned (Event.sync (Event.receive finished))
let all works = List.map join (List.map start works)

let both first second =
  let second = start second in
  let first = first () in
  (first, join second)

let spawn work = ignore (Thread.create work ())

let within seconds work =
  let finished = start work in
  let deadline = Unix.gettimeofday () +. seconds in
  let rec wait () =
    match Event.poll (Event.receive finished) with
    | Some outcome -> Ok (returned outcome)
    | None when Unix.gettimeofday () > deadline -> Error `Timeout
    | None ->
        Thread.delay 0.002;
        wait ()
  in
  wait ()

let loopback = Unix.ADDR_INET (Unix.inet_addr_loopback, 0)

let finished part =
  Quiver_core.Manifest.Done
    {
      part;
      reading =
        {
          reader = 0;
          started = Core.Time_ns.Span.zero;
          duration = Core.Time_ns.Span.zero;
          pool_wait = Core.Time_ns.Span.zero;
          batches = 0;
          allocated_words = 0;
        };
      writing =
        {
          rendering = Core.Time_ns.Span.zero;
          appending = Core.Time_ns.Span.zero;
          reordering = Core.Time_ns.Span.zero;
          committing = Core.Time_ns.Span.zero;
        };
    }

let reconnect = Core.Time_ns.Span.of_sec 0.5

let settings ?(compression =
    Quiver_net.Remote.Fixed
      (Quiver_sink.Compression.Zstd
         (Result.get_ok (Quiver_sink.Compression.Level.of_int 1)))) ~streams
    ~chunks () =
  {
    Quiver_net.Remote.streams;
    most_streams = streams;
    chunks;
    compression;
    pace = None;
    traffic = None;
    reconnect;
    stall = Core.Time_ns.Span.of_sec 0.3;
  }
