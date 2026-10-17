open Base.Result.Let_syntax
open Quiver_core

(** How the agent reaches the collector that answers it. *)
type topology =
  | Direct
  | Relayed of int  (** Through this many relays. *)
  | Chained  (** To a collector that passes everything on to a second one. *)

(** How every connection of the session is protected. *)
type wire = Plain | Mutual_tls

(** What a collector refuses of the table with this number. *)
type refused =
  | Ddl_of of int  (** Creating it. *)
  | Parts_of of int  (** Closing each of its parts. *)

(** Which collector of a chain refuses, if any. *)
type refusal = Nobody | First of refused | Last of refused

(** What a recorder refuses. *)
type refusing = Creating of Schema.Name.t | Closing of Schema.Name.t

(** How the agent ends a part. *)
type ending =
  | Kept
  | Abandoned_after of int  (** Discarded after sending so many batches. *)

type scenario = {
  tables : (int list * ending) list list;
      (** Per table, per part: rows per batch, and how the part ends. *)
  streams : int;
  chunks : int;
  readers : int;
  topology : topology;
  refusal : refusal;
  wire : wire;
}

let ident = Support.ident

let table n =
  Support.get_ok
    (Schema.Table.make
       ~name:{ database = ident "db"; table = ident ("t" ^ string_of_int n) }
       ~fields:[ { name = ident "id"; ltype = Int64; nullability = Not_null } ]
       ~key:No_key ~estimate:{ rows = 0; bytes = 0 })

(** A sink that remembers what reached it, refusing one table or its parts if
    told to. *)
module Recorder = struct
  type error = Refused_table of Schema.Name.t | Unknown_part

  let sexp_of_error = function
    | Refused_table name ->
        Sexplib0.Sexp.Atom (Format.asprintf "refused %a" Schema.Name.pp name)
    | Unknown_part -> Atom "a part of no table"

  type log = {
    mutable tables : Schema.Name.t list;  (** Created, latest first. *)
    mutable closed : ((Schema.Name.t * int) * int64 list) list;
    mutable finished : string option;  (** The manifest, as JSON text. *)
  }

  type t = {
    refuse : refusing option;
    log : log Await.Capsule.Await.With_mutex.t;
  }

  type part = {
    owner : t;
    name : Schema.Name.t;
    index : int;
    mutable rows : int64 list;  (** Latest first. *)
  }

  type renderer = unit
  type rendered = { values : int64 list @@ aliased many } [@@unboxed]

  let create refuse =
    {
      refuse;
      log =
        Await.Capsule.Await.With_mutex.create (fun () ->
            { tables = []; closed = []; finished = None });
    }

  let logged t f =
    Guarded.hold t.log f

  let tables t = logged t (fun log -> log.tables)
  let closed t = logged t (fun log -> log.closed)

  let finished t =
    Option.map Yojson.Safe.from_string (logged t (fun log -> log.finished))

  let format _ = Output_format.Sql
  let database _ _ ~ddl:_ = Ok ()

  let table t ({ name; _ } : Schema.Table.t) ~ddl:_ ~parts:_ =
    match t.refuse with
    | Some (Creating refused) when Schema.Name.equal refused name ->
        Error (`Sink (Refused_table name))
    | Some (Creating _ | Closing _) | None ->
        logged t (fun log -> log.tables <- name :: log.tables);
        Ok ()

  let objects _ _ _ = Ok ()

  let part t ({ name; _ } : Schema.Table.t) ~index =
    match logged t (fun log -> List.mem name log.tables) with
    | true -> Ok { owner = t; name; index; rows = [] }
    | false -> Error (`Sink Unknown_part)

  let renderer _ = ()

  let render () (batch : Batch.sealed @ local) =
    match Support.int64s batch with
    | Some values -> Ok { values }
    | None -> Error (`Sink Unknown_part)

  let append part { values = rows } =
    part.rows <- List.rev_append rows part.rows;
    Ok ()

  let close part answer =
    let rows = List.rev part.rows and name = part.name and index = part.index in
    match part.owner.refuse with
    | Some (Closing refused) when Schema.Name.equal refused name ->
        answer (Error (Refused_table name))
    | Some (Creating _ | Closing _) | None ->
    logged part.owner (fun log ->
        log.closed <- ((name, index), rows) :: log.closed);
    answer
      (Ok
         {
           Manifest.file =
             File_path.Part.of_string
               (Printf.sprintf "%s.%d" (part.name.table :> string) part.index);
           rows = List.length rows;
           bytes = 0;
           stored_bytes = 0;
           checksum = Xxh3_64 (Int64.of_int part.index);
         })

  let discard _ = ()

  let finish t ~source:_ ~tables:_ ~manifest =
    let manifest = Yojson.Safe.to_string manifest in
    logged t (fun log -> log.finished <- Some manifest);
    Ok ()
end

module Recording = Quiver_net.Collector.Make (Recorder)

let scenario =
  QCheck2.Gen.(
    let* tables =
      list_size (int_range 1 4)
        (list_size (int_range 1 4)
           (pair
              (list_size (int_range 0 5) (int_range 0 40))
              (oneof_weighted
                 [
                   (6, pure Kept);
                   (1, map (fun n -> Abandoned_after n) (int_range 0 5));
                 ])))
    in
    let* streams = int_range 1 4
    and* chunks = int_range 1 6
    and* readers = int_range 1 3
    and* topology =
      oneof
        [ pure Direct; map (fun n -> Relayed n) (int_range 1 2); pure Chained ]
    in
    let* refusal =
      let refused =
        let* n = int_range 0 (List.length tables - 1) in
        oneof_list [ Ddl_of n; Parts_of n ]
      in
      oneof_weighted
        [
          (3, pure Nobody);
          (1, map (fun refused -> First refused) refused);
          ( 1,
            match topology with
            | Chained -> map (fun refused -> Last refused) refused
            | Direct | Relayed _ -> pure Nobody );
        ]
    in
    let+ wire = oneof [ pure Plain; pure Mutual_tls ] in
    { tables; streams; chunks; readers; topology; refusal; wire })

let print { tables; streams; chunks; readers; topology; refusal; wire } =
  Printf.sprintf "%s; %d streams, %d chunks, %d readers, %s, %s, %s"
    (String.concat " | "
       (List.map
          (fun parts ->
            String.concat ","
              (List.map
                 (fun (batches, ending) ->
                   String.concat "+" (List.map string_of_int batches)
                   ^
                   match ending with
                   | Kept -> ""
                   | Abandoned_after n -> "/" ^ string_of_int n)
                 parts))
          tables))
    streams chunks readers
    (match topology with
    | Direct -> "direct"
    | Relayed n -> string_of_int n ^ " relays"
    | Chained -> "chained")
    (let what = function
       | Ddl_of n -> "t" ^ string_of_int n
       | Parts_of n -> "the parts of t" ^ string_of_int n
     in
     match refusal with
     | Nobody -> "no refusal"
     | First refused -> "first refuses " ^ what refused
     | Last refused -> "last refuses " ^ what refused)
    (match wire with Plain -> "plaintext" | Mutual_tls -> "mutual TLS")

let property { tables; streams; chunks; readers; topology; refusal; wire } =
  let schema = List.mapi (fun n _ -> table n) tables in
  let rows =
    let next = ref 0L in
    List.map
      (List.map (fun (batches, ending) ->
           ( List.map
               (fun count ->
                 List.init count (fun _ ->
                     next := Int64.succ !next;
                     !next))
               batches,
             ending )))
      tables
  in
  let refusal =
    match refusal with
    | (First (Parts_of n) | Last (Parts_of n))
      when not
             (List.exists
                (fun (_, ending) ->
                  match ending with Kept -> true | Abandoned_after _ -> false)
                (List.nth tables n)) ->
        Nobody
    | Nobody | First _ | Last _ -> refusal
  in
  let refusing = function
    | Ddl_of n -> Some (Creating (List.nth schema n).Schema.Table.name)
    | Parts_of n -> Some (Closing (List.nth schema n).Schema.Table.name)
  in
  let first =
    Recorder.create
      (match refusal with First refused -> refusing refused | Nobody | Last _ -> None)
  and last =
    Recorder.create
      (match refusal with Last refused -> refusing refused | Nobody | First _ -> None)
  in
  let outcomes =
    Await.Capsule.Await.With_mutex.create (fun () -> Hashtbl.create 16)
  in
  let recorded f =
    Guarded.hold outcomes f
  in
  let protection =
    match wire with
    | Plain -> Quiver_net.Protection.plaintext
    | Mutual_tls -> Support.protection ~node:"node" ~peers:"ca"
  in
  let listeners = ref [] in
  let session_seconds = 2. in
  let outcome =
  Support.within session_seconds
  @@ fun () ->
  let listen () =
    let socket = Result.get_ok (Quiver_net.Channel.listen Support.loopback) in
    listeners := socket :: !listeners;
    ( socket,
      {
        Quiver_net.Channel.host = "127.0.0.1";
        address = Quiver_net.Channel.listening_address socket;
      } )
  in
  let collected recorder ?next socket =
    Support.start (fun () ->
          (let%bind session =
             Quiver_net.Collector.accept socket protection
           in
           let%bind forward =
             match next with
             | None -> Ok None
             | Some address ->
                 let%map downstream =
                   Quiver_net.Collector.forward address session
                 in
                 Some downstream
           in
           Result.map_error
             (fun _ -> `Failed)
             (Recording.run ?forward socket session recorder)))
  in
  let relayed next =
    let socket, address = listen () in
    Support.spawn (fun () ->
        ignore
          (let%bind session =
             Quiver_net.Collector.accept socket protection
           in
           let%bind downstream =
             Quiver_net.Collector.forward next session
           in
           Quiver_net.Collector.relay socket session downstream));
    address
  in
  let collectors, entry =
    match topology with
    | Direct ->
        let socket, address = listen () in
        ([ (first, collected first socket) ], address)
    | Relayed hops ->
        let socket, address = listen () in
        let result = collected first socket in
        ( [ (first, result) ],
          List.fold_left
            (fun next _ -> relayed next)
            address (List.init hops Fun.id) )
    | Chained ->
        let tail, tail_address = listen () in
        let tail_result = collected last tail in
        let head, head_address = listen () in
        let head_result = collected first ~next:tail_address head in
        ([ (first, head_result); (last, tail_result) ], head_address)
  in
  let agent =
    match
      Quiver_net.Remote.create protection entry ~streams ~chunks
        (Result.get_ok (Quiver_sink.Compression.Level.of_int 1))
    with
    | Ok agent -> agent
    | Error (`Sink error) ->
        Alcotest.failf "agent: %a"
          (fun ppf value ->
            Sexplib0.Sexp.pp_hum ppf (Quiver_net.Remote.sexp_of_error value))
          error
  in
  ignore (Quiver_net.Remote.database agent (ident "db") ~ddl:"");
  List.iteri
    (fun n parts ->
      ignore
        (Quiver_net.Remote.table agent (table n) ~ddl:""
           ~parts:(List.length parts)))
    tables;
  let jobs =
    Array.of_list
      (List.concat
         (List.mapi
            (fun n parts ->
              List.mapi (fun index batches -> (n, index, batches)) parts)
            rows))
  and next = Atomic.make 0
  and closing = Atomic.make 0 in
  ignore
  @@ Support.all
    (List.init readers (fun _ () ->
         (fun () ->
             let rec work () =
               match Atomic.fetch_and_add next 1 with
               | job when job >= Array.length jobs -> ()
               | job -> (
                   let n, index, (batches, ending) = jobs.(job) in
                   match Quiver_net.Remote.part agent (table n) ~index with
                   | Error _ -> ()
                   | Ok part ->
                       let sent =
                         List.for_all
                           (fun values ->
                             let batch = Batch.create [ Int64 ] in
                             let rec add (batch : Batch.t @ local) = function
                               | [] -> ()
                               | value :: rest ->
                                   Support.add_int64 (Batch.columns batch) value;
                                   Batch.commit_row batch;
                                   add batch rest
                             in
                             add (borrow_ batch) values;
                             match
                               let%bind sealed = Batch.seal batch in
                               match Quiver_net.Remote.render (Quiver_net.Remote.renderer part) sealed with
                               | Ok rendered ->
                                   Quiver_net.Remote.append part rendered
                               | Error error -> Error error
                             with
                             | Ok () -> true
                             | Error _ -> false)
                           (match ending with
                           | Kept -> batches
                           | Abandoned_after count ->
                               List.filteri (fun at _ -> at < count) batches)
                       in
                       (match (sent, ending) with
                       | true, Kept ->
                           Atomic.incr closing;
                           Quiver_net.Remote.close part (fun outcome ->
                               recorded (fun outcomes ->
                                   Hashtbl.replace outcomes (n, index) outcome))
                       | false, _ | true, Abandoned_after _ ->
                           Quiver_net.Remote.discard part);
                       work ())
             in
             work ()) ()));
  let rec settled () =
    match
      recorded (fun outcomes -> Hashtbl.length outcomes)
      >= Atomic.get closing
    with
    | true -> ()
    | false ->
        Thread.yield ();
        settled ()
  in
  settled ();
  let manifest =
    Manifest.yojson_of_t
      (Manifest.make ~level:(Any Best_effort) ~format:Sql ~position:`Null
         ~schema:(Support.get_ok (Schema.make schema))
         (List.concat
            (List.mapi
               (fun n parts ->
                 List.concat
                   (List.mapi
                      (fun index _ ->
                        match
                          recorded (fun outcomes ->
                              Hashtbl.find_opt outcomes (n, index))
                        with
                        | Some (Ok part) ->
                            [
                              ( Chunk.whole (table n),
                                Support.finished part );
                            ]
                        | Some (Error _) | None -> [])
                      parts))
               tables)))
  in
  let whole =
    match
      List.exists
        (List.exists (fun (_, ending) ->
             match ending with Kept -> false | Abandoned_after _ -> true))
        tables
    with
    | true -> `Cut_short
    | false -> `Whole
  in
  let rejected =
    recorded (fun outcomes ->
        Hashtbl.fold (fun _ outcome seen -> seen || Result.is_error outcome)
          outcomes false)
  in
  let finished =
    match (whole, refusal, rejected) with
    | _, _, true -> Error ()
    | `Cut_short, Nobody, false -> Ok ()
    | `Whole, _, false | `Cut_short, (First _ | Last _), false ->
        Result.map_error ignore
          (Quiver_net.Remote.finish agent ~source:"" ~tables:[] ~manifest)
  in
  Quiver_net.Remote.shutdown agent;
  let results =
    List.map
      (fun (recorder, result) -> (recorder, Support.join result))
      collectors
  in
  let expected =
    List.concat
      (List.mapi
         (fun n parts ->
           List.concat
             (List.mapi
                (fun index (rows, ending) ->
                  match ending with
                  | Kept ->
                      [
                        ( ((List.nth schema n).Schema.Table.name, index),
                          List.concat rows );
                      ]
                  | Abandoned_after _ -> [])
                parts))
         rows)
  in
  match (refusal, whole) with
  | Nobody, `Cut_short ->
      List.for_all
        (fun ((recorder : Recorder.t), _) ->
          List.sort compare (Recorder.closed recorder) = List.sort compare expected
          && Option.is_none (Recorder.finished recorder))
        results
  | Nobody, `Whole ->
      Result.is_ok finished
      && List.for_all
           (fun ((recorder : Recorder.t), result) ->
             Result.is_ok result
             && List.sort compare (Recorder.closed recorder)
                = List.sort compare expected
             &&
             match Option.map Manifest.read (Recorder.finished recorder) with
             | Some (Ok { parts; _ }) ->
                 List.map
                   (fun ((name : Schema.Name.t), (part : Manifest.part)) ->
                     (name, part.file))
                   parts
                 = List.map
                     (fun ((name, index), _) ->
                       ( name,
                         File_path.Part.of_string
                           (Printf.sprintf "%s.%d"
                              (name.Schema.Name.table :> string)
                              index) ))
                     expected
             | Some (Error _) | None -> false)
           results
  | (First (Parts_of n) | Last (Parts_of n)), _ ->
      Result.is_error finished
      && List.for_all
           (fun ((recorder : Recorder.t), _) ->
             Option.is_none (Recorder.finished recorder))
           results
      && List.for_all Fun.id
           (List.mapi
              (fun index (_, ending) ->
                match ending with
                | Abandoned_after _ -> true
                | Kept -> (
                    match
                      recorded (fun outcomes ->
                          Hashtbl.find_opt outcomes (n, index))
                    with
                    | Some (Error _) | None -> true
                    | Some (Ok _) -> false))
              (List.nth tables n))
  | (First (Ddl_of refused_at) | Last (Ddl_of refused_at)), _ ->
      let refuser =
        match refusal with Last _ -> last | Nobody | First _ -> first
      in
      Result.is_error finished
      && List.for_all
           (fun ((recorder : Recorder.t), _) ->
             Option.is_none (Recorder.finished recorder))
           results
      && List.rev (Recorder.tables refuser)
         = List.filteri
             (fun n _ -> n < refused_at)
             (List.map (fun ({ name; _ } : Schema.Table.t) -> name) schema)
  in
  List.iter Quiver_net.Channel.stop !listeners;
  match outcome with Ok verdict -> verdict | Error `Timeout -> false

let session =
  QCheck2.Test.make ~count:60
    ~name:
      "a session reaches every collector with every part's rows in order, or \
       fails everywhere when one refuses" ~print scenario property

let refused_downstream =
  QCheck2.Test.make ~count:50
    ~name:
      "no node of a chain finishes when the last one refused a table, even if \
       its refusal arrives after Finish"
    ~print
    (QCheck2.Gen.pure
       {
         tables = [ [ ([], Abandoned_after 0) ] ];
         streams = 3;
         chunks = 4;
         readers = 2;
         topology = Chained;
         refusal = Last (Ddl_of 0);
         wire = Plain;
       })
    property

let tests =
  List.map QCheck_alcotest.to_alcotest [ session; refused_downstream ]
