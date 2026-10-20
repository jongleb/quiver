open Base.Result.Let_syntax
open Quiver_core
open Sexplib0.Sexp_conv

(** How the agent reaches the collector that answers it. *)
type topology =
  | Direct
  | Relayed of int  (** Through this many relays. *)
  | Chained  (** To a collector that passes everything on to a second one. *)
[@@deriving sexp_of]

(** How every connection of the session is protected. *)
type wire = Plain | Mutual_tls [@@deriving sexp_of]

(** What a collector refuses of the table with this number. *)
type refused =
  | Ddl_of of int  (** Creating it. *)
  | Parts_of of int  (** Closing each of its parts. *)
[@@deriving sexp_of]

(** Which collector of a chain refuses, if any. *)
type refusal = Nobody | First of refused | Last of refused [@@deriving sexp_of]

(** What a recorder refuses. *)
type refusing = Creating of Schema.Name.t | Closing of Schema.Name.t

(** How the agent ends a part. *)
type ending =
  | Kept
  | Abandoned_after of int  (** Discarded after sending so many batches. *)
[@@deriving sexp_of]

(** Which hop the cut connections lead to. *)
type place = Into_first | Into_last [@@deriving sexp_of]

(** How the frame a connection breaks at arrives. *)
type breaking = Truncated | Damaged | Reset [@@deriving sexp_of]

type cut = {
  connection : int;  (** A data connection, in the order they are accepted. *)
  after : int;  (** Batch frames it carries before it breaks at the next. *)
  breaking : breaking;
}
[@@deriving sexp_of]
(** One of the first data connections, whose acks never return and that
    breaks. *)

type scenario = {
  tables : (int list * ending) list list;
      (** Per table, per part: rows per batch, and how the part ends. *)
  streams : int;
  chunks : int;
  readers : int;
  topology : topology;
  refusal : refusal;
  wire : wire;
  cuts : cut list;  (** Plaintext only; connections that replace them do not break. *)
  control : (int * breaking) option;
      (** Plaintext into the first node only: the control connection passes so
          many frames, then no more replies, and breaks at its next frame. *)
  adjustments : Quiver_core.Width.decision list;
      (** Taken by readers, one before each part. *)
  compression : Quiver_net.Remote.compression;
  place : place;  (** [Into_last] in a chain only. *)
  linger : float;  (** Seconds readers wait before their first part. *)
}
[@@deriving sexp_of]

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

  let add_part t ({ name; _ } : Schema.Table.t) =
    match logged t (fun log -> List.mem name log.tables) with
    | true -> Ok ()
    | false -> Error (`Sink Unknown_part)

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
    let* wire = oneof [ pure Plain; pure Mutual_tls ] in
    let* cuts =
      match wire with
      | Mutual_tls -> pure []
      | Plain ->
          let+ broken =
            list_size (int_range 0 streams)
              (pair (int_range 0 6) (oneof_list [ Truncated; Damaged; Reset ]))
          in
          List.mapi
            (fun connection (after, breaking) -> { connection; after; breaking })
            broken
    and* control =
      match wire with
      | Mutual_tls -> pure None
      | Plain ->
          option
            (pair (int_range 1 12) (oneof_list [ Truncated; Damaged; Reset ]))
    and* place =
      match topology with
      | Chained -> oneof_list [ Into_first; Into_last ]
      | Direct | Relayed _ -> pure Into_first
    in
    let* adjustments =
      list_size (int_range 0 6)
        (oneof
           [
             map (fun n -> Quiver_core.Width.Open n) (int_range 1 3);
             map (fun n -> Quiver_core.Width.Close n) (int_range 1 3);
           ])
    in
    let* compression =
      oneof_list
        [
          Quiver_net.Remote.Fixed Plain;
          Fixed (Zstd (Result.get_ok (Quiver_sink.Compression.Level.of_int 1)));
          Adaptive;
        ]
    in
    pure
      {
        compression;
        tables;
        streams;
        chunks;
        readers;
        topology;
        refusal;
        wire;
        cuts;
        control = (match place with Into_first -> control | Into_last -> None);
        place;
        adjustments;
        linger = 0.;
      })

let print scenario = Sexplib0.Sexp.to_string_hum (sexp_of_scenario scenario)

(** Passes connections on to [target] byte for byte, reading frames from the
    connecting side, and breaks data connections as [cuts] say. Its socket is
    never closed: a thread may still accept on it. *)
let breaking ~target ~control cuts =
  let listening = Unix.socket PF_INET SOCK_STREAM 0 in
  Unix.setsockopt listening SO_REUSEADDR true;
  Unix.bind listening Support.loopback;
  Unix.listen listening 16;
  let rec exactly fd bytes off len =
    match len with
    | 0 -> `Read
    | _ -> (
        match Unix.read fd bytes off len with
        | 0 -> `Ended
        | count -> exactly fd bytes (off + count) (len - count)
        | exception Unix.Unix_error _ -> `Ended)
  in
  let pass ~from ~into ~reset ~deaf cut =
    let header = Bytes.create Quiver_net.Frame.header_bytes in
    let rec go frames =
      (match cut with
      | Some { after; _ } when frames = after + 1 -> Atomic.set deaf true
      | Some _ | None -> ());
      match exactly from header 0 (Bytes.length header) with
      | `Ended -> ignore (Unix.shutdown into SHUTDOWN_SEND)
      | `Read -> (
          let length =
            Int32.to_int (Bytes.get_int32_le header 16) land 0xFFFF_FFFF
          in
          let payload = Bytes.create length in
          match (exactly from payload 0 length, cut) with
          | `Ended, _ -> ignore (Unix.shutdown into SHUTDOWN_SEND)
          | `Read, Some { after; breaking; _ } when frames > after ->
              let shutdown fd how =
                try Unix.shutdown fd how with Unix.Unix_error _ -> ()
              in
              (match breaking with
              | Truncated ->
                  ignore (Unix.write into header 0 (Bytes.length header / 2));
                  shutdown from SHUTDOWN_ALL;
                  shutdown into SHUTDOWN_ALL
              | Damaged ->
                  let at = Bytes.length header - 1 in
                  Bytes.set header at
                    (Char.chr (Char.code (Bytes.get header at) lxor 1));
                  ignore (Unix.write into header 0 (Bytes.length header));
                  ignore (Unix.write into payload 0 length);
                  shutdown from SHUTDOWN_ALL;
                  shutdown into SHUTDOWN_ALL
              | Reset ->
                  ignore (Unix.write into header 0 (Bytes.length header / 2));
                  Unix.setsockopt_optint into SO_LINGER (Some 0);
                  Atomic.set reset true;
                  shutdown from SHUTDOWN_ALL;
                  shutdown into SHUTDOWN_RECEIVE)
          | `Read, (Some _ | None) ->
              ignore (Unix.write into header 0 (Bytes.length header));
              ignore (Unix.write into payload 0 length);
              go (frames + 1))
    in
    (try go 0 with Unix.Unix_error _ -> ())
  and copy ~from ~into ~reset ~deaf cut =
    let buffer = Bytes.create 65536 in
    let rec go () =
      match (Unix.read from buffer 0 (Bytes.length buffer), cut) with
      | 0, _ -> (
          (try Unix.shutdown into SHUTDOWN_SEND with Unix.Unix_error _ -> ());
          match Atomic.get reset with true -> Unix.close from | false -> ())
      | _, Some (_ : cut) -> go ()
      | _, None when Atomic.get deaf -> go ()
      | count, None ->
          ignore (Unix.write into buffer 0 count);
          go ()
    in
    try go () with Unix.Unix_error _ -> ()
  in
  Support.spawn (fun () ->
      let rec accept accepted =
        match Unix.accept listening with
        | exception Unix.Unix_error _ -> ()
        | upstream, _ ->
            let downstream = Unix.socket PF_INET SOCK_STREAM 0 in
            Unix.connect downstream target.Quiver_net.Channel.address;
            let cut =
              List.find_opt
                (fun { connection; _ } -> connection = accepted - 1)
                cuts
            and broken =
              match accepted with
              | 0 ->
                  Option.map
                    (fun (after, breaking) ->
                      { connection = -1; after; breaking })
                    control
              | _ -> None
            in
            let reset = Atomic.make false and deaf = Atomic.make false in
            Support.spawn (fun () ->
                pass ~from:upstream ~into:downstream ~reset ~deaf
                  (Option.fold ~none:cut ~some:Option.some broken));
            Support.spawn (fun () ->
                copy ~from:downstream ~into:upstream ~reset ~deaf cut);
            accept (accepted + 1)
      in
      accept 0);
  {
    Quiver_net.Channel.host = target.Quiver_net.Channel.host;
    address = Unix.getsockname listening;
  }

let property
    {
      tables;
      streams;
      chunks;
      readers;
      topology;
      refusal;
      wire;
      cuts;
      control;
      place;
      adjustments;
      compression;
      linger;
    } =
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
  let session_seconds =
    linger +. match control with None -> 2. | Some (_ : int * breaking) -> 4.
  in
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
               ~reconnect:Support.reconnect
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
               ~reconnect:Support.reconnect
           in
           let%bind downstream =
             Quiver_net.Collector.forward next session
           in
           Quiver_net.Collector.relay socket session downstream));
    address
  in
  let breaking_at here target =
    match (cuts, control, here = place) with
    | [], None, _ | _, _, false -> target
    | _ :: _, _, true | _, Some _, true -> breaking ~target ~control cuts
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
        let head_result =
          collected first ~next:(breaking_at Into_last tail_address) head
        in
        ([ (first, head_result); (last, tail_result) ], head_address)
  in
  let entry = breaking_at Into_first entry in
  let agent =
    match
      Quiver_net.Remote.create protection entry (Support.settings ~compression ~streams ~chunks ())
    with
    | Ok agent -> agent
    | Error (`Sink error) ->
        Alcotest.failf "agent: %a"
          Sexplib0.Sexp.pp_hum
          (Quiver_net.Remote.sexp_of_error error)
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
      (Base.List.concat_mapi rows ~f:(fun n parts ->
           List.mapi (fun index batches -> (n, index, batches)) parts))
  and next = Atomic.make 0
  and closing = Atomic.make 0
  and adjustments = Array.of_list adjustments in
  ignore
  @@ Support.all
    (List.init readers (fun _ () ->
         (fun () ->
             Unix.sleepf linger;
             let rec work () =
               match Atomic.fetch_and_add next 1 with
               | job when job >= Array.length jobs -> ()
               | job -> (
                   if job < Array.length adjustments then
                     Quiver_net.Remote.adjust agent adjustments.(job);
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
         ~threads:{ sessions = 1; readers = 1; peak = 1; renderers = 1 }
         ~schema:(Support.get_ok (Schema.make schema))
         (Base.List.concat_mapi tables ~f:(fun n parts ->
              Base.List.filter_mapi parts ~f:(fun index _ ->
                  match
                    recorded (fun outcomes -> Hashtbl.find_opt outcomes (n, index))
                  with
                  | Some (Ok part) ->
                      Some (Chunk.whole (table n), Support.finished part)
                  | Some (Error _) | None -> None))))
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
  let sent_frames =
    List.concat_map
      (List.filter_map (fun (batches, ending) ->
           match ending with
           | Kept -> Some (List.length batches)
           | Abandoned_after _ -> None))
      rows
  and carried json =
    let open Yojson.Safe.Util in
    List.map
      (fun chunk ->
        match member "state" chunk with
        | `List [ `String "Done"; finished ] -> (
            match member "carrying" finished with
            | `Null -> None
            | carrying ->
                let frames = to_int (member "frames" carrying)
                and encoded =
                  List.fold_left ( + ) 0
                    (List.map
                       (fun encoding -> to_int (member "frames" encoding))
                       (to_list (member "encodings" carrying)))
                in
                Base.Option.some_if
                  (encoded = frames
                  && (to_int (member "raw_bytes" carrying) > 0) = (frames > 0)
                  && to_int (member "wire_bytes" carrying)
                     >= frames * Quiver_net.Frame.header_bytes
                  && to_int (member "streams" carrying) >= 1)
                  frames)
        | _ -> None)
      (to_list (member "chunks" json))
  in
  let expected =
    Base.List.concat_mapi rows ~f:(fun n parts ->
        Base.List.filter_mapi parts ~f:(fun index (rows, ending) ->
            match ending with
            | Kept ->
                Some (((List.nth schema n).Schema.Table.name, index), List.concat rows)
            | Abandoned_after _ -> None))
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
                 && Option.map carried (Recorder.finished recorder)
                    = Some (List.map (fun count -> Some count) sent_frames)
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
         cuts = [];
         control = None;
         place = Into_first;
         adjustments = [];
         compression = Adaptive;
         linger = 0.;
       })
    property

let unreachable_next =
  Alcotest.test_case
    "an agent is refused at once when the first hop cannot reach the next"
    `Quick (fun () ->
      let plaintext = Quiver_net.Protection.plaintext in
      let peer listener =
        {
          Quiver_net.Channel.host = "127.0.0.1";
          address = Quiver_net.Channel.listening_address listener;
        }
      in
      let gone = Result.get_ok (Quiver_net.Channel.listen Support.loopback) in
      let nowhere = peer gone in
      Quiver_net.Channel.stop gone;
      let head = Result.get_ok (Quiver_net.Channel.listen Support.loopback) in
      Support.spawn (fun () ->
          ignore
            (Result.bind (Quiver_net.Collector.accept head plaintext ~reconnect:Support.reconnect)
               (Quiver_net.Collector.forward nowhere)));
      let refusal_seconds = 2. in
      let created =
        Support.within refusal_seconds (fun () ->
            Quiver_net.Remote.create plaintext (peer head) (Support.settings ~streams:2 ~chunks:2 ()))
      in
      Quiver_net.Channel.stop head;
      Alcotest.(check bool)
        "refused before the deadline" true
        (match created with
        | Ok (Error (`Sink (Quiver_net.Remote.Refused { request = "hello"; _ }))) ->
            true
        | Ok (Ok agent) ->
            Quiver_net.Remote.shutdown agent;
            false
        | Ok (Error _) | Error `Timeout -> false))

let resumed_through_relays =
  QCheck2.Test.make ~count:20
    ~name:
      "a control connection that breaks in front of two relays is replaced \
       through them, and the session goes on"
    ~print
    (QCheck2.Gen.pure
       {
         tables =
           [ [ ([ 3; 4 ], Kept); ([ 5 ], Kept) ]; [ ([ 2; 2; 2 ], Kept) ] ];
         streams = 1;
         chunks = 2;
         readers = 2;
         topology = Relayed 2;
         refusal = Nobody;
         wire = Plain;
         cuts = [];
         control = Some (3, Truncated);
         place = Into_first;
         adjustments = [];
         compression = Fixed Plain;
         linger = 0.;
       })
    property

let resumed_after_heartbeats =
  QCheck2.Test.make ~count:5
    ~name:
      "heartbeats that arrived before the control connection broke are not \
       counted as replies"
    ~print
    (QCheck2.Gen.pure
       {
         tables = [ [ ([ 3 ], Kept); ([ 5; 1 ], Kept); ([ 2 ], Kept) ] ];
         streams = 2;
         chunks = 1;
         readers = 1;
         topology = Direct;
         refusal = Nobody;
         wire = Plain;
         cuts = [];
         control = Some (5, Reset);
         place = Into_first;
         adjustments = [];
         compression = Fixed Plain;
         linger = 1.3;
       })
    property

let refused_while_cut =
  QCheck2.Test.make ~count:10
    ~name:
      "an agent whose control connection broke learns of a failed session \
       when it reconnects, instead of waiting"
    ~print
    (QCheck2.Gen.pure
       {
         tables = [ [ ([], Kept) ]; [ ([], Kept); ([], Kept); ([ 0; 0 ], Kept); ([], Kept) ] ];
         streams = 1;
         chunks = 3;
         readers = 3;
         topology = Chained;
         refusal = First (Ddl_of 0);
         wire = Plain;
         cuts = [ { connection = 0; after = 0; breaking = Truncated } ];
         control = Some (4, Truncated);
         place = Into_first;
         adjustments = [ Open 1 ];
         compression = Fixed Plain;
         linger = 0.;
       })
    property

let tests =
  unreachable_next
  :: List.map QCheck_alcotest.to_alcotest
       [
         session;
         refused_downstream;
         resumed_through_relays;
         resumed_after_heartbeats;
         refused_while_cut;
       ]
