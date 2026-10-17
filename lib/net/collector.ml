open Base.Result.Let_syntax

type error : value mod contended portable =
  | Lost of Channel.error
  | Damaged of Frame.error
  | Foreign_byte_order of Message.byte_order
  | Out_of_place of string
  | Downstream of string
  | Manifest of Quiver_core.Manifest.error
[@@deriving sexp_of ~portable]

type session = {
  control : Channel.t;
  id : string;
  streams : int;
  chunks : int;
  protection : Protection.t;
}

type downstream = {
  control : Channel.t;
  data : Channel.t list;
  format : Quiver_core.Output_format.t;
}

let failed result =
  Result.map_error
    (function
      | `Channel error -> `Collector (Lost error)
      | `Frame error -> `Collector (Damaged error))
    result

let out_of_place what = Error (`Collector (Out_of_place what))

let unexpected reply =
  out_of_place
    ("a reply " ^ Sexplib0.Sexp.to_string (Message.sexp_of_reply reply))

let accept listening protection =
  let%bind control = failed (Channel.accept listening protection) in
  let greeted =
    let%bind hello =
      failed (Channel.receive control Message.control_of_sexp)
    in
    match hello with
    | Hello { byte_order; _ } when byte_order <> Message.byte_order ->
        Error (`Collector (Foreign_byte_order byte_order))
    | Hello { session; streams; chunks; _ } ->
        Ok { control; id = session; streams; chunks; protection }
    | Database _ | Table _ | Objects _ | Begin _ | End _ | Abort _ | Finish _
      ->
        out_of_place "before hello"
  in
  Result.iter_error (fun _ -> Channel.close control) greeted;
  greeted

let chunks (session : session) = session.chunks

let attached listening (session : session) =
  let protection = session.protection and id = session.id in
  Result.map_error
    (fun error -> `Collector error)
    (Channel.all session.streams ~close:Channel.close (fun () ->
             match Channel.accept listening protection with
             | Error (`Channel error) -> Error (Lost error)
             | Ok channel -> (
                 let refused error =
                   Channel.close channel;
                   Error error
                 in
                 match Channel.receive channel Message.attach_of_sexp with
                 | Ok { session } when String.equal session id -> Ok channel
                 | Ok _ ->
                     refused (Out_of_place "a data connection of another session")
                 | Error (`Channel error) -> refused (Lost error)
                 | Error (`Frame error) -> refused (Damaged error))))

let forward peer (session : session) =
  let connect () = failed (Channel.connect session.protection peer) in
  let%bind control = connect () in
  let greeted =
  let%bind () =
    failed
      (Channel.send control
         (Message.sexp_of_control
            (Hello
               {
                 session = session.id;
                 byte_order = Message.byte_order;
                 streams = session.streams;
                 chunks = session.chunks;
               })))
  in
  let%bind ready = failed (Channel.receive control Message.reply_of_sexp) in
  let%bind format =
    match ready with
    | Ready format -> Ok format
    | Refused message -> Error (`Collector (Downstream message))
    | Done | Skipped | Committed _ | Rejected _ -> unexpected ready
  in
  let%map data =
    let protection = session.protection and id = session.id in
    Result.map_error
      (fun error -> `Collector error)
      (Channel.all session.streams ~close:Channel.close (fun () ->
               match Channel.connect protection peer with
               | Error (`Channel error) -> Error (Lost error)
               | Ok channel -> (
                   match
                     Channel.send channel (Message.sexp_of_attach { session = id })
                   with
                   | Ok () -> Ok channel
                   | Error (`Channel error) ->
                       Channel.close channel;
                       Error (Lost error))))
  in
  { control; data; format }
  in
  Result.iter_error (fun _ -> Channel.close control) greeted;
  greeted

let relay listening (session : session) (downstream : downstream) =
  let%bind () =
    failed
      (Channel.send session.control
         (Message.sexp_of_reply (Ready downstream.format)))
  in
  let%bind channels = attached listening session in
  let pairs =
    (session.control, downstream.control) :: List.combine channels downstream.data
  in
  failed
    (Base.Result.all_unit
       (Quiver_core.Concurrent.each (List.length pairs) (fun index ->
            let from, into = List.nth pairs index in
            let spliced = Channel.splice from into in
            Channel.close from;
            Channel.close into;
            Result.map_error (fun (`Channel error) -> error) spliced))
    |> Result.map_error (fun error -> `Channel error))

module Make (S : Quiver_core.Sink_intf.S) = struct
  module Part = Quiver_core.Assembly.Make (S)

  type failure : value mod contended portable =
    [ `Sink of S.error | `Collector of error ]

  (** Whether requests are still applied, or skipped after a refusal. *)
  type stance = Accepting | Refusing

  (** How the agent ended a chunk. *)
  type ending =
    | Ended of int  (** After so many batches. *)
    | Aborted

  type lane = {
    table : Quiver_core.Schema.Table.t;
    opened : (Part.t, S.error) result Await.Await.Ivar.t;
        (** Filled by the chunk's thread once it tried to open the part. *)
  }
  (** What the threads serving connections know of a chunk. *)

  type chunk = {
    id : int;
    name : Quiver_core.Schema.Name.t;
    index : int;
    lane : lane;
    ending : ending Await.Await.Ivar.t;  (** Filled by the control thread. *)
  }
  (** A chunk begun; a thread of its own opens its part and ends it. *)

  (** Whether chunks are passed on to a next hop that answers too. *)
  type forwarding = Alone | Chained

  (** What the next hop answered to a request this node applied. *)
  type passed : value mod contended portable =
    [ `Done | `Skipped | `Refused of string ]

  (** What the answerer tells the agent next, in the order of its requests. *)
  type verdict : value mod contended portable =
    [ `Say of Message.reply
    | `Settle of passed Await.Await.Ivar.t
      (** The next hop's answer to [Finish], before this node finishes. *)
    | `Skipped
    | `Applied of (unit, failure) result ]

  type finishing = {
    source : string;
    tables : (Quiver_core.Schema.Name.t * int) list;
    manifest : string;
    finished : (unit, failure) result Await.Await.Ivar.t;
  }
  (** An agent's [Finish], for the thread that started the session to apply. *)

  let run ?forward listening (session : session) sink =
    let parts =
      Await.Capsule.Await.With_mutex.create (fun () -> Hashtbl.create 256)
    and owners =
      Await.Capsule.Await.With_mutex.create (fun () -> Hashtbl.create 64)
    in
    let lanes = Registry.create ()
    and (passed : (unit, string) result Registry.t) = Registry.create ()
    and outbox = Await.Await.Mpmc_queue.create ()
    and (answers : (Message.reply, failure) result Quiver_core.Mailbox.t) =
      Quiver_core.Mailbox.create ()
    and (verdicts : verdict Quiver_core.Mailbox.t) = Quiver_core.Mailbox.create ()
    and (finishing : finishing option Await.Await.Ivar.t) =
      Await.Await.Ivar.create ()
    and (answered : (unit, failure) result Await.Await.Ivar.t) =
      Await.Await.Ivar.create ()
    and sink_failure = Portable.Atomic.make None
    and (first_failure : failure option Atomic.t) = Atomic.make None
    and (stance : stance Atomic.t) = Atomic.make Accepting
    and forwarding =
      match forward with None -> Alone | Some (_ : downstream) -> Chained
    and (stopped : passed) = `Refused "the session stopped" in
    let describe (error : failure) =
      match error with
      | `Sink error ->
          ignore
            (Portable.Atomic.compare_and_set sink_failure ~if_phys_equal_to:None
               ~replace_with:(Some error));
          Sexplib0.Sexp.to_string_hum (S.sexp_of_error error)
      | `Collector error -> Sexplib0.Sexp.to_string_hum (sexp_of_error error)
    in
    let say (reply : Message.reply) =
      Await.Await.Mpmc_queue.push outbox (Some reply)
    in
    let refused failure =
      Atomic.set stance Refusing;
      Message.Refused (describe failure)
    in
    let rec answerer () =
      let passed () : ((passed, failure) result) =
        match forward with
        | None -> Ok `Done
        | Some _ -> (
            match
              Option.value (Quiver_core.Mailbox.take answers)
                ~default:(Error (`Collector (Lost Channel.Closed)))
            with
            | Ok Message.Done -> Ok `Done
            | Ok Skipped -> Ok `Skipped
            | Ok (Refused reason) -> Ok (`Refused reason)
            | Ok reply -> unexpected reply
            | Error error -> Error error)
      in
      match Quiver_core.Mailbox.take verdicts with
      | None -> Ok ()
      | Some (`Say reply) ->
          say reply;
          answerer ()
      | Some (`Settle settled) ->
          let%bind passed = passed () in
          Await.Await.Ivar.fill_if_empty settled passed;
          answerer ()
      | Some ((`Skipped | `Applied _) as verdict) ->
          let%bind passed = passed () in
          say
            (match (verdict, passed) with
            | `Skipped, _ | `Applied (Ok ()), `Skipped -> Skipped
            | `Applied (Ok ()), `Done -> Done
            | `Applied (Error failure), _ -> refused failure
            | `Applied (Ok ()), `Refused reason ->
                refused (`Collector (Downstream reason)));
          answerer ()
    in
    let pass message =
      match forward with
      | None -> Ok ()
      | Some (downstream : downstream) ->
          failed
            (Channel.send downstream.control (Message.sexp_of_control message))
    in
    let%bind () =
      failed
        (Channel.send session.control
           (Message.sexp_of_reply (Ready (S.format sink))))
    in
    let%bind channels = attached listening session in
    let handles =
      List.map Channel.handle
        (session.control
         :: Option.fold ~none:[]
              ~some:(fun (downstream : downstream) -> [ downstream.control ])
              forward
        @ channels
        @ Option.fold ~none:[]
            ~some:(fun (downstream : downstream) -> downstream.data)
            forward)
    in
    let report = function
      | Ok () -> ()
      | Error error -> (
          match Atomic.compare_and_set first_failure None (Some error) with
          | false -> ()
          | true ->
              Registry.close lanes;
              Registry.close passed;
              Quiver_core.Mailbox.close verdicts;
              Await.Await.Mpmc_queue.push outbox None;
              Quiver_core.Mailbox.close answers;
              Await.Await.Ivar.fill_if_empty finishing None;
              Quiver_core.Guarded.hold owners (fun owners ->
                  Hashtbl.iter
                    (fun _ { ending; _ } ->
                      Await.Await.Ivar.fill_if_empty ending Aborted)
                    owners);
              List.iter Channel.sever handles)
    in
    let own { id; name; index; lane; ending } =
      let opened = Part.create sink lane.table ~index in
      Await.Await.Ivar.fill_if_empty lane.opened opened;
      let local =
        match
          (Await.Await.Ivar.read Quiver_core.Blocking.await ending, opened)
        with
        | Aborted, Ok part ->
            Part.abandon part;
            None
        | Aborted, Error _ -> None
        | Ended frames, Ok part ->
            let closed = Await.Await.Ivar.create () in
            Part.seal part ~batches:frames (fun outcome ->
                Await.Await.Ivar.fill_if_empty closed outcome);
            Some
              (match Await.Await.Ivar.read Quiver_core.Blocking.await closed with
              | Closed { part; _ } -> Ok part
              | Failed error -> Error (`Sink error))
        | Ended _, Error error -> Some (Error (`Sink error))
      in
      Quiver_core.Guarded.hold owners (fun owners -> Hashtbl.remove owners id);
      Option.iter
        (fun local ->
          Registry.forget lanes id;
          let downstream =
            match forwarding with
            | Alone -> Ok ()
            | Chained ->
                Result.map_error
                  (fun reason -> `Collector (Downstream reason))
                  (Option.value (Registry.await passed id)
                     ~default:(Error "gone before answering"))
          in
          Registry.forget passed id;
          say
            (match (local, downstream) with
            | Ok (part : Quiver_core.Manifest.part), Ok () ->
                Quiver_core.Guarded.hold parts (fun parts ->
                    Hashtbl.replace parts (name, index) part);
                Committed { chunk = id; part }
            | Error failure, _ | Ok _, Error failure ->
                Rejected { chunk = id; reason = describe failure }))
        local
    in
    let ended id (ending : ending) =
      Quiver_core.Guarded.hold owners (fun owners ->
          Option.iter
            (fun { ending = slot; _ } -> Await.Await.Ivar.fill_if_empty slot ending)
            (Hashtbl.find_opt owners id))
    in
    let rec write () =
      match Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await outbox with
      | None -> ()
      | Some reply -> (
          match
            failed (Channel.send session.control (Message.sexp_of_reply reply))
          with
          | Ok () -> write ()
          | Error _ -> ())
    in
    let rec listen_downstream (downstream : downstream) =
      match failed (Channel.receive downstream.control Message.reply_of_sexp) with
      | Error error ->
          Registry.close passed;
          ignore (Quiver_core.Mailbox.add answers (Error error))
      | Ok (Committed { chunk; _ }) ->
          Registry.publish passed chunk (Ok ());
          listen_downstream downstream
      | Ok (Rejected { chunk; reason }) ->
          Registry.publish passed chunk (Error reason);
          listen_downstream downstream
      | Ok ((Ready _ | Done | Refused _ | Skipped) as reply) ->
          ignore (Quiver_core.Mailbox.add answers (Ok reply));
          listen_downstream downstream
    in
    let control () =
      let tables = Hashtbl.create 64 in
      let rec next () =
        let%bind message =
          failed (Channel.receive session.control Message.control_of_sexp)
        in
        let answered local =
          let%map () = pass message in
          ignore
          @@ Quiver_core.Mailbox.add verdicts
               (match Atomic.get stance with
               | Refusing -> `Skipped
               | Accepting -> (
                   match local () with
                   | Ok () -> `Applied (Ok ())
                   | Error failure ->
                       Atomic.set stance Refusing;
                       `Applied (Error failure)))
        in
        match message with
        | Hello _ -> out_of_place "a second hello"
        | Database { name; ddl } ->
            let%bind () = answered (fun () -> S.database sink name ~ddl) in
            next ()
        | Table { table; ddl; parts } ->
            let%bind () =
              answered (fun () ->
                  Hashtbl.replace tables table.Quiver_core.Schema.Table.name table;
                  S.table sink table ~ddl ~parts)
            in
            next ()
        | Objects { database; definitions } ->
            let%bind () =
              answered (fun () -> S.objects sink database definitions)
            in
            next ()
        | Begin { chunk = id; table = name; index } -> (
            let%bind () = pass message in
            match
              ( Hashtbl.find_opt tables name,
                Quiver_core.Guarded.hold owners (fun owners -> Hashtbl.mem owners id) )
            with
            | None, _ -> out_of_place "a chunk of a table never announced"
            | Some _, true -> out_of_place "a chunk begun twice"
            | Some table, false -> (
                let lane = { table; opened = Await.Await.Ivar.create () } in
                let chunk =
                  { id; name; index; lane; ending = Await.Await.Ivar.create () }
                in
                Quiver_core.Guarded.hold owners (fun owners ->
                    Hashtbl.replace owners id chunk);
                Registry.publish lanes id lane;
                match Quiver_core.Concurrent.start (fun () -> own chunk) with
                | Ok (_ : unit Quiver_core.Concurrent.task) -> next ()
                | Error (`Concurrent _) -> out_of_place "no thread to own a chunk"))
        | End { chunk = id; frames } -> (
            let%bind () = pass message in
            match
              Quiver_core.Guarded.hold owners (fun owners -> Hashtbl.mem owners id)
            with
            | false -> out_of_place "the end of a chunk never begun"
            | true ->
                ended id (Ended frames);
                next ())
        | Abort { chunk = id } -> (
            let%bind () = pass message in
            match
              Quiver_core.Guarded.hold owners (fun owners -> Hashtbl.mem owners id)
            with
            | false -> out_of_place "an abort of a chunk never begun"
            | true ->
                ended id Aborted;
                next ())
        | Finish { source; tables; manifest } -> (
            match Atomic.get stance with
            | Refusing ->
                ignore (Quiver_core.Mailbox.add verdicts (`Say Message.Skipped));
                Ok ()
            | Accepting ->
                let%map () = pass message in
                let settled = Await.Await.Ivar.create () in
                let passed =
                  match Quiver_core.Mailbox.add verdicts (`Settle settled) with
                  | Delivered -> Await.Await.Ivar.read Quiver_core.Blocking.await settled
                  | Closed -> stopped
                in
                let reply =
                  match (Atomic.get stance, passed) with
                  | Refusing, _ | Accepting, `Skipped -> Message.Skipped
                  | Accepting, `Refused reason ->
                      refused (`Collector (Downstream reason))
                  | Accepting, `Done -> (
                      let finished = Await.Await.Ivar.create () in
                      Await.Await.Ivar.fill_if_empty finishing
                        (Some { source; tables; manifest; finished });
                      match Await.Await.Ivar.read Quiver_core.Blocking.await finished with
                      | Ok () -> Done
                      | Error failure -> refused failure)
                in
                ignore (Quiver_core.Mailbox.add verdicts (`Say reply)))
      in
      next ()
    in
    let serve channel next =
      let batches = Base.Hashtbl.create (module Quiver_core.Schema.Name)
      and inflated = Quiver_core.Byte_buffer.create () in
      let malformed reason = Error (`Collector (Lost (Malformed reason))) in
      let deliver lane ~codec ~seq ~raw ~payload =
        match Await.Await.Ivar.read Quiver_core.Blocking.await lane.opened with
        | Error _ -> Ok ()
        | Ok part -> (
            Quiver_core.Byte_buffer.clear inflated;
            match
              match (codec : Frame.codec) with
              | Plain ->
                  Ok (Quiver_core.Byte_buffer.append_string inflated payload)
              | Zstd -> Quiver_sink.Compression.unzstd payload ~into:inflated
            with
            | Error reason -> malformed reason
            | Ok () when Quiver_core.Byte_buffer.length inflated <> raw ->
                malformed "decompressed length differs from the header's"
            | Ok () -> (
                let { Quiver_core.Schema.Table.name; fields = first, rest; _ } =
                  lane.table
                in
                let batch =
                  Base.Hashtbl.find_or_add batches name ~default:(fun () ->
                      Quiver_core.Batch.create
                        (List.map
                           (fun ({ ltype; _ } : Quiver_core.Schema.Field.t) ->
                             ltype)
                           (first :: rest)))
                in
                match
                  Quiver_core.Batch.decode batch inflated.bs ~stop:inflated.len
                with
                | Error (`Batch error) ->
                    malformed
                      (Sexplib0.Sexp.to_string_hum
                         (Quiver_core.Batch.sexp_of_error
                            (match error with
                            | Malformed -> Malformed
                            | Ragged { column; length; rows } ->
                                Ragged { column; length; rows })))
                | Ok sealed ->
                    Part.deliver part seq (Part.render part sealed);
                    Ok ()))
      in
      let rec loop () =
        match Channel.read channel with
        | Error (`Channel Channel.Closed) -> Ok ()
        | Error _ as error -> failed error
        | Ok (Message _) -> out_of_place "a message on a data connection"
        | Ok (Batch { codec; chunk = id; seq; raw; header; payload }) -> (
            match
              Option.fold ~none:(Ok ())
                ~some:(fun next -> failed (Channel.forward next ~header ~payload))
                next
            with
            | Error _ as error -> error
            | Ok () -> (
                match Registry.await lanes id with
                | None -> out_of_place "a batch of a chunk never begun"
                | Some lane -> (
                    match deliver lane ~codec ~seq ~raw ~payload with
                    | Error _ as error -> error
                    | Ok () -> loop ())))
      in
      let (served : (unit, failure) result) = loop () in
      Option.iter Channel.finish next;
      Channel.close channel;
      served
    in
    let launch work ~unstarted =
      match Quiver_core.Concurrent.start work with
      | Ok task -> Some task
      | Error (`Concurrent _) ->
          report (out_of_place "no thread for the session");
          unstarted ();
          None
    in
    let threads =
      [
        launch
          (fun () ->
            let answer = answerer () in
            report answer;
            Quiver_core.Mailbox.close verdicts;
            let rec settle_rest () =
              match Quiver_core.Mailbox.take verdicts with
              | None -> ()
              | Some (`Settle settled) ->
                  Await.Await.Ivar.fill_if_empty settled stopped;
                  settle_rest ()
              | Some (`Say _ | `Skipped | `Applied _) -> settle_rest ()
            in
            settle_rest ();
            Await.Await.Ivar.fill_if_empty answered answer)
          ~unstarted:(fun () -> Await.Await.Ivar.fill_if_empty answered (Ok ()));
        launch
          (fun () ->
            let controlled = control () in
            Registry.close lanes;
            Quiver_core.Mailbox.close verdicts;
            Await.Await.Ivar.fill_if_empty finishing None;
            let answer = Await.Await.Ivar.read Quiver_core.Blocking.await answered in
            Await.Await.Mpmc_queue.push outbox None;
            Option.iter
              (fun (downstream : downstream) -> Channel.finish downstream.control)
              forward;
            report (Result.bind controlled (fun () -> answer)))
          ~unstarted:ignore;
        launch write ~unstarted:ignore;
      ]
      @ Option.fold ~none:[]
          ~some:(fun (downstream : downstream) ->
            [ launch (fun () -> listen_downstream downstream) ~unstarted:ignore ])
          forward
      @ List.mapi
          (fun index (channel : Channel.t) ->
            let next =
              Option.map
                (fun ({ data; _ } : downstream) -> List.nth data index)
                forward
            in
            launch (fun () -> report (serve channel next)) ~unstarted:ignore)
          channels
    in
    Option.iter
      (fun { source; tables; manifest; finished } ->
        Await.Await.Ivar.fill_if_empty finished
          (let%bind manifest =
             Result.map_error
               (fun (`Manifest error) -> `Collector (Manifest error))
               (let%bind json =
                  match Yojson.Safe.from_string manifest with
                  | json -> Ok json
                  | exception Yojson.Json_error reason ->
                      Error (`Manifest (Quiver_core.Manifest.Malformed reason))
                in
                Quiver_core.Manifest.localize json ~format:(S.format sink)
                  (fun table index ->
                    Quiver_core.Guarded.hold parts (fun parts ->
                        Hashtbl.find_opt parts (table, index))))
           in
           S.finish sink ~source ~tables ~manifest))
      (Await.Await.Ivar.read Quiver_core.Blocking.await finishing);
    List.iter (Option.iter Quiver_core.Concurrent.join) threads;
    let outcome =
      Option.fold ~none:(Ok ()) ~some:Result.error (Atomic.get first_failure)
    in
    Channel.close session.control;
    Option.iter
      (fun (downstream : downstream) -> Channel.close downstream.control)
      forward;
    match (Portable.Atomic.get sink_failure, outcome) with
    | Some error, _ -> Error (`Sink error)
    | None, Ok () -> Ok ()
    | None, Error (`Sink error) -> Error (`Sink error)
    | None, Error (`Collector error) -> Error (`Collector error)
end
