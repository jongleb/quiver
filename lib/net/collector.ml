open Base.Result.Let_syntax

type error : value mod contended portable =
  | Lost of Channel.error
  | Damaged of Frame.error
  | Foreign_byte_order of Message.byte_order
  | Out_of_place of string
  | Downstream of string
  | Manifest of Quiver_core.Manifest.error
  | Abandoned
[@@deriving sexp_of ~portable]

type session = {
  control : Channel.t;
  id : string;
  streams : int;
  chunks : int;
  protection : Protection.t;
  reconnect : Core.Time_ns.Span.t;
      (** How long the agent may take to replace a broken control connection. *)
  introduction : Core.Time_ns.Span.t;
      (** How long a connection accepted may take to say what it is. *)
}

type downstream = {
  control : Channel.t;
  format : Quiver_core.Output_format.t;
  columns : Message.columns;  (** What the next hop takes. *)
  peer : Channel.peer;  (** Where a data connection goes for each that arrives. *)
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

let accept listening protection ~reconnect ~introduction =
  let%bind arrival = failed (Channel.arrive listening) in
  let%bind control, hello =
    failed
      (Channel.introduced arrival protection ~within:introduction
         Message.control_of_sexp)
  in
  let greeted =
    match hello with
    | Hello { byte_order; _ } when byte_order <> Message.byte_order ->
        Error (`Collector (Foreign_byte_order byte_order))
    | Hello { session; streams; chunks; _ } ->
        Ok { control; id = session; streams; chunks; protection; reconnect; introduction }
    | Database _ | Table _ | Grown _ | Accepts _ | Objects _ | Begin _ | End _ | Abort _ | Finish _
    | Heartbeat _ | Leave ->
        out_of_place "before hello"
  in
  Result.iter_error (fun _ -> Channel.close control) greeted;
  greeted

let chunks (session : session) = session.chunks

let forward peer (session : session) =
  let (forwarded : (downstream, [ `Collector of error ]) result) =
  let%bind control = failed (Channel.connect session.protection peer) in
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
  let%map format, columns =
    match ready with
    | Ready { format; columns } -> Ok (format, columns)
    | Refused message | Failed message -> Error (`Collector (Downstream message))
    | Done | Skipped | Committed _ | Rejected _ | Heard _ -> unexpected ready
  in
  { control; format; columns; peer }
  in
  Result.iter_error (fun _ -> Channel.close control) greeted;
  greeted
  in
  match forwarded with
  | Ok downstream -> Ok downstream
  | Error (`Collector error) ->
      ignore
        (Channel.send session.control
           (Message.sexp_of_reply
              (Refused
                 ("the next hop: "
                 ^ Sexplib0.Sexp.to_string_hum (sexp_of_error error)))));
      Channel.close session.control;
      Error (`Collector error)

let reaching (session : session) (downstream : downstream) attach =
  match Channel.connect session.protection downstream.peer with
  | Error (`Channel _) -> None
  | Ok next -> (
      match Channel.send next (Message.sexp_of_attach attach) with
      | Ok () -> Some next
      | Error (`Channel _) ->
          Channel.close next;
          None)

let accepting listening (session : session) take =
  let attaching arrival =
    match
      Channel.introduced arrival session.protection ~within:session.introduction
        Message.attach_of_sexp
    with
    | Ok (channel, ((Data { session = id } | Control { session = id; _ }) as attach))
      when String.equal id session.id ->
        Some (channel, attach)
    | Ok (channel, (Data _ | Control _)) ->
        Channel.close channel;
        None
    | Error (`Channel _ | `Frame _) -> None
  in
  let rec go taken =
    match Channel.arrive listening with
    | Error (`Channel Channel.Closed) -> taken
    | Error (`Channel _) ->
        let pause = 0.01 in
        Unix.sleepf pause;
        go taken
    | Ok arrival -> (
        match
          Quiver_core.Concurrent.start (fun () ->
              Option.iter take (attaching arrival))
        with
        | Ok task -> go (task :: taken)
        | Error (`Concurrent _) ->
            Channel.release arrival;
            go taken)
  in
  List.iter Quiver_core.Concurrent.join (go [])

(** A control connection of the agent passed on to the next hop. *)
type pair = {
  number : int;  (** Control pairs before it. *)
  ended : (unit, Channel.error) result Await.Await.Ivar.t;
  ends : Channel.t list;  (** Both of its connections. *)
}

(** Whether the relay still pairs the connections that arrive. *)
type course = Pairing | Ended

type pairs = {
  mutable control : pair;  (** The latest. *)
  mutable opened : Channel.t list;
      (** Every pair's connections, closed only once the relay ends, so ending
          one never reaches a descriptor reused since. *)
  mutable course : course;
}

let relay listening (session : session) (downstream : downstream) =
  match
    failed
      (Channel.send session.control
         (Message.sexp_of_reply (Ready { format = downstream.format; columns = downstream.columns })))
  with
  | Error _ as error ->
      List.iter Channel.close [ session.control; downstream.control ];
      error
  | Ok () ->
      let (spliced @ portable) from into =
        Result.map_error (fun (`Channel error) -> error) (Channel.splice from into)
      in
      let (steered @ portable) (from : Channel.t) (into : Channel.t) =
        let left = Portable.Atomic.make `Staying in
        let passed =
          Quiver_core.Concurrent.each 2 (fun direction ->
              let rec up () =
                match Channel.receive from Message.control_of_sexp with
                | Ok message -> (
                    (match message with
                    | Leave -> Portable.Atomic.set left `Left
                    | Hello _ | Database _ | Table _ | Grown _ | Accepts _ | Objects _ | Begin _ | End _
                    | Abort _ | Finish _ | Heartbeat _ ->
                        ());
                    match Channel.send into (Message.sexp_of_control message) with
                    | Ok () -> up ()
                    | Error (`Channel error) -> Error error)
                | Error (`Channel Channel.Closed) -> (
                    Channel.finish into;
                    match Portable.Atomic.get left with
                    | `Left -> Ok ()
                    | `Staying -> Error Channel.Closed)
                | Error (`Channel error) -> Error error
                | Error (`Frame (_ : Frame.error)) ->
                    Error (Channel.Malformed "a damaged frame")
              and down () =
                match Channel.receive into Message.reply_of_sexp with
                | Ok reply -> (
                    match Channel.send from (Message.sexp_of_reply reply) with
                    | Ok () -> down ()
                    | Error (`Channel error) -> Error error)
                | Error (`Channel Channel.Closed) ->
                    Channel.finish from;
                    Ok ()
                | Error (`Channel error) -> Error error
                | Error (`Frame (_ : Frame.error)) ->
                    Error (Channel.Malformed "a damaged frame")
              in
              let passed = match direction with 0 -> up () | _ -> down () in
              Result.iter_error
                (fun (_ : Channel.error) ->
                  Channel.abort from;
                  Channel.abort into)
                passed;
              passed)
        in
        match (Portable.Atomic.get left, passed) with
        | `Left, _ -> Ok ()
        | `Staying, (Error error :: _ | _ :: Error error :: _) -> Error error
        | `Staying, _ -> Error Channel.Closed
      in
      let first =
        {
          number = 0;
          ended = Await.Await.Ivar.create ();
          ends = [ session.control; downstream.control ];
        }
      in
      let pairs =
        Await.Capsule.Await.With_mutex.create (fun () ->
            { control = first; opened = first.ends; course = Pairing })
      in
      let (started @ portable) work =
        Result.to_option (Quiver_core.Concurrent.start work)
      in
      let controlled =
        started (fun () ->
            Await.Await.Ivar.fill_if_empty first.ended
              (steered session.control downstream.control))
      in
      (match controlled with
      | Some (_ : unit Quiver_core.Concurrent.task) -> ()
      | None ->
          Await.Await.Ivar.fill_if_empty first.ended
            (Error (Channel.Io "no thread for the control connection")));
      let (opening @ portable) pairs ends =
        pairs.opened <- ends @ pairs.opened;
        match pairs.course with
        | Pairing -> ()
        | Ended -> List.iter Channel.abort ends
      in
      let arriving =
        started (fun () ->
            accepting listening session (fun (channel, attach) ->
                match reaching session downstream attach with
                | None -> Channel.close channel
                | Some next -> (
                    let ends = [ channel; next ] in
                    match attach with
                    | Data _ ->
                        Quiver_core.Guarded.hold pairs (fun pairs -> opening pairs ends);
                        ignore (spliced channel next)
                    | Control _ ->
                        let ended = Await.Await.Ivar.create () in
                        Quiver_core.Guarded.hold pairs (fun pairs ->
                            List.iter Channel.abort pairs.control.ends;
                            pairs.control <-
                              { number = pairs.control.number + 1; ended; ends };
                            opening pairs ends);
                        Await.Await.Ivar.fill_if_empty ended (steered channel next))))
      in
      let latest () = Quiver_core.Guarded.hold pairs (fun pairs -> pairs.control) in
      let rec watch ({ number; ended; _ } : pair) =
        match Await.Await.Ivar.read Quiver_core.Blocking.await ended with
        | Ok () -> Ok ()
        | Error error ->
            let waited = Quiver_core.Stopwatch.start () in
            let rec replaced () =
              match latest () with
              | pair when pair.number <> number -> watch pair
              | (_ : pair)
                when Core.Time_ns.Span.( > )
                       (Quiver_core.Stopwatch.elapsed waited)
                       session.reconnect ->
                  Error error
              | (_ : pair) ->
                  Unix.sleepf 0.01;
                  replaced ()
            in
            replaced ()
      in
      let outcome = watch first in
      Quiver_core.Guarded.hold pairs (fun pairs ->
          pairs.course <- Ended;
          List.iter Channel.abort pairs.opened);
      Channel.stop listening;
      List.iter (Option.iter Quiver_core.Concurrent.join) [ arriving; controlled ];
      Quiver_core.Guarded.hold pairs (fun pairs ->
          List.iter Channel.close pairs.opened);
      failed (Result.map_error (fun error -> `Channel error) outcome)

module Make (S : Quiver_core.Sink_intf.S) = struct
  module Part = Quiver_core.Assembly.Make (S)

  type failure : value mod contended portable =
    [ `Sink of S.error | `Collector of error | `Mask of Quiver_core.Mask.error ]

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

  type attached = { mutable handles : Channel.handle list }
  (** Data connections taken, to end at a failure or once the session is over. *)

  (** Where a chunk's batches go. *)
  type arrival =
    | Into of lane
    | Settled  (** Answered: batches sent again are dropped. *)

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

  (** What the control writer sends next. *)
  type outgoing = Reply of Message.reply @@ aliased many | Beat | Last

  type steering = {
    mutable channel : Channel.t;  (** The control connection now. *)
    mutable generation : int;  (** Control connections before it. *)
    mutable greeting : int;
        (** Messages that had arrived when it replaced the one before. *)
    unheard : Message.reply Queue.t;
        (** Counted replies sent and not known to have arrived, oldest first. *)
    mutable first : int;  (** The number of the oldest of them. *)
    mutable replacement : (Channel.t * int) option;
        (** A control connection that came in, and the replies that had
            arrived. *)
    mutable retired : Channel.t list;  (** Replaced, closed at the end. *)
  }
  (** The agent's control connection, which a new one replaces when it breaks.
  *)

  (** Whether the session still beats. *)
  type life = Live | Over

  let run ?forward listening (session : session) sink ~columns =
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
    and (stopped : passed) = `Refused "the session stopped"
    and steering =
      Await.Capsule.Await.With_mutex.create (fun () ->
          {
            channel = session.control;
            generation = 0;
            greeting = 0;
            unheard = Queue.create ();
            first = 0;
            replacement = None;
            retired = [];
          })
    and messages = Portable.Atomic.make 0
    and downstream_replies = Portable.Atomic.make 0
    and life = Portable.Atomic.make Live
    and passing = Await.Capsule.Await.With_mutex.create (fun () -> ()) in
    let describe (error : failure) =
      match error with
      | `Sink error ->
          ignore
            (Portable.Atomic.compare_and_set sink_failure ~if_phys_equal_to:None
               ~replace_with:(Some error));
          Sexplib0.Sexp.to_string_hum (S.sexp_of_error error)
      | `Collector error -> Sexplib0.Sexp.to_string_hum (sexp_of_error error)
      | `Mask error -> Sexplib0.Sexp.to_string_hum (Quiver_core.Mask.sexp_of_error error)
    in
    let hear steering replies =
      while steering.first < replies && not (Queue.is_empty steering.unheard) do
        ignore (Queue.pop steering.unheard : Message.reply);
        steering.first <- steering.first + 1
      done
    in
    let say (reply : Message.reply) =
      Await.Await.Mpmc_queue.push outbox (Reply reply)
    in
    let turned_away channel failure =
      ignore
        (Channel.send channel (Message.sexp_of_reply (Failed (describe failure))));
      Channel.close channel
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
            match%bind
              Option.value (Quiver_core.Mailbox.take answers)
                ~default:(Error (`Collector (Lost Channel.Closed)))
            with
            | Message.Done -> Ok `Done
            | Skipped -> Ok `Skipped
            | Refused reason -> Ok (`Refused reason)
            | reply -> unexpected reply)
      in
      match Quiver_core.Mailbox.take verdicts with
      | None -> Ok ()
      | Some (`Say reply) ->
          say reply;
          answerer ()
      | Some (`Settle settled) ->
          let passed = passed () in
          Await.Await.Ivar.fill_if_empty settled (Result.value passed ~default:stopped);
          let%bind (_ : passed) = passed in
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
            (Quiver_core.Guarded.hold passing (fun () ->
                 (Channel.send downstream.control (Message.sexp_of_control message)
                   : (unit, [ `Channel of Channel.error ]) result)))
    in
    match
      failed
        (Channel.send session.control
           (Message.sexp_of_reply
              (Ready
                 {
                   format = S.format sink;
                   columns =
                     Option.fold forward ~none:columns ~some:(fun (downstream : downstream) ->
                         match ((columns : Message.columns), downstream.columns) with
                         | Written, Written -> Written
                         | Stored_too, _ | _, Stored_too -> Stored_too);
                 })))
    with
    | Error _ as error ->
        Channel.close session.control;
        Option.iter (fun (downstream : downstream) -> Channel.close downstream.control) forward;
        error
    | Ok () ->
    let handles =
      Option.fold ~none:[]
        ~some:(fun (downstream : downstream) -> [ Channel.handle downstream.control ])
        forward
    in
    let attached =
      Await.Capsule.Await.With_mutex.create (fun () -> { handles = [] })
    in
    let report = function
      | Ok () -> ()
      | Error error -> (
          match Atomic.compare_and_set first_failure None (Some error) with
          | false -> ()
          | true ->
              Portable.Atomic.set life Over;
              say (Failed (describe error));
              Await.Await.Mpmc_queue.push outbox Last;
              ignore (pass Leave);
              Channel.stop listening;
              List.iter Channel.sever
                (Quiver_core.Guarded.hold attached (fun { handles } -> handles));
              Registry.close lanes;
              Registry.close passed;
              Quiver_core.Mailbox.close verdicts;
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
              | Failed error -> Error (error :> failure))
        | Ended _, Error error -> Some (Error (`Sink error))
      in
      Quiver_core.Guarded.hold owners (fun owners -> Hashtbl.remove owners id);
      Option.iter
        (fun local ->
          Registry.forget lanes id;
          Registry.publish lanes id Settled;
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
    let rec write generation =
      match Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await outbox with
      | Last -> (
          match Atomic.get first_failure with
          | None -> ()
          | Some _ ->
              Quiver_core.Guarded.hold steering (fun steering ->
                  Channel.abort steering.channel))
      | (Reply _ | Beat) as outgoing ->
          let channel, current, replay =
            Quiver_core.Guarded.hold steering (fun steering ->
                (match outgoing with
                | Reply ((Done | Refused _ | Skipped | Committed _ | Rejected _) as reply)
                  ->
                    Queue.push reply steering.unheard
                | Reply (Ready _ | Heard _ | Failed _) | Beat | Last -> ());
                ( steering.channel,
                  steering.generation,
                  match steering.generation = generation with
                  | true -> []
                  | false ->
                      Message.Heard { messages = steering.greeting }
                      :: List.of_seq (Queue.to_seq steering.unheard) ))
          in
          let own =
            match (outgoing, replay) with
            | Reply (Done | Refused _ | Skipped | Committed _ | Rejected _), _ :: _
            | Last, _ ->
                []
            | Reply reply, _ -> [ reply ]
            | Beat, _ -> [ Message.Heard { messages = Portable.Atomic.get messages } ]
          in
          ignore
            (Base.List.fold_result (replay @ own) ~init:() ~f:(fun () reply ->
                 Channel.send channel (Message.sexp_of_reply reply)));
          write current
    in
    let rec listen_downstream (downstream : downstream) =
      match failed (Channel.receive downstream.control Message.reply_of_sexp) with
      | Error error ->
          Registry.close passed;
          ignore (Quiver_core.Mailbox.add answers (Error error))
      | Ok (Failed reason) ->
          Registry.close passed;
          ignore
            (Quiver_core.Mailbox.add answers
               (Error (`Collector (Downstream reason))))
      | Ok (Heard _) -> listen_downstream downstream
      | Ok (Committed { chunk; _ }) ->
          Portable.Atomic.incr downstream_replies;
          Registry.publish passed chunk (Ok ());
          listen_downstream downstream
      | Ok (Rejected { chunk; reason }) ->
          Portable.Atomic.incr downstream_replies;
          Registry.publish passed chunk (Error reason);
          listen_downstream downstream
      | Ok ((Ready _ | Done | Refused _ | Skipped) as reply) ->
          Portable.Atomic.incr downstream_replies;
          ignore (Quiver_core.Mailbox.add answers (Ok reply));
          listen_downstream downstream
    in
    let control () =
      let tables = Hashtbl.create 64 in
      let finished () =
        match Await.Await.Ivar.peek finishing with
        | This (Some _) -> `Finished
        | This None | Null -> `Unfinished
      in
      let resumed error =
        let waited = Quiver_core.Stopwatch.start () in
        let rec wait () =
          match Atomic.get first_failure with
          | Some failure ->
              Option.iter
                (fun (channel, _) -> turned_away channel failure)
                (Quiver_core.Guarded.hold steering (fun steering ->
                     let pending = steering.replacement in
                     steering.replacement <- None;
                     pending));
              Error (`Collector error)
          | None -> (
              match
                Quiver_core.Guarded.hold steering (fun steering ->
                    match steering.replacement with
                    | None -> `Waiting
                    | Some (channel, replies) -> (
                        steering.replacement <- None;
                        match
                          replies < steering.first
                          || replies > steering.first + Queue.length steering.unheard
                        with
                        | true ->
                            Channel.close channel;
                            `Skipping
                        | false ->
                            hear steering replies;
                            steering.retired <- steering.channel :: steering.retired;
                            steering.channel <- channel;
                            steering.generation <- steering.generation + 1;
                            steering.greeting <- Portable.Atomic.get messages;
                            `Resumed channel))
              with
              | `Resumed channel ->
                  Await.Await.Mpmc_queue.push outbox Beat;
                  Ok channel
              | `Skipping -> out_of_place "a control connection that skips replies"
              | `Waiting
                when Core.Time_ns.Span.( > )
                       (Quiver_core.Stopwatch.elapsed waited)
                       session.reconnect ->
                  Error (`Collector error)
              | `Waiting ->
                  Unix.sleepf 0.01;
                  wait ())
        in
        wait ()
      in
      let rec next stage channel =
        match failed (Channel.receive channel Message.control_of_sexp) with
        | Error (`Collector (Lost (Malformed _))) as error -> error
        | Error (`Collector error) -> (
            match (resumed error, finished ()) with
            | Ok channel, _ -> next stage channel
            | Error _, `Finished -> Ok ()
            | (Error _ as error), `Unfinished -> error)
        | Ok
            ( Hello _ | Database _ | Table _ | Grown _ | Accepts _ | Objects _ | Begin _ | End _
            | Abort _ | Finish _ )
          when stage = `Concluded ->
            out_of_place "a message after finish"
        | Ok message ->
        (match message with
        | Hello _ | Heartbeat _ | Leave -> ()
        | Database _ | Table _ | Grown _ | Accepts _ | Objects _ | Begin _ | End _ | Abort _ | Finish _
          ->
            Portable.Atomic.incr messages);
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
        | Database { name; ddl; sequences } ->
            let%bind () = answered (fun () -> S.database sink name ~ddl ~sequences) in
            next stage channel
        | Table { table; ddl; parts; order; splits } -> (
            let checked =
              match (table.Quiver_core.Schema.Table.key, splits) with
              | _, [] -> Ok []
              | Primary key, _ :: _ ->
                  Result.map_error ignore
                    (Base.Result.all
                       (List.map (fun (first, rest) -> Quiver_core.Key.make key (first :: rest)) splits))
              | (Unique _ | Indexed _ | Row_id _ | No_key), _ :: _ -> Error ()
            in
            match checked with
            | Error () -> out_of_place "splits that are not of the table's primary key"
            | Ok splits ->
                let%bind () =
                  answered (fun () ->
                      Hashtbl.replace tables table.name table;
                      S.table sink table ~ddl ~parts ~order ~splits)
                in
                next stage channel)
        | Grown { table = name } -> (
            match Hashtbl.find_opt tables name with
            | None -> out_of_place "a part of a table never announced"
            | Some table ->
                let%bind () = answered (fun () -> S.add_part sink table) in
                next stage channel)
        | Accepts { database; entries } ->
            let%bind () = answered (fun () -> S.accepts sink database entries) in
            next stage channel
        | Objects { database; definitions } ->
            let%bind () =
              answered (fun () -> S.objects sink database definitions)
            in
            next stage channel
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
                Registry.publish lanes id (Into lane);
                match Quiver_core.Concurrent.start (fun () -> own chunk) with
                | Ok (_ : unit Quiver_core.Concurrent.task) -> next stage channel
                | Error (`Concurrent _) -> out_of_place "no thread to own a chunk"))
        | End { chunk = id; frames } -> (
            let%bind () = pass message in
            match
              Quiver_core.Guarded.hold owners (fun owners -> Hashtbl.mem owners id)
            with
            | false -> out_of_place "the end of a chunk never begun"
            | true ->
                ended id (Ended frames);
                next stage channel)
        | Abort { chunk = id } -> (
            let%bind () = pass message in
            match
              Quiver_core.Guarded.hold owners (fun owners -> Hashtbl.mem owners id)
            with
            | false -> out_of_place "an abort of a chunk never begun"
            | true ->
                ended id Aborted;
                next stage channel)
        | Finish { source; tables; manifest } -> (
            match Atomic.get stance with
            | Refusing ->
                ignore (Quiver_core.Mailbox.add verdicts (`Say Message.Skipped));
                next `Concluded channel
            | Accepting ->
                let%bind () = pass message in
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
                ignore (Quiver_core.Mailbox.add verdicts (`Say reply));
                next `Concluded channel)
        | Heartbeat { replies } ->
            Quiver_core.Guarded.hold steering (fun steering ->
                hear steering replies);
            let%bind () =
              pass (Heartbeat { replies = Portable.Atomic.get downstream_replies })
            in
            next stage channel
        | Leave -> (
            let%bind () = pass message in
            match finished () with
            | `Finished -> Ok ()
            | `Unfinished -> Error (`Collector Abandoned))
      in
      next `Open session.control
    in
    let serve channel next =
      let batches = Base.Hashtbl.create (module Quiver_core.Schema.Name)
      and inflated = Quiver_core.Byte_buffer.create ()
      and inflater = Quiver_sink.Compression.inflater () in
      let malformed reason = Error (`Collector (Lost (Malformed reason))) in
      let deliver lane ~codec ~seq ~raw ~payload ~length =
        match Await.Await.Ivar.read Quiver_core.Blocking.await lane.opened with
        | Error _ -> Ok ()
        | Ok part -> (
            match
              match (codec : Frame.codec) with
              | Plain ->
                  (* Batch.decode only reads the bytes. *)
                  Ok (Bytes.unsafe_of_string payload, length)
              | Zstd ->
                  Quiver_core.Byte_buffer.clear inflated;
                  Result.map
                    (fun () -> (inflated.bs, inflated.len))
                    (Quiver_sink.Compression.unzstd inflater payload ~length
                       ~bound:raw ~into:inflated)
            with
            | Error reason -> malformed reason
            | Ok (_, stop) when stop <> raw ->
                malformed "decompressed length differs from the header's"
            | Ok (decoded, stop) -> (
                let { Quiver_core.Schema.Table.name; _ } = lane.table in
                let batch =
                  Base.Hashtbl.find_or_add batches name ~default:(fun () ->
                      Quiver_core.Batch.create
                        (List.map
                           (fun ({ ltype; _ } : Quiver_core.Schema.Field.t) ->
                             ltype)
                           (Quiver_core.Schema.Table.columns lane.table)))
                in
                match
                  Quiver_core.Batch.decode batch decoded ~stop
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
                    Part.deliver part seq (Part.render part Quiver_core.Mask.Table.none sealed);
                    Ok ()))
      in
      let acknowledged =
        match next with
        | None -> fun received -> Channel.acknowledge channel ~received
        | Some (_ : Channel.t) -> fun (_ : int) -> Ok ()
      in
      let rec loop received =
        match Channel.read channel with
        | Error (`Channel Channel.Closed) -> Ok `Finished
        | Error (`Channel _ | `Frame _) -> Ok `Broken
        | Ok (Message _) -> out_of_place "a message on a data connection"
        | Ok (Acknowledged _) -> out_of_place "an ack from the agent"
        | Ok (Batch { codec; chunk = id; seq; raw; header; payload; length })
          -> (
            let received = received + 1 in
            match
              ( Option.fold ~none:(Ok ())
                  ~some:(fun next -> Channel.forward next ~header ~payload ~length)
                  next,
                acknowledged received )
            with
            | Error (`Channel _), _ | _, Error (`Channel _) -> Ok `Broken
            | Ok (), Ok () -> (
                match Registry.await lanes id with
                | None -> out_of_place "a batch of a chunk never begun"
                | Some Settled -> loop received
                | Some (Into lane) -> (
                    match deliver lane ~codec ~seq ~raw ~payload ~length with
                    | Error _ as error -> error
                    | Ok () -> loop received)))
      in
      let passing =
        Option.map
          (fun (next : Channel.t) ->
            Quiver_core.Concurrent.start (fun () ->
                let rec pass () =
                  match Channel.read next with
                  | Ok (Acknowledged received) -> (
                      match Channel.acknowledge channel ~received with
                      | Ok () -> pass ()
                      | Error (`Channel _) -> ())
                  | Ok (Message _ | Batch _) | Error (`Channel _ | `Frame _) -> ()
                in
                pass ();
                Channel.abort channel))
          next
      in
      let (served : (_, failure) result) = loop 0 in
      (match served with
      | Ok `Finished -> Option.iter Channel.finish next
      | Ok `Broken | Error _ ->
          Option.iter Channel.abort next;
          Channel.abort channel);
      Option.iter
        (function
          | Ok task -> Quiver_core.Concurrent.join task
          | Error (`Concurrent _) -> Option.iter Channel.abort next)
        passing;
      Channel.close channel;
      Result.map (fun (`Finished | `Broken) -> ()) served
    in
    let launch work ~unstarted =
      match Quiver_core.Concurrent.start work with
      | Ok task -> Some task
      | Error (`Concurrent _) ->
          report (out_of_place "no thread for the session");
          unstarted ();
          None
    in
    let steering_threads =
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
            Portable.Atomic.set life Over;
            Registry.close lanes;
            Quiver_core.Mailbox.close verdicts;
            Await.Await.Ivar.fill_if_empty finishing None;
            let answer = Await.Await.Ivar.read Quiver_core.Blocking.await answered in
            Await.Await.Mpmc_queue.push outbox Last;
            let outcome = Result.bind controlled (fun () -> answer) in
            report outcome;
            Result.iter
              (fun () ->
                Option.iter
                  (fun (downstream : downstream) -> Channel.finish downstream.control)
                  forward)
              outcome)
          ~unstarted:ignore;
        launch (fun () -> write 0) ~unstarted:ignore;
      ]
      @ Option.fold ~none:[]
          ~some:(fun (downstream : downstream) ->
            [ launch (fun () -> listen_downstream downstream) ~unstarted:ignore ])
          forward
    in
    let arriving =
      launch
        (fun () ->
          accepting listening session (fun (channel, attach) ->
              match attach with
              | Control { replies; _ } -> (
                  match Atomic.get first_failure with
                  | Some failure -> turned_away channel failure
                  | None ->
                      Channel.sever
                        (Quiver_core.Guarded.hold steering (fun steering ->
                             Option.iter
                               (fun (pending, _) -> Channel.close pending)
                               steering.replacement;
                             steering.replacement <- Some (channel, replies);
                             Channel.handle steering.channel)))
              | Data _ ->
              match
                Option.fold ~none:(Some None)
                  ~some:(fun downstream ->
                    Option.map Option.some (reaching session downstream attach))
                  forward
              with
              | None -> Channel.close channel
              | Some next ->
                  let held =
                    List.map Channel.handle
                      (channel :: Option.to_list next)
                  in
                  Quiver_core.Guarded.hold attached (fun attached ->
                      attached.handles <- held @ attached.handles);
                  (match Portable.Atomic.get life with
                  | Over -> List.iter Channel.sever held
                  | Live -> ());
                  report (serve channel next)))
        ~unstarted:ignore
    in
    ignore
      (Quiver_core.Concurrent.start (fun () ->
           let rec beat () =
             Unix.sleepf 1.;
             match Portable.Atomic.get life with
             | Over -> ()
             | Live ->
                 Await.Await.Mpmc_queue.push outbox Beat;
                 beat ()
           in
           beat ()));
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
    List.iter (Option.iter Quiver_core.Concurrent.join) steering_threads;
    List.iter Channel.sever
      (Quiver_core.Guarded.hold attached (fun attached -> attached.handles));
    Channel.stop listening;
    Option.iter Quiver_core.Concurrent.join arriving;
    let outcome =
      Option.fold ~none:(Ok ()) ~some:Result.error (Atomic.get first_failure)
    in
    Quiver_core.Guarded.hold steering (fun steering ->
        List.iter Channel.close
          ((steering.channel :: steering.retired)
          @ Option.fold ~none:[] ~some:(fun (pending, _) -> [ pending ])
              steering.replacement));
    Option.iter
      (fun (downstream : downstream) -> Channel.close downstream.control)
      forward;
    match (Portable.Atomic.get sink_failure, outcome) with
    | Some error, _ -> Error (`Sink error)
    | None, Ok () -> Ok ()
    | None, Error (`Sink error) -> Error (`Sink error)
    | None, Error (`Collector error) -> Error (`Collector error)
    | None, Error (`Mask error) -> Error (`Mask error)
end
