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
}

type downstream = {
  control : Channel.t;
  data : Channel.t list;
  format : Quiver_core.Output_format.t;
  peer : Channel.peer;  (** Where data connections that replace broken ones go. *)
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

let accept listening protection ~reconnect =
  let%bind control = failed (Channel.accept listening protection) in
  let greeted =
    let%bind hello =
      failed (Channel.receive control Message.control_of_sexp)
    in
    match hello with
    | Hello { byte_order; _ } when byte_order <> Message.byte_order ->
        Error (`Collector (Foreign_byte_order byte_order))
    | Hello { session; streams; chunks; _ } ->
        Ok { control; id = session; streams; chunks; protection; reconnect }
    | Database _ | Table _ | Objects _ | Begin _ | End _ | Abort _ | Finish _
    | Heartbeat _ | Leave ->
        out_of_place "before hello"
  in
  Result.iter_error (fun _ -> Channel.close control) greeted;
  greeted

let chunks (session : session) = session.chunks

(** How accepting one data connection ended. *)
type attachment : value mod contended portable =
  | Attached of Channel.t
  | Broke  (** Before its attach arrived: the agent sends its frames again. *)
  | Failed of error

let attached listening (session : session) =
  let protection = session.protection and id = session.id in
  let tried =
    Quiver_core.Concurrent.each session.streams (fun (_ : int) ->
        match Channel.accept listening protection with
        | Error (`Channel error) -> Failed (Lost error)
        | Ok channel -> (
            match Channel.receive channel Message.attach_of_sexp with
            | Ok (Data { session }) when String.equal session id ->
                Attached channel
            | Ok (Data _ | Control _) ->
                Channel.close channel;
                Failed (Out_of_place "a data connection of another session")
            | Error (`Channel _ | `Frame _) ->
                Channel.close channel;
                Broke))
  in
  let channels =
    List.filter_map
      (function Attached channel -> Some channel | Failed _ | Broke -> None)
      tried
  in
  match
    ( List.find_map
        (function Failed error -> Some error | Attached _ | Broke -> None)
        tried,
      channels )
  with
  | None, _ :: _ -> Ok channels
  | None, [] -> Error (`Collector (Lost Closed))
  | Some error, _ ->
      List.iter Channel.close channels;
      Error (`Collector error)

let forward peer (session : session) =
  let (forwarded : (downstream, [ `Collector of error ]) result) =
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
    | Refused message | Failed message -> Error (`Collector (Downstream message))
    | Done | Skipped | Committed _ | Rejected _ | Heard _ -> unexpected ready
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
                     Channel.send channel
                       (Message.sexp_of_attach (Data { session = id }))
                   with
                   | Ok () -> Ok channel
                   | Error (`Channel error) ->
                       Channel.close channel;
                       Error (Lost error))))
  in
  { control; data; format; peer }
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

let attaching (session : session) channel =
  match Channel.receive channel Message.attach_of_sexp with
  | Ok ((Data { session = id } | Control { session = id; _ }) as attach)
    when String.equal id session.id ->
      Some (channel, attach)
  | Ok (Data _ | Control _) | Error (`Channel _ | `Frame _) ->
      Channel.close channel;
      None

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
  let rec go taken =
    match Channel.accept listening session.protection with
    | Error (`Channel Channel.Closed) -> taken
    | Error (`Channel _) ->
        let pause = 0.01 in
        Unix.sleepf pause;
        go taken
    | Ok channel -> (
        match
          Quiver_core.Concurrent.start (fun () ->
              Option.iter take (attaching session channel))
        with
        | Ok task -> go (task :: taken)
        | Error (`Concurrent _) ->
            Channel.close channel;
            go taken)
  in
  List.iter Quiver_core.Concurrent.join (go [])

(** A control connection of the agent passed on to the next hop. *)
type pair = {
  number : int;  (** Control pairs before it. *)
  ended : (unit, Channel.error) result Await.Await.Ivar.t;
  ends : Channel.t list;  (** Both of its connections. *)
}

type pairs = {
  mutable control : pair;  (** The latest. *)
  mutable opened : Channel.t list;
      (** Every pair's connections, closed only once the relay ends, so ending
          one never reaches a descriptor reused since. *)
}

let relay listening (session : session) (downstream : downstream) =
  match
    let%bind () =
      failed
        (Channel.send session.control
           (Message.sexp_of_reply (Ready downstream.format)))
    in
    attached listening session
  with
  | Error _ as error ->
      List.iter Channel.close
        (session.control :: downstream.control :: downstream.data);
      error
  | Ok channels ->
      let #(used, spare) =
        Base.List.split_n downstream.data (List.length channels)
      in
      List.iter Channel.close spare;
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
                    | Hello _ | Database _ | Table _ | Objects _ | Begin _ | End _
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
            {
              control = first;
              opened = channels @ used @ first.ends;
            })
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
      let tasks =
        controlled
        :: List.map2
             (fun (from : Channel.t) (into : Channel.t) ->
               started (fun () -> ignore (spliced from into)))
             channels used
      in
      let late =
        started (fun () ->
            accepting listening session (fun (channel, attach) ->
                match reaching session downstream attach with
                | None -> Channel.close channel
                | Some next -> (
                    let ends = [ channel; next ] in
                    match attach with
                    | Data _ ->
                        Quiver_core.Guarded.hold pairs (fun pairs ->
                            pairs.opened <- ends @ pairs.opened);
                        ignore (spliced channel next)
                    | Control _ ->
                        let ended = Await.Await.Ivar.create () in
                        Quiver_core.Guarded.hold pairs (fun pairs ->
                            List.iter Channel.abort pairs.control.ends;
                            pairs.control <-
                              { number = pairs.control.number + 1; ended; ends };
                            pairs.opened <- ends @ pairs.opened);
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
      Result.iter_error
        (fun (_ : Channel.error) ->
          Quiver_core.Guarded.hold pairs (fun pairs ->
              List.iter Channel.abort pairs.opened))
        outcome;
      Channel.stop listening;
      List.iter (Option.iter Quiver_core.Concurrent.join) (late :: tasks);
      Quiver_core.Guarded.hold pairs (fun pairs ->
          List.iter Channel.close pairs.opened);
      failed (Result.map_error (fun error -> `Channel error) outcome)

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

  type late = { mutable handles : Channel.handle list }
  (** Data connections that replaced broken ones, to end at a failure. *)

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
  type outgoing = Reply of Message.reply | Beat | Last

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
            (Quiver_core.Guarded.hold passing (fun () ->
                 (Channel.send downstream.control (Message.sexp_of_control message)
                   : (unit, [ `Channel of Channel.error ]) result)))
    in
    match
      let%bind () =
        failed
          (Channel.send session.control
             (Message.sexp_of_reply (Ready (S.format sink))))
      in
      attached listening session
    with
    | Error _ as error ->
        Channel.close session.control;
        Option.iter
          (fun (downstream : downstream) ->
            List.iter Channel.close (downstream.control :: downstream.data))
          forward;
        error
    | Ok channels ->
    let forward =
      Option.map
        (fun (downstream : downstream) ->
          let #(used, spare) =
            Base.List.split_n downstream.data (List.length channels)
          in
          List.iter Channel.close spare;
          { downstream with data = used })
        forward
    in
    let handles =
      List.map Channel.handle
        (Option.fold ~none:[]
              ~some:(fun (downstream : downstream) -> [ downstream.control ])
              forward
        @ channels
        @ Option.fold ~none:[]
            ~some:(fun (downstream : downstream) -> downstream.data)
            forward)
    in
    let late =
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
                (Quiver_core.Guarded.hold late (fun { handles } -> handles));
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
              | Failed error -> Error (`Sink error))
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
            ( Hello _ | Database _ | Table _ | Objects _ | Begin _ | End _
            | Abort _ | Finish _ )
          when stage = `Concluded ->
            out_of_place "a message after finish"
        | Ok message ->
        (match message with
        | Hello _ | Heartbeat _ | Leave -> ()
        | Database _ | Table _ | Objects _ | Begin _ | End _ | Abort _ | Finish _
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
        | Database { name; ddl } ->
            let%bind () = answered (fun () -> S.database sink name ~ddl) in
            next stage channel
        | Table { table; ddl; parts } ->
            let%bind () =
              answered (fun () ->
                  Hashtbl.replace tables table.Quiver_core.Schema.Table.name table;
                  S.table sink table ~ddl ~parts)
            in
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
                    Part.deliver part seq (Part.render part sealed);
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
            Portable.Atomic.set life Over;
            Registry.close lanes;
            Quiver_core.Mailbox.close verdicts;
            Await.Await.Ivar.fill_if_empty finishing None;
            let answer = Await.Await.Ivar.read Quiver_core.Blocking.await answered in
            Await.Await.Mpmc_queue.push outbox Last;
            Option.iter
              (fun (downstream : downstream) -> Channel.finish downstream.control)
              forward;
            report (Result.bind controlled (fun () -> answer)))
          ~unstarted:ignore;
        launch (fun () -> write 0) ~unstarted:ignore;
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
    let replacing =
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
                  Quiver_core.Guarded.hold late (fun late ->
                      late.handles <- held @ late.handles);
                  (match Atomic.get first_failure with
                  | Some _ -> List.iter Channel.sever held
                  | None -> ());
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
    List.iter (Option.iter Quiver_core.Concurrent.join) threads;
    Channel.stop listening;
    Option.iter Quiver_core.Concurrent.join replacing;
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
end
