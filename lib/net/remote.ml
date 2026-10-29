open Base.Result.Let_syntax

type error =
  | Lost of Channel.error
  | Damaged of Frame.error
  | Refused of { request : string; reason : string }
  | Rejected of string
  | Unexpected_reply of string
  | Codec of string @@ aliased
  | Manifest of Quiver_core.Manifest.error
  | Unstarted of Quiver_core.Concurrent.error
  | Stalled
[@@deriving sexp_of ~portable]

type compression = Fixed of Quiver_sink.Compression.t | Adaptive
[@@deriving sexp_of ~portable]

type settings = {
  streams : int;
  most_streams : int;
  chunks : int;
  compression : compression;
  pace : int option;
  traffic : Quiver_core.Tcp.Class.t option;
  reconnect : Core.Time_ns.Span.t;
  stall : Core.Time_ns.Span.t;
}

(** A frame in the first [length] bytes of a framing buffer, which the sender
    gives back to the pool once acknowledged; [raw] bytes of batch before
    encoding. *)
type rendered = {
  frame : Bytes.t @@ aliased many;
  length : int;
  raw : int;
  encoding : Quiver_core.Manifest.encoding;
}

(** A frame to send: the first [length] bytes of a framing buffer. *)
type framed = { text : string; length : int; raw : int }

(** What a data connection's sender receives. *)
type outgoing = Frame of framed | Stop

(** Whether a data connection's sender found its window full since the last
    ack. *)
type pressure = Pressed | Free

(** Whether a data connection still carries frames. *)
type link =
  | Alive
  | Retiring  (** Told to stop: its end is not a break to replace. *)
  | Broken

type sent = {
  unacknowledged : framed Queue.t;  (** Written, oldest first. *)
  mutable acknowledged : int;  (** Frames the collector has, from the first. *)
  mutable link : link;
  mutable waiting : Quiver_core.Stopwatch.t;
      (** Since the oldest unacknowledged frame waits: its writing or the last
          ack, whichever came later. *)
  mutable allowed : int;  (** Frames the window holds now. *)
  mutable unacknowledged_bytes : int;
  mutable pressure : pressure;
}
(** What one data connection carried. *)

type line = {
  channel : Channel.t;
  sent : sent Await.Capsule.Await.With_mutex.t;
  window : Await.Await.Semaphore.t;
      (** Frames that may still be written unacknowledged: bounds the frames
          kept for sending again, and makes a slow path hold rendering back. *)
  written : unit Await.Await.Ivar.t;  (** Filled when the sender ended. *)
}
(** One data connection. *)

(** Whether connection failures still matter. *)
type phase = Running | Stopping

type senders = { mutable started : unit Await.Await.Ivar.t list }

type lines = { mutable lines : line list }

type tally = {
  bytes_sent : int Portable.Atomic.t;
  bytes_resent : int Portable.Atomic.t;
  busy : int Portable.Atomic.t;  (** Microseconds, Linux. *)
  window_full : int Portable.Atomic.t;  (** Microseconds, Linux. *)
  buffer_full : int Portable.Atomic.t;  (** Microseconds, Linux. *)
  peer_windows : int Portable.Atomic.t;  (** Bytes, summed, macOS. *)
  counted : int Portable.Atomic.t;  (** Connections that told. *)
}
(** What the kernel counted for the data connections that ended. *)
(** The data connections that may still carry frames. *)
(** Filled when each data sender ended, with a TLS [close_notify]: [shutdown]
    waits for them, and for the control writer. *)

(** What the control writer does next. *)
type outbound =
  | Send of Message.control  (** Counted, kept until the collector heard it. *)
  | Beat
  | Broke of { generation : int; error : error }
      (** The reader of that control connection lost it. *)
  | Leave  (** The last: the session ends. *)

(** Whether a control connection may still be used. *)
type held = Open of Channel.t | Gone

type steering = {
  mutable held : held;  (** The control connection now. *)
  unheard : Message.control Queue.t;
      (** Counted messages sent and not known to have arrived, oldest first. *)
  mutable first : int;  (** The number of the oldest of them. *)
  mutable trying : (Channel.t * Quiver_core.Stopwatch.t) option;
      (** A connection that may replace it, since when: ended when it takes
          longer than the session's [reconnect]. *)
}
(** The control connection, which a new one replaces when it breaks. *)

(** The control connection the writer sends on. *)
type control =
  | Connected of {
      channel : Channel.t;
      generation : int;  (** Control connections before it. *)
      read : unit Await.Await.Ivar.t;  (** Filled when its reader ended. *)
    }
  | Disconnected

(** What waits for the collector's answers. *)
type pending = {
  requests : (string * (unit, error) result Await.Await.Ivar.t) Queue.t;
      (** Requests sent and not answered, in order. *)
  waiting : (int, (Quiver_core.Manifest.part, error) result -> unit) Hashtbl.t;
      (** Chunks ended and not answered. *)
}

type t = {
  steering : steering Await.Capsule.Await.With_mutex.t;
  carried :
    (Quiver_core.Schema.Name.t * int, Quiver_core.Manifest.carrying) Hashtbl.t
    Await.Capsule.Await.With_mutex.t;
      (** How each committed part travelled, by table and part number. *)
  protection : Protection.t;
  peer : Channel.peer;
  session : string;
  pacer : Pacer.t option;  (** Shared by every data connection. *)
  traffic : Quiver_core.Tcp.Class.t option;
  format : Quiver_core.Output_format.t;
  columns : Message.columns;
  ladder : Quiver_sink.Compression.t list;
      (** Encodings of batches, from the cheapest to the most compact. *)
  rung : int Portable.Atomic.t;  (** The encoding batches get now. *)
  path : Quiver_core.Tcp.path option;  (** Of the first control connection. *)
  reconnect : Core.Time_ns.Span.t;
      (** How long replacing a broken control connection may take. *)
  lines : lines Await.Capsule.Await.With_mutex.t;
  tally : tally;
  began : Quiver_core.Stopwatch.t;
  outbox : outbound Await.Await.Mpmc_queue.t;  (** To the control writer. *)
  replies : int Portable.Atomic.t;  (** Counted replies that arrived. *)
  silent : int Portable.Atomic.t;
      (** Seconds the control connection brought nothing. *)
  window : Await.Await.Semaphore.t;  (** Requests that may be sent unanswered. *)
  frames : outgoing Await.Await.Mpmc_queue.t;
  room : Await.Await.Semaphore.t;  (** Frames that may still be queued. *)
  slots : Await.Await.Semaphore.t;  (** Chunks that may begin. *)
  pending : pending Await.Capsule.Await.With_mutex.t;
  failure : error option Portable.Atomic.t;
  scratch : Quiver_core.Buffer_pool.t;  (** Encoding buffers. *)
  framing : Quiver_core.Buffer_pool.t;
      (** Frame buffers, kept until acknowledged, so sized by what they hold. *)
  phase : phase Portable.Atomic.t;
  next : int Portable.Atomic.t;  (** The next chunk's number. *)
  alive : int Portable.Atomic.t;
      (** Data connections not broken or being replaced. *)
  delivered : int Portable.Atomic.t;
      (** Bytes the collector acknowledged since the tuner last looked. *)
  delivered_raw : int Portable.Atomic.t;
      (** The same frames' bytes of batch before encoding. *)
  paced : int Portable.Atomic.t;
      (** Nanoseconds data connections waited for the pacer since the tuner
          last looked. *)
  starved : int Portable.Atomic.t;
      (** Nanoseconds data connections waited for frames since the tuner last
          looked. *)
  waited : int Portable.Atomic.t;
      (** Nanoseconds rendering waited for room since the tuner last looked. *)
  unacknowledged_wait : int Portable.Atomic.t;
      (** Nanoseconds data connections waited for acks to write more, in all. *)
  control_written : unit Await.Await.Ivar.t;
  senders : senders Await.Capsule.Await.With_mutex.t;
}

type renderer = {
  scratch : Quiver_core.Buffer_pool.t;
  framing : Quiver_core.Buffer_pool.t;
  ladder : Quiver_sink.Compression.t list;
  rung : int Portable.Atomic.t;
  chunk : int;
}

type part = {
  owner : t;
  chunk : int;
  table : Quiver_core.Schema.Name.t;
  index : int;
  started : Quiver_core.Stopwatch.t;
  mutable frames : int;
  mutable wire_bytes : int;
  mutable raw_bytes : int;
  mutable encodings : (Quiver_core.Manifest.encoding * int) list;
}

let failed result =
  Result.map_error
    (function `Channel error -> Lost error | `Frame error -> Damaged error)
    result

let unexpected reply =
  Error
    (Unexpected_reply (Sexplib0.Sexp.to_string (Message.sexp_of_reply reply)))

let fail t error =
  match
    ( Portable.Atomic.get t.phase,
      Portable.Atomic.compare_and_set t.failure ~if_phys_equal_to:None
        ~replace_with:(Some error) )
  with
  | Stopping, _ | Running, Compare_failed -> ()
  | Running, Set_here ->
      let window = t.window in
      Quiver_core.Guarded.hold t.pending (fun { requests; waiting } ->
          Hashtbl.iter (fun _ answer -> answer (Error error)) waiting;
          Hashtbl.reset waiting;
          Queue.iter
            (fun (_, answer) ->
              Await.Await.Semaphore.release window;
              Await.Await.Ivar.fill_if_empty answer (Error error))
            requests;
          Queue.clear requests)

let give (t : t) { text; length = _; raw = _ } =
  (* Acknowledged or dropped: nothing else holds the frame. *)
  Quiver_core.Buffer_pool.give t.framing
    { bs = Bytes.unsafe_of_string text; len = 0 }

let hear steering messages =
  while steering.first < messages && not (Queue.is_empty steering.unheard) do
    ignore (Queue.pop steering.unheard : Message.control);
    steering.first <- steering.first + 1
  done

let launch (t : t) work ~unstarted =
  match Quiver_core.Concurrent.start work with
  | Ok (_ : unit Quiver_core.Concurrent.task) -> ()
  | Error (`Concurrent error) ->
      fail t (Unstarted error);
      unstarted ()

let lose (t : t) error =
  match Portable.Atomic.fetch_and_add t.alive (-1) with
  | 1 -> fail t error
  | _ -> ()

let attach (t : t) =
  let%bind channel = failed (Channel.connect t.protection t.peer) in
  Option.iter (Channel.mark channel) t.traffic;
  match
    failed
      (Channel.send channel (Message.sexp_of_attach (Data { session = t.session })))
  with
  | Ok () -> Ok channel
  | Error error ->
      Channel.close channel;
      Error error

let rec retire alive =
  match (Portable.Atomic.get alive : int) with
  | 0 | 1 -> `Last
  | count -> (
      match
        Portable.Atomic.compare_and_set alive ~if_phys_equal_to:count
          ~replace_with:(count - 1)
      with
      | Set_here -> `Retired
      | Compare_failed -> retire alive)

let rec break (t : t) line error =
  match
    Quiver_core.Guarded.hold line.sent (fun sent ->
        let frames = List.of_seq (Queue.to_seq sent.unacknowledged) in
        match sent.link with
        | Broken -> None
        | (Alive | Retiring) as link ->
            sent.link <- Broken;
            Queue.clear sent.unacknowledged;
            Some (link, frames))
  with
  | None -> ()
  | Some (link, frames) -> (
      Channel.abort line.channel;
      Await.Await.Semaphore.release line.window;
      match (Portable.Atomic.get t.phase, link) with
      | Stopping, (Alive | Retiring | Broken) -> List.iter (give t) frames
      | Running, Retiring ->
          List.iter
            (fun frame -> Await.Await.Mpmc_queue.push t.frames (Frame frame))
            frames
      | Running, (Alive | Broken) ->
          List.iter
            (fun frame -> Await.Await.Mpmc_queue.push t.frames (Frame frame))
            frames;
          launch t
            (fun () ->
              let pauses = [ 0.05; 0.2; 0.5; 1.; 2. ] in
              replace t pauses error)
            ~unstarted:(fun () -> lose t error))

and replace (t : t) pauses error =
  match (Portable.Atomic.get t.phase, Portable.Atomic.get t.failure) with
  | Stopping, _ | Running, Some _ -> ()
  | Running, None -> (
      match (attach t, pauses) with
      | Ok channel, _ -> carry t channel
      | Error error, [] -> lose t error
      | Error _, pause :: rest ->
          Unix.sleepf pause;
          replace t rest error)

and carry (t : t) channel =
  let first_window = 128 in
  let line =
    {
      channel;
      sent =
        Await.Capsule.Await.With_mutex.create (fun () ->
            {
              unacknowledged = Queue.create ();
              acknowledged = 0;
              link = Alive;
              waiting = Quiver_core.Stopwatch.start ();
              allowed = first_window;
              unacknowledged_bytes = 0;
              pressure = Free;
            });
      window = Await.Await.Semaphore.create first_window;
      written = Await.Await.Ivar.create ();
    }
  and phase = t.phase in
  match
    Quiver_core.Guarded.hold t.senders (fun senders ->
        match Portable.Atomic.get phase with
        | Stopping -> Stopping
        | Running ->
            senders.started <- line.written :: senders.started;
            Quiver_core.Guarded.hold t.lines (fun lines ->
                lines.lines <- line :: lines.lines);
            Running)
  with
  | Stopping -> Channel.close channel
  | Running ->
      launch t
        (fun () -> send t line)
        ~unstarted:(fun () -> Await.Await.Ivar.fill_if_empty line.written ());
      launch t
        (fun () -> acknowledgements t line)
        ~unstarted:(fun () -> Channel.close channel)

and send (t : t) line =
  let finish () =
    Quiver_core.Guarded.hold line.sent (fun sent ->
        match sent.link with
        | Alive -> sent.link <- Retiring
        | Retiring | Broken -> ());
    Channel.finish line.channel
  in
  let rec go () =
    let outgoing =
      match Await.Await.Mpmc_queue.pop_nonblocking t.frames with
      | This outgoing -> outgoing
      | Null ->
          let started = Quiver_core.Stopwatch.start () in
          let outgoing =
            Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await t.frames
          in
          ignore
            (Portable.Atomic.fetch_and_add t.starved
               (Core.Time_ns.Span.to_int_ns (Quiver_core.Stopwatch.elapsed started)));
          outgoing
    in
    Await.Await.Semaphore.release t.room;
    match outgoing with
    | Stop -> (
        match
          ( Portable.Atomic.get t.phase,
            Quiver_core.Guarded.hold line.sent (fun sent -> sent.link) )
        with
        | _, (Broken | Retiring) -> ()
        | Stopping, Alive -> finish ()
        | Running, Alive -> (
            match retire t.alive with `Retired -> finish () | `Last -> go ()))
    | Frame frame -> (
        match Portable.Atomic.get t.failure with
        | Some _ ->
            give t frame;
            go ()
        | None -> (
            let () =
              match Await.Await.Semaphore.try_acquire line.window with
              | Acquired -> ()
              | Would_block ->
                  Quiver_core.Guarded.hold line.sent (fun sent ->
                      sent.pressure <- Pressed);
                  let started = Quiver_core.Stopwatch.start () in
                  Await.Await.Semaphore.acquire Quiver_core.Blocking.await line.window;
                  ignore
                    (Portable.Atomic.fetch_and_add t.unacknowledged_wait
                       (Core.Time_ns.Span.to_int_ns
                          (Quiver_core.Stopwatch.elapsed started)))
            in
            match
              Quiver_core.Guarded.hold line.sent (fun sent ->
                  match sent.link with
                  | Broken | Retiring -> `Elsewhere
                  | Alive ->
                      if Queue.is_empty sent.unacknowledged then
                        sent.waiting <- Quiver_core.Stopwatch.start ();
                      sent.unacknowledged_bytes <- sent.unacknowledged_bytes + frame.length;
                      Queue.push frame sent.unacknowledged;
                      `Here)
            with
            | `Elsewhere ->
                Await.Await.Semaphore.release line.window;
                Await.Await.Mpmc_queue.push t.frames outgoing
            | `Here -> (
                Option.iter
                  (fun pacer ->
                    ignore
                      (Portable.Atomic.fetch_and_add t.paced
                         (Pacer.wait pacer ~bytes:frame.length)))
                  t.pacer;
                match
                  failed
                    (Channel.write line.channel frame.text ~length:frame.length)
                with
                | Ok () -> go ()
                | Error error -> break t line error)))
  in
  go ();
  Await.Await.Ivar.fill_if_empty line.written ()

and acknowledgements (t : t) line =
  let rec go () =
    match failed (Channel.read line.channel) with
    | Ok (Acknowledged received) -> (
        match
          Quiver_core.Guarded.hold line.sent (fun sent ->
              match (sent.link, received - sent.acknowledged) with
              | Broken, _ -> Some ([], 0)
              | (Alive | Retiring), count
                when count < 0 || count > Queue.length sent.unacknowledged ->
                  None
              | (Alive | Retiring), count ->
                  sent.acknowledged <- received;
                  sent.waiting <- Quiver_core.Stopwatch.start ();
                  let frames =
                    List.init count (fun _ -> Queue.pop sent.unacknowledged)
                  in
                  sent.unacknowledged_bytes <-
                    sent.unacknowledged_bytes
                    - Base.List.sum (module Base.Int) frames ~f:(fun frame -> frame.length);
                  let grown =
                    let most_frames = 4096 and most_bytes = 32 * 1024 * 1024 in
                    match sent.pressure with
                    | Pressed when sent.unacknowledged_bytes < most_bytes ->
                        Int.min count (most_frames - sent.allowed)
                    | Pressed | Free -> 0
                  in
                  sent.allowed <- sent.allowed + grown;
                  sent.pressure <- Free;
                  Some (frames, grown))
        with
        | Some (frames, grown) ->
            Base.Fn.apply_n_times ~n:grown
              (fun () -> Await.Await.Semaphore.release line.window)
              ();
            List.iter
              (fun ({ length; raw; _ } as frame) ->
                ignore (Portable.Atomic.fetch_and_add t.delivered length);
                ignore (Portable.Atomic.fetch_and_add t.delivered_raw raw);
                Await.Await.Semaphore.release line.window;
                give t frame)
              frames;
            go ()
        | None -> break t line (Unexpected_reply "an ack of frames never sent"))
    | Ok (Message _ | Batch _) ->
        break t line (Unexpected_reply "data from the collector")
    | Error error -> break t line error
  in
  go ();
  Await.Await.Ivar.read Quiver_core.Blocking.await line.written;
  Option.iter
    (fun ({ sent; resent; held } : Quiver_core.Tcp.strain) ->
      let add counter amount = ignore (Portable.Atomic.fetch_and_add counter amount) in
      add t.tally.bytes_sent sent;
      add t.tally.bytes_resent resent;
      add t.tally.counted 1;
      match held with
      | Timed { busy; receive_window; send_buffer } ->
          add t.tally.busy (Core.Time_ns.Span.to_int_us busy);
          add t.tally.window_full (Core.Time_ns.Span.to_int_us receive_window);
          add t.tally.buffer_full (Core.Time_ns.Span.to_int_us send_buffer)
      | Peer_window window -> add t.tally.peer_windows window)
    (Channel.strain line.channel);
  Channel.close line.channel

let adjust (t : t) (decision : Quiver_core.Width.decision) =
  match decision with
  | Keep -> ()
  | Open count ->
      List.iter
        (fun (_ : int) ->
          match attach t with
          | Ok channel ->
              Portable.Atomic.incr t.alive;
              carry t channel
          | Error (_ : error) -> ())
        (List.init count Fun.id)
  | Close count ->
      List.iter
        (fun (_ : int) -> Await.Await.Mpmc_queue.push t.frames Stop)
        (List.init count Fun.id)

let create protection peer
    { streams; most_streams; chunks; compression; pace; traffic; reconnect; stall } =
  let connect () = failed (Channel.connect protection peer) in
  Result.map_error
    (fun error -> `Sink error)
    (let session =
       Printf.sprintf "%016Lx"
         (Random.State.bits64 (Random.State.make_self_init ()))
     in
     let (attach @ portable) () =
       let%bind channel = failed (Channel.connect protection peer) in
       Option.iter (Channel.mark channel) traffic;
       match
         failed (Channel.send channel (Message.sexp_of_attach (Data { session })))
       with
       | Ok () -> Ok channel
       | Error error ->
           Channel.close channel;
           Error error
     in
     let%bind control = connect () in
     Option.iter (Channel.mark control) traffic;
     let greeted =
     let%bind () =
       failed
         (Channel.send control
            (Message.sexp_of_control
               (Hello
                  { session; byte_order = Message.byte_order; streams; chunks })))
     in
     let%bind ready =
       failed (Channel.receive control Message.reply_of_sexp)
     in
     let%bind format, columns =
       match ready with
       | Ready { format; columns } -> Ok (format, columns)
       | Refused reason | Failed reason ->
           Error (Refused { request = "hello"; reason })
       | Done | Skipped | Committed _ | Rejected _ | Heard _ -> unexpected ready
     in
     let%map channels = Channel.all streams ~close:Channel.close attach in
     (format, columns, channels)
     in
     Result.iter_error (fun _ -> Channel.close control) greeted;
     let%map format, columns, channels = greeted in
     let control_written = Await.Await.Ivar.create () in
     let t =
       {
         steering =
           Await.Capsule.Await.With_mutex.create (fun () ->
               {
                 held = Open control;
                 unheard = Queue.create ();
                 first = 0;
                 trying = None;
               });
         carried =
           Await.Capsule.Await.With_mutex.create (fun () -> Hashtbl.create 64);
         protection;
         peer;
         session;
         pacer =
           Option.map
             (fun bytes_per_second -> Pacer.create ~bytes_per_second)
             pace;
         traffic;
         format;
         columns;
         ladder =
           (match compression with
           | Fixed fixed -> [ fixed ]
           | Adaptive ->
               (Quiver_sink.Compression.Plain
                 :: List.filter_map
                      (fun level ->
                        Result.to_option
                          (Result.map
                             (fun level -> Quiver_sink.Compression.Zstd level)
                             (Quiver_sink.Compression.Level.of_int level)))
                      [ -5; 1; 3 ]));
         rung =
           Portable.Atomic.make
             (match compression with Fixed _ -> 0 | Adaptive -> 2);
         path = Channel.path control;
         reconnect;
         lines = Await.Capsule.Await.With_mutex.create (fun () -> { lines = [] });
         tally =
           {
             bytes_sent = Portable.Atomic.make 0;
             bytes_resent = Portable.Atomic.make 0;
             busy = Portable.Atomic.make 0;
             window_full = Portable.Atomic.make 0;
             buffer_full = Portable.Atomic.make 0;
             peer_windows = Portable.Atomic.make 0;
             counted = Portable.Atomic.make 0;
           };
         began = Quiver_core.Stopwatch.start ();
         outbox = Await.Await.Mpmc_queue.create ();
         replies = Portable.Atomic.make 0;
         silent = Portable.Atomic.make 0;
         window =
           (let requests_ahead = 1024 in
            Await.Await.Semaphore.create requests_ahead);
         frames = Await.Await.Mpmc_queue.create ();
         room =
           (let frames_per_stream = 4 in
            Await.Await.Semaphore.create (frames_per_stream * streams));
         slots = Await.Await.Semaphore.create chunks;
         pending =
           Await.Capsule.Await.With_mutex.create (fun () ->
               { requests = Queue.create (); waiting = Hashtbl.create (2 * chunks) });
         failure = Portable.Atomic.make None;
         scratch = Quiver_core.Buffer_pool.create ();
         framing = Quiver_core.Buffer_pool.create ~capacity:0 ();
         phase = Portable.Atomic.make Running;
         next = Portable.Atomic.make 0;
         alive = Portable.Atomic.make streams;
         delivered = Portable.Atomic.make 0;
         delivered_raw = Portable.Atomic.make 0;
         starved = Portable.Atomic.make 0;
         paced = Portable.Atomic.make 0;
         waited = Portable.Atomic.make 0;
         unacknowledged_wait = Portable.Atomic.make 0;
         control_written;
         senders =
           Await.Capsule.Await.With_mutex.create (fun () -> { started = [] });
       }
     in
     let rec read channel generation =
       let answered reply =
         match
           Quiver_core.Guarded.hold t.pending (fun { requests; _ } ->
               Queue.take_opt requests)
         with
         | None -> fail t (Unexpected_reply "a reply to no request")
         | Some (request, answer) -> (
             Await.Await.Semaphore.release t.window;
             match (reply : Message.reply) with
             | Done -> Await.Await.Ivar.fill_if_empty answer (Ok ())
             | Refused reason ->
                 Await.Await.Ivar.fill_if_empty answer
                   (Error (Refused { request; reason }));
                 fail t (Refused { request; reason })
             | Skipped ->
                 Await.Await.Ivar.fill_if_empty answer
                   (Error
                      (Option.value (Portable.Atomic.get t.failure)
                         ~default:(Unexpected_reply "skipped, nothing refused")))
             | Ready _ | Committed _ | Rejected _ | Heard _ | Failed _ ->
                 Await.Await.Ivar.fill_if_empty answer
                   (Error (Unexpected_reply "ready twice"));
                 fail t (Unexpected_reply "ready twice"))
       in
       match failed (Channel.receive channel Message.reply_of_sexp) with
       | Error error ->
           Await.Await.Mpmc_queue.push t.outbox (Broke { generation; error })
       | Ok (Failed reason) -> fail t (Refused { request = "the session"; reason })
       | Ok (Heard { messages }) ->
           Portable.Atomic.set t.silent 0;
           Quiver_core.Guarded.hold t.steering (fun steering ->
               hear steering messages);
           read channel generation
       | Ok reply ->
           Portable.Atomic.set t.silent 0;
           Portable.Atomic.incr t.replies;
           (match reply with
           | Committed { chunk; part } ->
               Quiver_core.Guarded.hold t.pending (fun { waiting; _ } ->
                   Option.iter
                     (fun answer ->
                       Hashtbl.remove waiting chunk;
                       answer (Ok part))
                     (Hashtbl.find_opt waiting chunk))
           | Rejected { chunk; reason } ->
               Quiver_core.Guarded.hold t.pending (fun { waiting; _ } ->
                   Option.iter
                     (fun answer ->
                       Hashtbl.remove waiting chunk;
                       answer (Error (Rejected reason)))
                     (Hashtbl.find_opt waiting chunk))
           | Ready _ | Done | Refused _ | Skipped | Heard _ | Failed _ ->
               answered reply);
           read channel generation
     in
     let reading channel generation =
       let ended = Await.Await.Ivar.create () in
       launch t
         (fun () ->
           read channel generation;
           Await.Await.Ivar.fill_if_empty ended ())
         ~unstarted:(fun () -> Await.Await.Ivar.fill_if_empty ended ());
       Connected { channel; generation; read = ended }
     in
     let rec write control =
       let sent control outcome =
         match (outcome, control) with
         | Ok (), _ | Error _, Disconnected -> write control
         | Error error, (Connected _ as control) -> write (resume control error)
       in
       match (Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await t.outbox, control) with
       | Leave, Connected { channel; read; _ } ->
           ignore (Channel.send channel (Message.sexp_of_control Leave));
           Channel.finish channel;
           let rec drained waits =
             match (Await.Await.Ivar.peek read, waits) with
             | This (), _ -> ()
             | Null, 0 -> Channel.abort channel
             | Null, _ ->
                 Unix.sleepf 0.01;
                 drained (waits - 1)
           in
           drained (let reader_seconds = 1. in Float.to_int (reader_seconds /. 0.01));
           Await.Await.Ivar.read Quiver_core.Blocking.await read;
           Quiver_core.Guarded.hold t.steering (fun steering ->
               steering.held <- Gone;
               Channel.close channel)
       | Leave, Disconnected -> ()
       | Send message, _ -> (
           Quiver_core.Guarded.hold t.steering (fun steering ->
               Queue.push message steering.unheard);
           match (control, Portable.Atomic.get t.failure) with
           | Connected { channel; _ }, None ->
               sent control
                 (failed (Channel.send channel (Message.sexp_of_control message)))
           | Connected _, Some _ | Disconnected, (Some _ | None) -> write control)
       | Beat, Connected { channel; _ } ->
           sent control
             (failed
                (Channel.send channel
                   (Message.sexp_of_control
                      (Heartbeat { replies = Portable.Atomic.get t.replies }))))
       | Beat, Disconnected -> write control
       | Broke { generation; error }, (Connected { generation = current; _ } as control)
         when current = generation ->
           write (resume control error)
       | Broke _, (Connected _ | Disconnected) -> write control
     and resume control error =
       let generation =
         match control with
         | Connected { channel; generation; read } ->
             Channel.abort channel;
             Await.Await.Ivar.read Quiver_core.Blocking.await read;
             generation
         | Disconnected -> 0
       in
       let gone () =
         Quiver_core.Guarded.hold t.steering (fun steering ->
             (match steering.held with Open channel -> Channel.close channel | Gone -> ());
             steering.held <- Gone);
         Disconnected
       in
       let replaced () =
         let%bind channel = failed (Channel.connect t.protection t.peer) in
         Option.iter (Channel.mark channel) t.traffic;
         Quiver_core.Guarded.hold t.steering (fun steering ->
             steering.trying <- Some (channel, Quiver_core.Stopwatch.start ()));
         match
           let%bind () =
             failed
               (Channel.send channel
                  (Message.sexp_of_attach
                     (Control
                        { session = t.session; replies = Portable.Atomic.get t.replies })))
           in
           let%bind heard =
             failed (Channel.receive channel Message.reply_of_sexp)
           in
           match heard with
           | Failed reason ->
               Ok (`Refused (Refused { request = "the session"; reason }))
           | Heard { messages } -> (
               match
                 Quiver_core.Guarded.hold t.steering (fun steering ->
                     match
                       messages < steering.first
                       || messages > steering.first + Queue.length steering.unheard
                     with
                     | true -> None
                     | false ->
                         hear steering messages;
                         Some (List.of_seq (Queue.to_seq steering.unheard)))
               with
               | None ->
                   Ok (`Refused (Unexpected_reply "a count of messages never sent"))
               | Some unheard ->
                   let%map () =
                     Base.List.fold_result unheard ~init:() ~f:(fun () message ->
                         failed (Channel.send channel (Message.sexp_of_control message)))
                   in
                   `Resumed)
           | Ready _ | Done | Refused _ | Skipped | Committed _ | Rejected _ ->
               unexpected heard
         with
         | Ok `Resumed ->
             Quiver_core.Guarded.hold t.steering (fun steering ->
                 (match steering.held with Open old -> Channel.close old | Gone -> ());
                 steering.trying <- None;
                 steering.held <- Open channel);
             Portable.Atomic.set t.silent 0;
             Ok (reading channel (generation + 1))
         | Ok (`Refused refusal) ->
             Quiver_core.Guarded.hold t.steering (fun steering ->
                 steering.trying <- None;
                 Channel.close channel);
             fail t refusal;
             Ok (gone ())
         | Error error ->
             Quiver_core.Guarded.hold t.steering (fun steering ->
                 steering.trying <- None;
                 Channel.close channel);
             Error error
       in
       let waited = Quiver_core.Stopwatch.start () in
       let rec attempt pauses =
         match (Portable.Atomic.get t.phase, Portable.Atomic.get t.failure) with
         | Stopping, _ | Running, Some _ -> gone ()
         | Running, None -> (
             match replaced () with
             | Ok control -> control
             | Error (_ : error)
               when Core.Time_ns.Span.( > )
                      (Quiver_core.Stopwatch.elapsed waited)
                      t.reconnect ->
                 fail t error;
                 gone ()
             | Error (_ : error) ->
                 let pause, rest =
                   match pauses with
                   | [] -> (2., [])
                   | pause :: rest -> (pause, rest)
                 in
                 Unix.sleepf pause;
                 attempt rest)
       in
       attempt [ 0.05; 0.2; 0.5; 1. ]
     in
     launch t
       (fun () ->
         let stall_ns = Core.Time_ns.Span.to_int_ns stall in
         let rec watch rested =
           Unix.sleepf (Float.max 0.02 (Core.Time_ns.Span.to_sec stall /. 4.));
           match (Portable.Atomic.get t.phase, Portable.Atomic.get t.failure) with
           | Stopping, _ | Running, Some _ -> ()
           | Running, None ->
               let lines =
                 Quiver_core.Guarded.hold t.lines (fun lines ->
                     lines.lines <-
                       List.filter
                         (fun line ->
                           Quiver_core.Guarded.hold line.sent (fun sent ->
                               match sent.link with
                               | Alive | Retiring -> true
                               | Broken -> false))
                         lines.lines;
                     lines.lines)
               in
               let states =
                 List.mapi
                   (fun at line ->
                     Quiver_core.Guarded.hold line.sent (fun sent ->
                         ( at,
                           Queue.length sent.unacknowledged,
                           Core.Time_ns.Span.to_int_ns
                             (Quiver_core.Stopwatch.elapsed sent.waiting) )))
                   lines
               in
               let stalled =
                 Option.map
                   (fun (at, _, _) -> List.nth lines at)
                   (List.find_opt
                      (fun (at, unacknowledged, waiting) ->
                        unacknowledged > 0 && waiting >= stall_ns
                        && (waiting >= 5 * stall_ns
                           || List.exists
                                (fun (other, others, waited) ->
                                  other <> at && (others = 0 || 8 * waited <= waiting))
                                states))
                      states)
               in
               match stalled with
               | Some line when rested >= stall_ns ->
                   break t line Stalled;
                   watch 0
               | Some _ | None ->
                   watch (rested + Core.Time_ns.Span.to_int_ns stall / 4)
         in
         watch stall_ns)
       ~unstarted:ignore;
     launch t
       (fun () ->
         write (reading control 0);
         Await.Await.Ivar.fill_if_empty control_written ())
       ~unstarted:(fun () -> Await.Await.Ivar.fill_if_empty control_written ());
     launch t
       (fun () ->
         let rec beat () =
           Unix.sleepf 1.;
           match (Portable.Atomic.get t.phase, Portable.Atomic.get t.failure) with
           | Stopping, _ | Running, Some _ -> ()
           | Running, None ->
               (match Portable.Atomic.fetch_and_add t.silent 1 with
               | silent when silent >= 15 ->
                   Portable.Atomic.set t.silent 0;
                   Quiver_core.Guarded.hold t.steering (fun steering ->
                       match (steering.held, Portable.Atomic.get t.phase) with
                       | Open channel, Running -> Channel.abort channel
                       | Open _, Stopping | Gone, (Running | Stopping) -> ())
               | (_ : int) -> ());
               (let reconnect = t.reconnect in
                Quiver_core.Guarded.hold t.steering (fun steering ->
                    Option.iter
                      (fun (channel, started) ->
                        match
                          Core.Time_ns.Span.( > )
                            (Quiver_core.Stopwatch.elapsed started)
                            reconnect
                        with
                        | true -> Channel.abort channel
                        | false -> ())
                      steering.trying));
               Await.Await.Mpmc_queue.push t.outbox Beat;
               beat ()
         in
         beat ())
       ~unstarted:ignore;
     List.iter (carry t) channels;
     let most_streams = Int.max streams most_streams in
     (match (most_streams > streams, compression) with
     | false, Fixed _ -> ()
     | true, (Fixed _ | Adaptive) | false, Adaptive ->
         let interval = 1. in
         let rec tune streams_tuning encoding =
           Unix.sleepf interval;
           match (Portable.Atomic.get t.phase, Portable.Atomic.get t.failure) with
           | Stopping, _ | Running, Some _ -> ()
           | Running, None ->
               let delivered = Portable.Atomic.exchange t.delivered 0
               and delivered_raw = Portable.Atomic.exchange t.delivered_raw 0
               and waiting =
                 Float.min 1.
                   (Float.of_int (Portable.Atomic.exchange t.waited 0)
                   /. (interval *. 1e9))
               and connection_time =
                 interval *. 1e9
                 *. Float.of_int (Int.max 1 (Portable.Atomic.get t.alive))
               in
               let idle =
                 Float.min 1.
                   (Float.of_int (Portable.Atomic.exchange t.starved 0)
                   /. connection_time)
               and paced =
                 Float.of_int (Portable.Atomic.exchange t.paced 0)
                 /. connection_time
               in
               let encoded encoding =
                 let encoding =
                   Encoding.step encoding
                     { delivered = delivered_raw; waiting; idle }
                 in
                 Portable.Atomic.set t.rung (Encoding.rung encoding);
                 encoding
               in
               let streamed streams_tuning =
                 let streams_tuning, decision =
                   Quiver_core.Width.step streams_tuning
                     {
                       delivered;
                       waiting =
                         (let held_by_pacer = 0.5 in
                          match paced >= held_by_pacer with
                          | true -> 0.
                          | false -> waiting);
                     }
                 in
                 adjust t decision;
                 (streams_tuning, decision)
               in
               match
                 (Quiver_core.Width.activity streams_tuning, Encoding.activity encoding)
               with
               | Trying, (Watching | Trying) ->
                   tune (fst (streamed streams_tuning)) encoding
               | Watching, Trying -> tune streams_tuning (encoded encoding)
               | Watching, Watching -> (
                   match streamed streams_tuning with
                   | streams_tuning, Keep -> tune streams_tuning (encoded encoding)
                   | streams_tuning, (Open _ | Close _) -> tune streams_tuning encoding)
         in
         launch t
           (fun () ->
             tune
               (Quiver_core.Width.start ~width:streams ~limit:most_streams)
               (Encoding.start
                  ~rungs:(List.length t.ladder)
                  ~at:(Portable.Atomic.get t.rung)))
           ~unstarted:ignore);
     t)

let queue_frame t outgoing =
  (match Await.Await.Semaphore.try_acquire t.room with
  | Acquired -> ()
  | Would_block ->
      let started = Quiver_core.Stopwatch.start () in
      Await.Await.Semaphore.acquire Quiver_core.Blocking.await t.room;
      ignore
        (Portable.Atomic.fetch_and_add t.waited
           (Core.Time_ns.Span.to_int_ns (Quiver_core.Stopwatch.elapsed started))));
  Await.Await.Mpmc_queue.push t.frames outgoing

let shutdown t =
  Portable.Atomic.set t.phase Stopping;
  let started =
    Quiver_core.Guarded.hold t.senders (fun { started } -> started)
  in
  List.iter (fun (_ : unit Await.Await.Ivar.t) -> queue_frame t Stop) started;
  Await.Await.Mpmc_queue.push t.outbox Leave;
  List.iter
    (fun written -> Await.Await.Ivar.read Quiver_core.Blocking.await written)
    (t.control_written :: started)

let request t message =
  let request =
    match (message : Message.control) with
    | Database { name; _ } -> "database " ^ (name :> string)
    | Table { table; _ } ->
        "table "
        ^ Sexplib0.Sexp.to_string_hum (Quiver_core.Schema.Name.sexp_of_t table.name)
    | Grown { table } ->
        "a part of table "
        ^ Sexplib0.Sexp.to_string_hum (Quiver_core.Schema.Name.sexp_of_t table)
    | Objects { database; _ } -> "objects of " ^ (database :> string)
    | Finish _ -> "finish"
    | Hello _ | Begin _ | End _ | Abort _ | Heartbeat _ | Leave -> "control"
  in
  match Portable.Atomic.get t.failure with
  | Some error -> Error error
  | None ->
      Await.Await.Semaphore.acquire Quiver_core.Blocking.await t.window;
      let answer = Await.Await.Ivar.create ()
      and failure = t.failure
      and outbox = t.outbox in
      (match
         Quiver_core.Guarded.hold t.pending (fun { requests; _ } ->
             match Portable.Atomic.get failure with
             | Some error -> Some error
             | None ->
                 Queue.push (request, answer) requests;
                 Await.Await.Mpmc_queue.push outbox (Send message);
                 None)
       with
      | Some error ->
          Await.Await.Semaphore.release t.window;
          Await.Await.Ivar.fill_if_empty answer (Error error)
      | None -> ());
      Ok answer

let sent t message =
  Result.map_error
    (fun error -> `Sink error)
    (Result.bind (request t message) (fun _ ->
         Option.fold ~none:(Ok ()) ~some:Result.error
           (Portable.Atomic.get t.failure)))

let format t = t.format
let columns t = t.columns
let path t = t.path
let streams t = Portable.Atomic.get t.alive

let advice t =
  let get = Portable.Atomic.get in
  let held : Quiver_core.Advice.held =
    match get t.tally.busy with
    | 0 -> Peer_windows (get t.tally.peer_windows)
    | busy ->
        Timed
          {
            busy_us = busy;
            window_full_us = get t.tally.window_full;
            buffer_full_us = get t.tally.buffer_full;
          }
  in
  Quiver_core.Advice.of_counts
    {
      sent = get t.tally.bytes_sent;
      resent = get t.tally.bytes_resent;
      held;
      connections = get t.tally.counted;
    }
    t.path
    ~seconds:(Core.Time_ns.Span.to_sec (Quiver_core.Stopwatch.elapsed t.began))
    ~streams:(get t.alive)

let waited_for_acks t =
  Core.Time_ns.Span.of_int_ns (Portable.Atomic.get t.unacknowledged_wait)
let database t name ~ddl = sent t (Database { name; ddl })

let table t (table : Quiver_core.Schema.Table.t) ~ddl ~parts ~order =
  sent t (Table { table; ddl; parts; order })

let add_part t (table : Quiver_core.Schema.Table.t) =
  sent t (Grown { table = table.name })

let objects t database definitions = sent t (Objects { database; definitions })

let part t ({ name; _ } : Quiver_core.Schema.Table.t) ~index =
  Await.Await.Semaphore.acquire Quiver_core.Blocking.await t.slots;
  match Portable.Atomic.get t.failure with
  | Some error ->
      Await.Await.Semaphore.release t.slots;
      Error (`Sink error)
  | None ->
      let chunk = Portable.Atomic.fetch_and_add t.next 1 in
      Await.Await.Mpmc_queue.push t.outbox
        (Send (Begin { chunk; table = name; index }));
      Ok
        {
          owner = t;
          chunk;
          table = name;
          index;
          started = Quiver_core.Stopwatch.start ();
          frames = 0;
          wire_bytes = 0;
          raw_bytes = 0;
          encodings = [];
        }

let renderer part =
  {
    scratch = part.owner.scratch;
    framing = part.owner.framing;
    ladder = part.owner.ladder;
    rung = part.owner.rung;
    chunk = part.chunk;
  }

let render (renderer : renderer) (batch : Quiver_core.Batch.sealed @ local) =
  let scratch = renderer.scratch in
  let source = Quiver_core.Buffer_pool.take scratch
  and frame = Quiver_core.Buffer_pool.take renderer.framing in
  Quiver_core.Batch.encode batch (borrow_ source);
  let raw = Quiver_core.Byte_buffer.length (borrow_ source) in
  let plain (frame : Quiver_core.Byte_buffer.t @ local)
      (source : Quiver_core.Byte_buffer.t @ local) =
    Frame.start frame;
    Quiver_core.Byte_buffer.append_buffer frame source;
    Frame.seal frame Batch Plain ~raw ~chunk:renderer.chunk
  in
  let rendered =
    match
      List.nth renderer.ladder
        (Int.min
           (List.length renderer.ladder - 1)
           (Portable.Atomic.get renderer.rung))
    with
    | Plain ->
        plain (borrow_ frame) (borrow_ source);
        Ok Quiver_core.Manifest.Plain
    | Zstd level -> (
        Frame.start (borrow_ frame);
        match
          Quiver_sink.Compression.zstd level (borrow_ source) ~into:(borrow_ frame)
        with
        | Ok () ->
            Ok
              (match
                 Quiver_core.Byte_buffer.length (borrow_ frame) - Frame.header_bytes
                 < raw
               with
              | true ->
                  Frame.seal (borrow_ frame) Batch Zstd ~raw ~chunk:renderer.chunk;
                  Quiver_core.Manifest.Zstd (level :> int)
              | false ->
                  plain (borrow_ frame) (borrow_ source);
                  Quiver_core.Manifest.Plain)
        | Error message -> Error (`Sink (Codec message)))
  in
  Quiver_core.Buffer_pool.give scratch source;
  match rendered with
  | Ok encoding ->
      let { Quiver_core.Byte_buffer.bs; len } = frame in
      Ok { frame = bs; length = len; raw; encoding }
  | Error _ as error ->
      Quiver_core.Buffer_pool.give renderer.framing frame;
      error

let append part { frame; length; raw; encoding } =
  match Portable.Atomic.get part.owner.failure with
  | Some error -> Error (`Sink error)
  | None ->
      Frame.number frame ~seq:part.frames;
      part.frames <- part.frames + 1;
      part.wire_bytes <- part.wire_bytes + length;
      part.raw_bytes <- part.raw_bytes + raw;
      part.encodings <-
        (match
           Base.List.Assoc.find part.encodings
             ~equal:Quiver_core.Manifest.equal_encoding encoding
         with
        | None -> part.encodings @ [ (encoding, 1) ]
        | Some frames ->
            List.map
              (fun (known, count) ->
                match Quiver_core.Manifest.equal_encoding known encoding with
                | true -> (known, frames + 1)
                | false -> (known, count))
              part.encodings);
      (* The frame is numbered and no longer changed until it is written. *)
      queue_frame part.owner
        (Frame { text = Bytes.unsafe_to_string frame; length; raw });
      Ok ()

let close part answer =
  let t = part.owner in
  let slots = t.slots and failure = t.failure and chunk = part.chunk in
  let key = (part.table, part.index)
  and frames = part.frames
  and wire_bytes = part.wire_bytes
  and raw_bytes = part.raw_bytes
  and encodings = part.encodings
  and started = part.started in
  let release outcome =
    Await.Await.Semaphore.release slots;
    Result.iter
      (fun (_ : Quiver_core.Manifest.part) ->
        let carrying =
          {
            Quiver_core.Manifest.frames;
            wire_bytes;
            raw_bytes;
            encodings;
            streams = Portable.Atomic.get t.alive;
            round_trip =
              Option.map
                (fun ({ rtt; _ } : Quiver_core.Tcp.path) -> rtt)
                (Quiver_core.Guarded.hold t.steering (fun steering ->
                     match steering.held with
                     | Open channel -> Channel.path channel
                     | Gone -> None));
            sending = Quiver_core.Stopwatch.elapsed started;
          }
        in
        Quiver_core.Guarded.hold t.carried (fun carried ->
            Hashtbl.replace carried key carrying))
      outcome;
    answer outcome
  in
  match
    Quiver_core.Guarded.hold t.pending (fun { waiting; _ } ->
        match Portable.Atomic.get failure with
        | Some error -> Some error
        | None ->
            Hashtbl.replace waiting chunk release;
            None)
  with
  | Some error -> release (Error error)
  | None ->
      Await.Await.Mpmc_queue.push t.outbox
        (Send (End { chunk = part.chunk; frames = part.frames }))

let discard part =
  Await.Await.Mpmc_queue.push part.owner.outbox
    (Send (Abort { chunk = part.chunk }));
  Await.Await.Semaphore.release part.owner.slots

let finish t ~source ~tables ~manifest =
  Result.map_error
    (fun error -> `Sink error)
    (let%bind manifest =
       match
         Quiver_core.Guarded.hold t.carried (fun carried -> Hashtbl.length carried)
       with
       | 0 -> Ok manifest
       | _ ->
           Result.map_error
             (fun (`Manifest error) -> Manifest error)
             (Quiver_core.Manifest.annotate manifest (fun table index ->
                  Quiver_core.Guarded.hold t.carried (fun carried ->
                      Hashtbl.find_opt carried (table, index))))
     in
     Result.bind
       (request t
          (Finish { source; tables; manifest = Yojson.Safe.to_string manifest }))
       (fun answer -> Await.Await.Ivar.read Quiver_core.Blocking.await answer))
