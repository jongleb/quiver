open Base.Result.Let_syntax

type error =
  | Lost of Channel.error
  | Damaged of Frame.error
  | Refused of { request : string; reason : string }
  | Rejected of string
  | Unexpected_reply of string
  | Codec of string @@ aliased
  | Unstarted of Quiver_core.Concurrent.error
[@@deriving sexp_of ~portable]

type rendered = { frame : Bytes.t @@ aliased many } [@@unboxed]

(** What a data connection's sender receives. *)
type outgoing = Frame of string | Stop

(** Whether connection failures still matter. *)
type phase = Running | Stopping

(** What waits for the collector's answers. *)
type pending = {
  requests : (string * (unit, error) result Await.Await.Ivar.t) Queue.t;
      (** Requests sent and not answered, in order. *)
  waiting : (int, (Quiver_core.Manifest.part, error) result -> unit) Hashtbl.t;
      (** Chunks ended and not answered. *)
}

type t = {
  format : Quiver_core.Output_format.t;
  level : Quiver_sink.Compression.Level.t;
  streams : int;
  outbox : Message.control option Await.Await.Mpmc_queue.t;
      (** To the control writer; [None] ends it. *)
  window : Await.Await.Semaphore.t;  (** Requests that may be sent unanswered. *)
  frames : outgoing Await.Await.Mpmc_queue.t;
  room : Await.Await.Semaphore.t;  (** Frames that may still be queued. *)
  slots : Await.Await.Semaphore.t;  (** Chunks that may begin. *)
  pending : pending Await.Capsule.Await.With_mutex.t;
  failure : error option Portable.Atomic.t;
  scratch : Quiver_core.Buffer_pool.t;  (** Encoding and framing buffers. *)
  phase : phase Portable.Atomic.t;
  next : int Portable.Atomic.t;  (** The next chunk's number. *)
  written : unit Await.Await.Ivar.t list;
      (** Filled when the control writer and each data sender ended, with a
          TLS [close_notify]: [shutdown] waits for them. *)
}

type renderer = {
  scratch : Quiver_core.Buffer_pool.t;
  level : Quiver_sink.Compression.Level.t;
  chunk : int;
}

type part = { owner : t; chunk : int; mutable frames : int }

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

let create protection peer ~streams ~chunks level =
  let connect () = failed (Channel.connect protection peer) in
  Result.map_error
    (fun error -> `Sink error)
    (let session =
       Printf.sprintf "%016Lx"
         (Random.State.bits64 (Random.State.make_self_init ()))
     in
     let%bind control = connect () in
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
     let%bind format =
       match ready with
       | Ready format -> Ok format
       | Refused reason -> Error (Refused { request = "hello"; reason })
       | Done | Skipped | Committed _ | Rejected _ -> unexpected ready
     in
     let%map channels =
       Channel.all streams ~close:Channel.close (fun () ->
           let%bind channel = failed (Channel.connect protection peer) in
           match
             failed (Channel.send channel (Message.sexp_of_attach { session }))
           with
           | Ok () -> Ok channel
           | Error error ->
               Channel.close channel;
               Error error)
     in
     (format, channels)
     in
     Result.iter_error (fun _ -> Channel.close control) greeted;
     let%map format, channels = greeted in
     let control_written = Await.Await.Ivar.create ()
     and senders =
       List.map (fun channel -> (channel, Await.Await.Ivar.create ())) channels
     in
     let t =
       {
         format;
         level;
         streams;
         outbox = Await.Await.Mpmc_queue.create ();
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
         phase = Portable.Atomic.make Running;
         next = Portable.Atomic.make 0;
         written = control_written :: List.map snd senders;
       }
     in
     let rec write () =
       match Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await t.outbox with
       | None -> Channel.finish control
       | Some message ->
           (match Portable.Atomic.get t.failure with
           | Some _ -> ()
           | None ->
               Result.iter_error (fail t)
                 (failed
                    (Channel.send control (Message.sexp_of_control message))));
           write ()
     in
     let rec read () =
       match failed (Channel.receive control Message.reply_of_sexp) with
       | Error error -> fail t error
       | Ok (Committed { chunk; part }) ->
           Quiver_core.Guarded.hold t.pending (fun { waiting; _ } ->
               Option.iter
                 (fun answer ->
                   Hashtbl.remove waiting chunk;
                   answer (Ok part))
                 (Hashtbl.find_opt waiting chunk));
           read ()
       | Ok (Rejected { chunk; reason }) ->
           Quiver_core.Guarded.hold t.pending (fun { waiting; _ } ->
               Option.iter
                 (fun answer ->
                   Hashtbl.remove waiting chunk;
                   answer (Error (Rejected reason)))
                 (Hashtbl.find_opt waiting chunk));
           read ()
       | Ok ((Ready _ | Done | Refused _ | Skipped) as reply) -> (
           match Quiver_core.Guarded.hold t.pending (fun { requests; _ } -> Queue.take_opt requests) with
           | None -> fail t (Unexpected_reply "a reply to no request")
           | Some (request, answer) ->
               Await.Await.Semaphore.release t.window;
               (match reply with
               | Done -> Await.Await.Ivar.fill_if_empty answer (Ok ())
               | Refused reason ->
                   Await.Await.Ivar.fill_if_empty answer
                     (Error (Refused { request; reason }));
                   fail t (Refused { request; reason })
               | Skipped ->
                   Await.Await.Ivar.fill_if_empty answer
                     (Error
                        (Option.value (Portable.Atomic.get t.failure)
                           ~default:
                             (Unexpected_reply "skipped, nothing refused")))
               | Ready _ | Committed _ | Rejected _ ->
                   Await.Await.Ivar.fill_if_empty answer
                     (Error (Unexpected_reply "ready twice"));
                   fail t (Unexpected_reply "ready twice"));
               read ())
     in
     let (send @ portable) channel written =
       let rec go () =
         let outgoing =
           Await.Await.Mpmc_queue.pop Quiver_core.Blocking.await t.frames
         in
         Await.Await.Semaphore.release t.room;
         match outgoing with
         | Stop -> Channel.finish channel
         | Frame frame ->
             (match Portable.Atomic.get t.failure with
             | Some _ -> ()
             | None ->
                 Result.iter_error (fail t) (failed (Channel.write channel frame)));
             go ()
       in
       go ();
       Await.Await.Ivar.fill_if_empty written ()
     in
     let launch work ~unstarted =
       match Quiver_core.Concurrent.start work with
       | Ok (_ : unit Quiver_core.Concurrent.task) -> ()
       | Error (`Concurrent error) ->
           fail t (Unstarted error);
           unstarted ()
     in
     launch read ~unstarted:ignore;
     launch
       (fun () ->
         write ();
         Await.Await.Ivar.fill_if_empty control_written ())
       ~unstarted:(fun () -> Await.Await.Ivar.fill_if_empty control_written ());
     List.iter
       (fun ((channel : Channel.t), (written : unit Await.Await.Ivar.t)) ->
         launch
           (fun () -> send channel written)
           ~unstarted:(fun () ->
             Channel.close channel;
             Await.Await.Ivar.fill_if_empty written ()))
       senders;
     t)

let queue_frame t outgoing =
  Await.Await.Semaphore.acquire Quiver_core.Blocking.await t.room;
  Await.Await.Mpmc_queue.push t.frames outgoing

let shutdown t =
  Portable.Atomic.set t.phase Stopping;
  for _ = 1 to t.streams do
    queue_frame t Stop
  done;
  Await.Await.Mpmc_queue.push t.outbox None;
  List.iter
    (fun written -> Await.Await.Ivar.read Quiver_core.Blocking.await written)
    t.written

let request t message =
  let request =
    match (message : Message.control) with
    | Database { name; _ } -> "database " ^ (name :> string)
    | Table { table; _ } ->
        "table "
        ^ (fun value ->
            Sexplib0.Sexp.to_string_hum
              (Quiver_core.Schema.Name.sexp_of_t value))
            table.name
    | Objects { database; _ } -> "objects of " ^ (database :> string)
    | Finish _ -> "finish"
    | Hello _ | Begin _ | End _ | Abort _ -> "control"
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
                 Await.Await.Mpmc_queue.push outbox (Some message);
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
let database t name ~ddl = sent t (Database { name; ddl })

let table t (table : Quiver_core.Schema.Table.t) ~ddl ~parts =
  sent t (Table { table; ddl; parts })

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
        (Some (Begin { chunk; table = name; index }));
      Ok { owner = t; chunk; frames = 0 }

let renderer part =
  { scratch = part.owner.scratch; level = part.owner.level; chunk = part.chunk }

let render (renderer : renderer) (batch : Quiver_core.Batch.sealed @ local) =
  let scratch = renderer.scratch in
  let source = Quiver_core.Buffer_pool.take scratch
  and frame = Quiver_core.Buffer_pool.take scratch in
  Quiver_core.Batch.encode batch (borrow_ source);
  Frame.start (borrow_ frame);
  let rendered =
    match
      Quiver_sink.Compression.zstd renderer.level (borrow_ source)
        ~into:(borrow_ frame)
    with
    | Ok () ->
        let raw = Quiver_core.Byte_buffer.length (borrow_ source) in
        (match
           Quiver_core.Byte_buffer.length (borrow_ frame) - Frame.header_bytes < raw
         with
        | true -> Frame.seal (borrow_ frame) Batch Zstd ~raw ~chunk:renderer.chunk
        | false ->
            Frame.start (borrow_ frame);
            Quiver_core.Byte_buffer.append_buffer (borrow_ frame) (borrow_ source);
            Frame.seal (borrow_ frame) Batch Plain ~raw ~chunk:renderer.chunk);
        Ok { frame = Quiver_core.Byte_buffer.to_bytes (borrow_ frame) }
    | Error message -> Error (`Sink (Codec message))
  in
  Quiver_core.Buffer_pool.give scratch source;
  Quiver_core.Buffer_pool.give scratch frame;
  rendered

let append part { frame } =
  match Portable.Atomic.get part.owner.failure with
  | Some error -> Error (`Sink error)
  | None ->
      Frame.number frame ~seq:part.frames;
      part.frames <- part.frames + 1;
      (* The frame is numbered and no longer changed: it is sent as text. *)
      queue_frame part.owner (Frame (Bytes.unsafe_to_string frame));
      Ok ()

let close part answer =
  let t = part.owner in
  let slots = t.slots and failure = t.failure and chunk = part.chunk in
  let release outcome =
    Await.Await.Semaphore.release slots;
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
        (Some (End { chunk = part.chunk; frames = part.frames }))

let discard part =
  Await.Await.Mpmc_queue.push part.owner.outbox
    (Some (Abort { chunk = part.chunk }));
  Await.Await.Semaphore.release part.owner.slots

let finish t ~source ~tables ~manifest =
  Result.map_error
    (fun error -> `Sink error)
    (Result.bind
       (request t
          (Finish { source; tables; manifest = Yojson.Safe.to_string manifest }))
       (fun answer -> Await.Await.Ivar.read Quiver_core.Blocking.await answer))
