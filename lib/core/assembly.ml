module Make (S : Sink_intf.S) = struct
  type outcome =
    | Closed of { part : Manifest.part; writing : Manifest.writing }
    | Failed of S.error

  type rendered = {
    result : (S.rendered, S.error) result;
    took : Core.Time_ns.Span.t;
  }

  (** How far a part got. *)
  type stage =
    | Open
    | Sealed of { batches : int; told : outcome -> unit @@ portable }
        (** [told] hears the outcome once batch [batches - 1] is appended. *)
    | Broken of S.error  (** Discarded, not sealed yet. *)
    | Ended

  type state = {
    part : S.part;
    pending : (int, S.rendered or_null Unique.Ref.t * Stopwatch.t) Hashtbl.t;
        (** Each batch leaves its cell once, by exchange; and when it came. *)
    mutable next : int;  (** The batch to append next. *)
    mutable stage : stage;
    mutable rendering : Core.Time_ns.Span.t;
    mutable appending : Core.Time_ns.Span.t;
    mutable reordering : Core.Time_ns.Span.t;
  }

  type t = {
    state : state Await.Capsule.Await.With_mutex.t;
    renderer : S.renderer;
  }

  let create sink table ~index =
    match
      Guarded.create (fun () ->
          match S.part sink table ~index with
          | Ok part ->
              Ok
                {
                  part;
                  pending = Hashtbl.create 8;
                  next = 0;
                  stage = Open;
                  rendering = Core.Time_ns.Span.zero;
                  appending = Core.Time_ns.Span.zero;
                  reordering = Core.Time_ns.Span.zero;
                }
          | Error (`Sink error) -> Error error)
    with
    | Error error -> Error error
    | Ok state ->
        Ok
          {
            state;
            renderer =
              Guarded.hold state (fun { part; _ } -> S.renderer part);
          }

  let render t batch =
    let started = Stopwatch.start () in
    let result =
      match S.render t.renderer batch with
      | Ok rendered -> Ok rendered
      | Error (`Sink error) -> Error error
    in
    { result; took = Stopwatch.elapsed started }

  let finish state =
    match state.stage with
    | Sealed { batches; told } when state.next = batches ->
        state.stage <- Ended;
        let closing = Stopwatch.start ()
        and rendering = state.rendering
        and appending = state.appending
        and reordering = state.reordering in
        S.close state.part (function
          | Ok part ->
              told
                (Closed
                   {
                     part;
                     writing =
                       {
                         rendering;
                         appending;
                         reordering;
                         committing = Stopwatch.elapsed closing;
                       };
                   })
          | Error error -> told (Failed error))
    | Open | Sealed _ | Broken _ | Ended -> ()

  let break state error =
    match state.stage with
    | Open ->
        S.discard state.part;
        state.stage <- Broken error
    | Sealed { told; _ } ->
        S.discard state.part;
        state.stage <- Ended;
        told (Failed error)
    | Broken _ | Ended -> ()

  let deliver t (number : int) result =
    Guarded.hold t.state (fun state ->
        let append rendered =
          let started = Stopwatch.start () in
          let appended = S.append state.part rendered in
          state.appending <-
            Core.Time_ns.Span.( + ) state.appending (Stopwatch.elapsed started);
          match appended with
          | Ok () -> state.next <- state.next + 1
          | Error (`Sink error) -> break state error
        in
        let rec drain () =
          match (state.stage, Hashtbl.find_opt state.pending state.next) with
          | (Open | Sealed _), Some (cell, waiting) -> (
              Hashtbl.remove state.pending state.next;
              state.reordering <-
                Core.Time_ns.Span.( + ) state.reordering
                  (Stopwatch.elapsed waiting);
              match Unique.Ref.exchange cell Null with
              | This rendered ->
                  append rendered;
                  drain ()
              | Null -> ())
          | (Open | Sealed _ | Broken _ | Ended), _ -> ()
        in
        let { result; took } = Handoff.own result in
        state.rendering <- Core.Time_ns.Span.( + ) state.rendering took;
        match (state.stage, result) with
        | (Broken _ | Ended), _ -> ()
        | (Open | Sealed _), (Ok _ | Error _)
          when number < state.next || Hashtbl.mem state.pending number ->
            ()
        | (Open | Sealed _), Error error -> break state error
        | (Open | Sealed _), Ok rendered when number = state.next ->
            append rendered;
            drain ();
            finish state
        | (Open | Sealed _), Ok rendered ->
            Hashtbl.replace state.pending number
              (Unique.Ref.make (This rendered), Stopwatch.start ()))

  let seal t ~batches told =
    Guarded.hold t.state (fun state ->
        match state.stage with
        | Open ->
            state.stage <- Sealed { batches; told };
            finish state
        | Broken error ->
            state.stage <- Ended;
            told (Failed error)
        | Sealed _ | Ended -> ())

  type growth = Grown | Shut | Refused of S.error

  let grow t sink table =
    Guarded.hold t.state (fun state ->
        match state.stage with
        | Open -> (
            match S.add_part sink table with
            | Ok () -> Grown
            | Error (`Sink error) -> Refused error)
        | Sealed _ | Broken _ | Ended -> Shut)

  let abandon t =
    Guarded.hold t.state (fun state ->
        match state.stage with
        | Open | Sealed _ ->
            S.discard state.part;
            state.stage <- Ended
        | Broken _ -> state.stage <- Ended
        | Ended -> ())
end
