type t = {
  reading : Core.Time_ns.Span.t;
  waiting : Core.Time_ns.Span.t;
  rendering : Core.Time_ns.Span.t;
  writing : Core.Time_ns.Span.t;
  committing : Core.Time_ns.Span.t;
}

type stage = Source | Rendering | Writing

let of_states states =
  let zero = Core.Time_ns.Span.zero and add = Core.Time_ns.Span.( + ) in
  List.fold_left
    (fun spent (state : Manifest.state) ->
      match state with
      | Pending -> spent
      | Done { reading; writing; _ } ->
          {
            reading =
              add spent.reading Core.Time_ns.Span.(reading.duration - reading.pool_wait);
            waiting = add spent.waiting reading.pool_wait;
            rendering = add spent.rendering writing.rendering;
            writing = add spent.writing writing.appending;
            committing = add spent.committing writing.committing;
          })
    { reading = zero; waiting = zero; rendering = zero; writing = zero; committing = zero }
    states

let holding { reading; waiting; rendering; writing; committing } =
  let seconds = Core.Time_ns.Span.to_sec in
  if seconds waiting < 0.25 *. (seconds reading +. seconds waiting) then Source
  else if seconds rendering >= seconds writing +. seconds committing then Rendering
  else Writing
