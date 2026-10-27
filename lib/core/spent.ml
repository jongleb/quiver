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

type table = {
  parts : int;
  spent : t;
  began : Core.Time_ns.Span.t;
  ended : Core.Time_ns.Span.t;
}

let of_tables entries =
  let read =
    List.filter_map
      (fun ({ table; state; _ } : Manifest.entry) ->
        match state with
        | Pending -> None
        | Done { reading; _ } -> Some (table, (state, reading)))
      entries
  in
  Base.List.Assoc.sort_and_group read ~compare:Schema.Name.compare
  |> List.map (fun (name, parts) ->
         let spans f = List.map (fun (_, (reading : Manifest.reading)) -> f reading) parts in
         ( name,
           {
             parts = List.length parts;
             spent = of_states (List.map fst parts);
             began =
               Option.value ~default:Core.Time_ns.Span.zero
                 (Base.List.min_elt (spans (fun r -> r.started)) ~compare:Core.Time_ns.Span.compare);
             ended =
               Option.value ~default:Core.Time_ns.Span.zero
                 (Base.List.max_elt
                    (spans (fun r -> Core.Time_ns.Span.(r.started + r.duration)))
                    ~compare:Core.Time_ns.Span.compare);
           } ))
  |> List.stable_sort (fun (_, a) (_, b) -> Core.Time_ns.Span.compare b.ended a.ended)
