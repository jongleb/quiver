type sample = { delivered : int; waiting : float; idle : float }

(** What the next decision waits for. *)
type phase =
  | Watching of { quiet : int }  (** Intervals before a trial may start. *)
  | Trying of {
      from : int;  (** The rung before the trial. *)
      rate : float;  (** Bytes per interval before the trial. *)
      warming : int;  (** Intervals left before the trial is measured. *)
      measured : int list;  (** Bytes of the trial's intervals so far. *)
    }

type t = {
  rungs : int;
  rung : int;
  recent : int list;  (** Bytes of the last intervals, latest first. *)
  failures : int;  (** Trials in a row that did not pay. *)
  phase : phase;
}

let start ~rungs ~at =
  let rungs = Int.max 1 rungs in
  {
    rungs;
    rung = Int.max 0 (Int.min (rungs - 1) at);
    recent = [];
    failures = 0;
    phase = Watching { quiet = 0 };
  }

let rung t = t.rung

type activity = Watching | Trying

let activity t =
  match t.phase with
  | Watching { quiet = _ } -> (Watching : activity)
  | Trying { warming = _; _ } -> Trying

let average = function
  | [] -> 0.
  | bytes ->
      Float.of_int (Base.List.sum (module Base.Int) bytes ~f:Fun.id) /. Float.of_int (List.length bytes)

let step t { delivered; waiting; idle } =
  let held_back = 0.2 and paying = 1.05 and measured_intervals = 2 in
  let recent =
    match t.recent with
    | latest :: _ -> [ delivered; latest ]
    | [] -> [ delivered ]
  in
  match t.phase with
  | Watching { quiet } when quiet > 0 ->
      { t with recent; phase = Watching { quiet = quiet - 1 } }
  | Watching { quiet = _ } -> (
      let toward =
        match (waiting >= held_back, idle >= held_back) with
        | true, false when t.rung + 1 < t.rungs -> Some (t.rung + 1)
        | false, true when t.rung > 0 -> Some (t.rung - 1)
        | (true | false), (true | false) -> None
      in
      match
        ( toward,
          List.length recent = 2 && List.for_all (fun bytes -> bytes > 0) recent )
      with
      | Some rung, true ->
          {
            t with
            rung;
            recent;
            phase =
              Trying
                { from = t.rung; rate = average recent; warming = 1; measured = [] };
          }
      | Some _, false | None, (true | false) ->
          { t with recent; phase = Watching { quiet = 0 } })
  | Trying ({ warming; _ } as trial) when warming > 0 ->
      { t with recent; phase = Trying { trial with warming = warming - 1 } }
  | Trying ({ measured; _ } as trial)
    when List.length measured + 1 < measured_intervals ->
      { t with recent; phase = Trying { trial with measured = delivered :: measured } }
  | Trying { from; rate; measured; warming = _ } -> (
      match average (delivered :: measured) >= rate *. paying with
      | true -> { t with recent; failures = 0; phase = Watching { quiet = 0 } }
      | false ->
          let settle = 10 and longest = 160 in
          {
            t with
            rung = from;
            recent;
            failures = t.failures + 1;
            phase =
              Watching { quiet = Int.min longest (settle lsl Int.min 4 t.failures) };
          })
