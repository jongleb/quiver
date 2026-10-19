type sample = { delivered : int; waiting : float }
type decision = Keep | Open of int | Close of int [@@deriving sexp_of ~portable]

(** What the next decision waits for. *)
type phase =
  | Watching of { quiet : int }  (** Intervals before a trial may start. *)
  | Trying of {
      before : int;  (** Connections open before the trial. *)
      rate : float;  (** Bytes per interval before the trial. *)
      warming : int;  (** Intervals left before the trial is measured. *)
      measured : int list;  (** Bytes of the trial's intervals so far. *)
    }

type t = {
  streams : int;
  limit : int;
  growth : float;  (** How many times the connections a trial asks for. *)
  recent : int list;  (** Bytes of the last intervals, latest first. *)
  phase : phase;
}

let start ~streams ~limit =
  {
    streams = Int.max 1 (Int.min streams limit);
    limit = Int.max 1 limit;
    growth = 2.;
    recent = [];
    phase = Watching { quiet = 0 };
  }

let streams t = t.streams

type activity = Watching | Trying

let activity t =
  match t.phase with
  | Watching { quiet = _ } -> (Watching : activity)
  | Trying { warming = _; _ } -> Trying

let average = function
  | [] -> 0.
  | bytes -> Float.of_int (Base.List.sum (module Base.Int) bytes ~f:Fun.id) /. Float.of_int (List.length bytes)

let step t { delivered; waiting } =
  let held_back = 0.1 and paying = 1.1 and measured_intervals = 2 in
  let recent =
    match t.recent with
    | latest :: _ -> [ delivered; latest ]
    | [] -> [ delivered ]
  in
  match t.phase with
  | Watching { quiet } when quiet > 0 ->
      ({ t with recent; phase = Watching { quiet = quiet - 1 } }, Keep)
  | Watching { quiet = _ } -> (
      match
        ( waiting >= held_back,
          t.streams < t.limit,
          List.length recent = 2 && List.for_all (fun bytes -> bytes > 0) recent )
      with
      | true, true, true ->
          let wanted =
            Int.min t.limit
              (Int.max (t.streams + 1)
                 (Float.to_int (Float.of_int t.streams *. t.growth)))
          in
          ( {
              t with
              streams = wanted;
              recent;
              phase =
                Trying
                  {
                    before = t.streams;
                    rate = average recent;
                    warming = 1;
                    measured = [];
                  };
            },
            Open (wanted - t.streams) )
      | _ -> ({ t with recent; phase = Watching { quiet = 0 } }, Keep))
  | Trying ({ warming; _ } as trial) when warming > 0 ->
      ({ t with recent; phase = Trying { trial with warming = warming - 1 } }, Keep)
  | Trying ({ measured; _ } as trial)
    when List.length measured + 1 < measured_intervals ->
      ( { t with recent; phase = Trying { trial with measured = delivered :: measured } },
        Keep )
  | Trying { before; rate; measured; warming = _ } -> (
      match average (delivered :: measured) >= rate *. paying with
      | true -> ({ t with recent; phase = Watching { quiet = 0 } }, Keep)
      | false ->
          let settle = 30 in
          ( {
              t with
              streams = before;
              growth = 1.25;
              recent;
              phase = Watching { quiet = settle };
            },
            Close (t.streams - before) ))
