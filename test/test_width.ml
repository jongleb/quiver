open Sexplib0.Sexp_conv

(** A path: each connection carries at most [flow] bytes an interval, all of
    them [capacity], and the dump makes [demand]. *)
type path = { flow : int; capacity : int; demand : int } [@@deriving sexp_of]

let path =
  QCheck2.Gen.(
    let+ flow = int_range 1_000 1_000_000
    and+ capacity = int_range 1_000 100_000_000
    and+ demand = int_range 1_000 100_000_000 in
    { flow; capacity; demand })

(** What the path delivers with so many connections, and whether the dump
    waited for it; [noise] in thousandths. *)
let carried { flow; capacity; demand } streams noise =
  let rate = Int.min demand (Int.min capacity (streams * flow)) in
  let delivered = rate + (rate * noise / 1000) in
  {
    Quiver_core.Width.delivered;
    waiting = (match rate < demand with true -> 1. | false -> 0.);
  }

(** Runs the controller on the path for so many intervals: the connections
    after each, and every decision. *)
let run path ~streams ~limit noises =
  let rec go t noises trace =
    match noises with
    | [] -> List.rev trace
    | noise :: rest ->
        let sample = carried path (Quiver_core.Width.width t) noise in
        let next, decision = Quiver_core.Width.step t sample in
        go next rest ((Quiver_core.Width.width t, decision, next) :: trace)
  in
  go (Quiver_core.Width.start ~width:streams ~limit) noises []

let scenario =
  QCheck2.Gen.(
    let+ path = path
    and+ streams = int_range 1 16
    and+ limit = int_range 1 64
    and+ noises = list_size (return 80) (int_range (-40) 40) in
    (path, streams, limit, noises))

let print (path, streams, limit, _) =
  Sexplib0.Sexp.to_string_hum ([%sexp_of: path * int * int] (path, streams, limit))

let tests =
  List.map QCheck_alcotest.to_alcotest
    [
      QCheck2.Test.make ~count:500
        ~name:
          "connections stay within the limit and every decision moves them by \
           what it says"
        ~print scenario (fun (path, streams, limit, noises) ->
          List.for_all
            (fun (before, decision, next) ->
              let after = Quiver_core.Width.width next in
              after >= 1
              && after <= Int.max 1 limit
              &&
              match (decision : Quiver_core.Width.decision) with
              | Keep -> after = before
              | Open n -> n > 0 && after = before + n
              | Close n -> n > 0 && after = before - n)
            (run path ~streams ~limit noises));
      QCheck2.Test.make ~count:500
        ~name:"nothing opens while the dump does not wait for the network"
        ~print scenario (fun (path, streams, limit, noises) ->
          let roomy = { path with capacity = path.demand; flow = path.demand } in
          List.for_all
            (fun (_, decision, _) ->
              match (decision : Quiver_core.Width.decision) with
              | Keep -> true
              | Open _ | Close _ -> false)
            (run roomy ~streams ~limit noises));
      QCheck2.Test.make ~count:500
        ~name:
          "within a minute the connections carry at least four fifths of what \
           the path and the limit allow"
        ~print scenario (fun (path, streams, limit, noises) ->
          let rate streams =
            Int.min path.demand (Int.min path.capacity (streams * path.flow))
          in
          let _, _, last = List.nth (run path ~streams ~limit noises) 59 in
          rate (Quiver_core.Width.width last) * 5 >= rate limit * 4);
      QCheck2.Test.make ~count:500
        ~name:"on a path that more connections do not widen, every trial closes"
        ~print scenario (fun (path, streams, limit, noises) ->
          let narrow =
            {
              path with
              capacity = Int.min path.capacity (Int.min streams limit * path.flow);
            }
          in
          let decisions =
            Array.of_list
              (List.map (fun (_, decision, _) -> decision)
                 (run narrow ~streams ~limit noises))
          in
          let closes_within opened from =
            let trial = 4 in
            List.exists
              (fun at ->
                at >= Array.length decisions
                ||
                match (decisions.(at) : Quiver_core.Width.decision) with
                | Close closed -> closed = opened
                | Keep | Open _ -> false)
              (List.init trial (fun step -> from + 1 + step))
          in
          Array.for_all Fun.id
            (Array.mapi
               (fun at (decision : Quiver_core.Width.decision) ->
                 match decision with
                 | Open opened -> closes_within opened at
                 | Keep | Close _ -> true)
               decisions));
    ]
