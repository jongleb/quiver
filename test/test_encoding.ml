open Sexplib0.Sexp_conv

(** A dump on a path: rendering runs on [renderers], each rung of the ladder
    renders [speeds] raw bytes an interval per renderer and puts [ratios]
    thousandths of them on the wire, each slower and more compact than the one
    before, the network carries [network] wire bytes and the source reads
    [source] raw bytes an interval. *)
type path = {
  speeds : int array;
  ratios : int array;
  renderers : int;
  network : int;
  source : int;
}
[@@deriving sexp_of]

let path =
  QCheck2.Gen.(
    let* rungs = int_range 2 5 in
    let+ fastest = int_range 1_000 1_000_000
    and+ slowdowns = list_size (return (rungs - 1)) (int_range 50 90)
    and+ squeezes = list_size (return (rungs - 1)) (int_range 30 95)
    and+ renderers = int_range 1 8
    and+ network = int_range 1_000 10_000_000
    and+ source = int_range 1_000 10_000_000 in
    let ladder first steps =
      Array.of_list
        (List.rev
           (snd
              (List.fold_left
                 (fun (latest, rungs) step ->
                   let next = Int.max 1 (latest * step / 100) in
                   (next, next :: rungs))
                 (first, [ first ]) steps)))
    in
    {
      speeds = ladder fastest slowdowns;
      ratios = ladder 1000 squeezes;
      renderers;
      network;
      source;
    })

let rate path rung =
  Int.min path.source
    (Int.min
       (path.renderers * path.speeds.(rung))
       (path.network * 1000 / path.ratios.(rung)))

let sample path rung noise =
  let delivered = rate path rung + (rate path rung * noise / 1000)
  and rendered = path.renderers * path.speeds.(rung)
  and carried = path.network * 1000 / path.ratios.(rung) in
  {
    Quiver_net.Encoding.delivered;
    waiting =
      (match carried < Int.min path.source rendered with true -> 1. | false -> 0.);
    idle =
      (match rendered < Int.min path.source carried with true -> 1. | false -> 0.);
  }

let run path ~at noises =
  let rec go t noises rungs =
    match noises with
    | [] -> List.rev rungs
    | noise :: rest ->
        let rung = Quiver_net.Encoding.rung t in
        go (Quiver_net.Encoding.step t (sample path rung noise)) rest (rung :: rungs)
  in
  go
    (Quiver_net.Encoding.start ~rungs:(Array.length path.speeds) ~at)
    noises []

let scenario =
  QCheck2.Gen.(
    let* path = path in
    let+ at = int_range 0 (Array.length path.speeds - 1)
    and+ noises = list_size (return 80) (int_range (-40) 40) in
    (path, at, noises))

let print (path, at, _) =
  Sexplib0.Sexp.to_string_hum ([%sexp_of: path * int] (path, at))

let tests =
  List.map QCheck_alcotest.to_alcotest
    [
      QCheck2.Test.make ~count:1000 ~name:"the rung stays on the ladder" ~print
        scenario (fun (path, at, noises) ->
          List.for_all
            (fun rung -> rung >= 0 && rung < Array.length path.speeds)
            (run path ~at noises));
      QCheck2.Test.make ~count:1000
        ~name:"nothing moves while the source holds the dump back" ~print
        scenario (fun (path, at, noises) ->
          let slow =
            {
              path with
              source =
                Array.fold_left Int.min max_int
                  (Array.mapi (fun rung _ -> rate path rung) path.speeds)
                / 2
                |> Int.max 1;
            }
          in
          List.for_all (fun rung -> rung = at) (run slow ~at noises));
      QCheck2.Test.make ~count:1000
        ~name:
          "most of the last half minute goes to rungs within a fifth of the \
           fastest, or whose next rung toward the bottleneck gains less than \
           the noise hides"
        ~print scenario (fun (path, at, noises) ->
          let best =
            Array.fold_left Int.max 0
              (Array.mapi (fun rung _ -> rate path rung) path.speeds)
          in
          let late =
            List.filteri (fun index _ -> index >= 50) (run path ~at noises)
          in
          let settled rung =
            let rendered = path.renderers * path.speeds.(rung)
            and carried = path.network * 1000 / path.ratios.(rung) in
            let next =
              if carried < Int.min path.source rendered && rung + 1 < Array.length path.speeds then
                Some (rung + 1)
              else if rendered < Int.min path.source carried && rung > 0 then Some (rung - 1)
              else None
            in
            Option.fold ~none:true
              ~some:(fun next -> rate path next * 100 < rate path rung * 115)
              next
          in
          let good =
            Base.List.count late ~f:(fun rung -> rate path rung * 5 >= best * 4 || settled rung)
          in
          good * 10 >= List.length late * 8);
    ]
