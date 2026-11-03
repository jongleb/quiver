open Base.Result.Let_syntax

type error =
  | Unheld of string
  | Region_kept_failing of Kv.region_error
  | Still_locked of Kv.lock
  | Key of Kv.key_error
  | Disordered of string
[@@deriving sexp_of ~portable]

type freshness = Known | Afresh

type 'e store = {
  locate : freshness -> string -> (Region.t, 'e) result;
  scan : Region.t -> start:string -> stop:Kv.bound -> (Kv.scanned, 'e) result;
  pause : Core.Time_ns.Span.t -> unit;
}

let attempts = 20

let range store ~start ~stop ~limit ~f =
  let before key (bound : Kv.bound) = match bound with Endless -> true | Before stop -> String.compare key stop < 0 in
  let wait tries ~at_least =
    store.pause
      (Core.Time_ns.Span.max at_least (Core.Time_ns.Span.of_int_ms (Int.min 1000 (2 lsl Int.min tries 20))))
  in
  let rec go key freshness tries =
    if not (before key stop) then Ok ()
    else
      let%bind region = store.locate freshness key in
      match Region.find [ region ] key with
      | None -> Error (`Read (Unheld key))
      | Some region -> (
          let upper : Kv.bound =
            match (stop, region.stop) with
            | Endless, bound | bound, Endless -> bound
            | Before a, Before b -> Before (Base.String.min a b)
          in
          let%bind scanned = store.scan region ~start:key ~stop:upper in
          match scanned with
          | Pairs pairs -> (
              let rec ordered previous = function
                | [] -> Ok ()
                | (pair : Kv.pair) :: rest ->
                    if String.compare pair.key previous < 0 || not (before pair.key upper) then
                      Error (`Read (Disordered pair.key))
                    else ordered (pair.key ^ "\x00") rest
              in
              let%bind () = ordered key pairs in
              let%bind () = match pairs with [] -> Ok () | _ :: _ -> f pairs in
              let past () = match upper with Endless -> Ok () | Before next -> go next Known 0 in
              if List.length pairs < limit then past ()
              else
                match Base.List.last pairs with
                | Some last -> go (last.key ^ "\x00") Known 0
                | None -> past ())
          | Region_error error ->
              if tries >= attempts then Error (`Read (Region_kept_failing error))
              else
                let at_least =
                  match error with
                  | Server_is_busy { backoff_ms; _ } -> Core.Time_ns.Span.of_int_ms (Int64.to_int backoff_ms)
                  | Not_leader _ | Epoch_not_match | Region_not_found | Other _ -> Core.Time_ns.Span.zero
                in
                wait tries ~at_least;
                go key Afresh (tries + 1)
          | Key_error (Locked lock) ->
              if tries >= attempts then Error (`Read (Still_locked lock))
              else (
                wait tries ~at_least:Core.Time_ns.Span.zero;
                go key Known (tries + 1))
          | Key_error error -> Error (`Read (Key error)))
  in
  go start Known 0

let table link name ~version ~timeout ~limit ~f =
  let%bind table, regions = Region.of_table link name in
  let first, past = Codec.records table in
  let known = ref regions and stores = Stores.create ~timeout in
  let store =
    {
      locate =
        (fun freshness key ->
          let%bind () =
            match freshness with
            | Known -> Ok ()
            | Afresh -> Result.map (fun (_, regions) -> known := regions) (Region.of_table link name)
          in
          Option.to_result ~none:(`Read (Unheld key)) (Region.find !known key));
      scan =
        (fun (region : Region.t) ~start ~stop ->
          let%bind connection = Stores.connection stores region.address in
          Kv.scan connection region.context ~direction:Forward ~start ~stop ~version ~limit);
      pause = (fun span -> Unix.sleepf (Core.Time_ns.Span.to_sec span));
    }
  in
  let result = range store ~start:first ~stop:(Before past) ~limit ~f in
  Stores.close stores;
  result
