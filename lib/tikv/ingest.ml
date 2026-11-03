open Base.Result.Let_syntax

type failure = Write_failed of string | Leaderless | Ingest_failed of Kv.region_error [@@deriving sexp_of ~portable]
type error = Unheld of string | Kept_failing of failure | Refused of Kv.region_error [@@deriving sexp_of ~portable]

type ('sst, 'e) store = {
  locate : Read.freshness -> string -> (Region.t, 'e) result;
  write : Region.t -> Import.job -> ((Region.replica * 'sst Import.written) list, 'e) result;
  ingest : Region.t -> 'sst list -> (Import.ingested, 'e) result;
  uuid : unit -> string;
  pause : Core.Time_ns.Span.t -> unit;
}

let attempts = 20
let patience = Core.Time_ns.Span.of_int_sec 300

let deliver store ~commit_ts ~batch pairs =
  let wait tries ~at_least =
    let span = Core.Time_ns.Span.max at_least (Core.Time_ns.Span.of_int_ms (Int.min 1000 (2 lsl Int.min tries 20))) in
    store.pause span;
    span
  in
  let rec go pairs freshness tries =
    let retry failure tries =
      if tries >= attempts then Error (`Ingest (Kept_failing failure))
      else (
        let (_ : Core.Time_ns.Span.t) = wait tries ~at_least:Core.Time_ns.Span.zero in
        go pairs Read.Afresh (tries + 1))
    in
    match Pairs.length pairs with
    | 0 -> Ok ()
    | _ -> (
        let first = Pairs.key pairs 0 in
        let%bind region = store.locate freshness first in
        match Region.find [ region ] first with
        | None -> Error (`Ingest (Unheld first))
        | Some region -> (
            let inside, rest = Pairs.split pairs region.stop in
            let%bind written =
              store.write region { uuid = store.uuid (); region = region.context; commit_ts; pairs = Pairs.cut inside ~bytes:batch }
            in
            let refused = List.find_map (fun (_, outcome) -> match outcome with Import.Write_failed reason -> Some reason | Wrote _ -> None) written in
            let leading =
              List.find_map
                (fun ((replica : Region.replica), outcome) ->
                  match outcome with
                  | Import.Wrote ssts when Int64.equal replica.peer.id region.context.peer -> Some ssts
                  | Wrote _ | Write_failed _ -> None)
                written
            in
            match (refused, leading) with
            | Some reason, _ -> retry (Write_failed reason) tries
            | None, None -> retry Leaderless tries
            | None, Some ssts ->
                let rec ingest tries waited =
                  let%bind ingested = store.ingest region ssts in
                  match ingested with
                  | Ingested -> go rest Read.Known 0
                  | Ingest_failed (Server_is_busy { backoff_ms; _ } as error) ->
                      if Core.Time_ns.Span.( >= ) waited patience then Error (`Ingest (Kept_failing (Ingest_failed error)))
                      else
                        let span = wait tries ~at_least:(Core.Time_ns.Span.of_int_ms (Int64.to_int backoff_ms)) in
                        ingest (tries + 1) (Core.Time_ns.Span.( + ) waited span)
                  | Ingest_failed (Other { kind = 15; _ } as error) -> Error (`Ingest (Refused error))
                  | Ingest_failed error -> retry (Ingest_failed error) tries
                in
                ingest tries Core.Time_ns.Span.zero))
  in
  go pairs Read.Known 0
