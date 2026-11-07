open Base.Result.Let_syntax

type error =
  | Unheld of string
  | Region_kept_failing of Kv.region_error
  | Still_locked of Kv.lock
  | Key of Kv.key_error
  | Disordered of string
  | Disagreeing of int64
[@@deriving sexp_of ~portable]

type freshness = Known | Afresh

type 'e store = {
  locate : freshness -> string -> (Region.t, 'e) result;
  scan : Region.t -> Kv.settled -> start:string -> stop:Kv.bound -> (Kv.scan Kv.reply, 'e) result;
  check : Region.t -> Kv.check -> (Kv.status Kv.reply, 'e) result;
  secondaries : Region.t -> version:int64 -> string list -> (Kv.secondaries Kv.reply, 'e) result;
  now : unit -> (int64, 'e) result;
  pause : Core.Time_ns.Span.t -> unit;
}

let attempts = 20
let patience = Core.Time_ns.Span.of_int_sec 300

type verdict = Ignored | Read_through | Waiting
type decided = All_locked of int64 | Committed_at of int64 | Rolled_back | Fallen_back
type waiting = { rounds : int; spent : Core.Time_ns.Span.t }

let range store ~version ~start ~stop ~limit ~f =
  let calm = { rounds = 0; spent = Core.Time_ns.Span.zero } in
  let before key (bound : Kv.bound) = match bound with Endless -> true | Before stop -> String.compare key stop < 0 in
  let backoff tries = Core.Time_ns.Span.of_int_ms (Int.min 1000 (2 lsl Int.min tries 20)) in
  let wait tries (error : Kv.region_error) =
    let at_least =
      match error with
      | Server_is_busy { backoff_ms; _ } -> Core.Time_ns.Span.of_int_ms (Int64.to_int backoff_ms)
      | Not_leader _ | Epoch_not_match | Region_not_found | Other _ -> Core.Time_ns.Span.zero
    in
    store.pause (Core.Time_ns.Span.max at_least (backoff tries))
  in
  let holding freshness key =
    let%bind region = store.locate freshness key in
    Option.to_result ~none:(`Read (Unheld key)) (Region.find [ region ] key)
  in
  let expired (current : Kv.clock) ~start ~ttl =
    match current with
    | Past_every_ttl -> true
    | At now -> Int64.compare (Int64.add (Int64.shift_right_logical start 18) ttl) (Int64.shift_right_logical now 18) <= 0
  in
  let seen commit = if Int64.unsigned_compare commit version <= 0 then Read_through else Ignored in
  let decide (lock : Kv.lock) ~min_commit_ts secondaries =
    let disagreeing = Error (`Read (Disagreeing lock.version)) in
    let combine decided next =
      match (decided, next) with
      | Fallen_back, _ | _, Fallen_back -> Ok Fallen_back
      | All_locked least, All_locked other ->
          let greatest = if Int64.unsigned_compare least other < 0 then other else least in
          Ok (All_locked greatest)
      | All_locked least, Committed_at commit -> if Int64.unsigned_compare commit least < 0 then disagreeing else Ok next
      | All_locked _, Rolled_back | Rolled_back, (All_locked _ | Rolled_back) -> Ok Rolled_back
      | Committed_at _, All_locked _ -> Ok decided
      | Committed_at commit, Committed_at other -> if Int64.equal commit other then Ok decided else disagreeing
      | Committed_at _, Rolled_back | Rolled_back, Committed_at _ -> disagreeing
    in
    let found (locks : Kv.lock list) keys =
      if List.length locks < List.length keys then Ok Rolled_back
      else
        Base.List.fold_result locks ~init:(All_locked 0L) ~f:(fun decided (found : Kv.lock) ->
            if not (Int64.equal found.version lock.version) then disagreeing
            else
              match found.protocol with
              | Two_phase -> Ok Fallen_back
              | Async_commit _ -> combine decided (All_locked found.min_commit_ts))
    in
    let rec check keys freshness tries decided =
      let%bind located = Base.Result.all (List.map (fun key -> Result.map (fun region -> (region, key)) (holding freshness key)) keys) in
      let groups =
        List.rev
          (Base.List.fold located ~init:[] ~f:(fun groups ((region : Region.t), key) ->
               match List.partition (fun ((other : Region.t), _) -> Int64.equal other.context.id region.context.id) groups with
               | [ (region, keys) ], rest -> (region, key :: keys) :: rest
               | _, _ -> (region, [ key ]) :: groups))
      in
      Base.List.fold_result groups ~init:decided ~f:(fun decided (region, keys) ->
          let keys = List.rev keys in
          let%bind reply = store.secondaries region ~version:lock.version keys in
          match reply with
          | Region_error error ->
              if tries >= attempts then Error (`Read (Region_kept_failing error))
              else (
                wait tries error;
                check keys Afresh (tries + 1) decided)
          | Key_error error -> Error (`Read (Key error))
          | Reply (Committed_secondary commit) -> combine decided (Committed_at commit)
          | Reply (Locks_found locks) ->
              let%bind next = found locks keys in
              combine decided next)
    in
    check secondaries Known 0 (All_locked min_commit_ts)
  in
  let rec verdict (lock : Kv.lock) ~(missing : Kv.missing) ~(sync : Kv.sync) =
    let%bind current = if Int64.equal lock.ttl 0L then Ok Kv.Past_every_ttl else Result.map (fun now -> Kv.At now) (store.now ()) in
    let rec ask freshness tries =
      let%bind region = holding freshness lock.primary in
      let%bind reply = store.check region { primary = lock.primary; version = lock.version; caller = version; current; missing; sync } in
      match reply with
      | Region_error error ->
          if tries >= attempts then Error (`Read (Region_kept_failing error))
          else (
            wait tries error;
            ask Afresh (tries + 1))
      | Key_error Txn_not_found -> (
          match missing with
          | Leave_if_missing when expired current ~start:lock.version ~ttl:lock.ttl -> verdict lock ~missing:Rollback_if_missing ~sync
          | Leave_if_missing | Rollback_if_missing -> Ok Waiting)
      | Key_error error -> Error (`Read (Key error))
      | Reply (Committed commit) -> Ok (seen commit)
      | Reply Rolled_back -> Ok Ignored
      | Reply (Unsettled _) -> Ok Waiting
      | Reply (Uncommitted { ttl; pushed; min_commit_ts; protocol }) -> (
          match (protocol, sync, pushed) with
          | Async_commit secondaries, As_locked, _ -> (
              if not (expired current ~start:lock.version ~ttl) then Ok Waiting
              else
                let%bind decided = decide lock ~min_commit_ts secondaries in
                match decided with
                | Fallen_back -> verdict lock ~missing ~sync:Sync_commit_forced
                | Rolled_back -> Ok Ignored
                | Committed_at commit | All_locked commit -> Ok (seen commit))
          | (Async_commit _ | Two_phase), _, Pushed -> Ok Ignored
          | (Async_commit _ | Two_phase), _, Not_pushed -> Ok Waiting)
    in
    ask Known 0
  in
  let rec go key freshness tries (settled : Kv.settled) waiting =
    if not (before key stop) then Ok ()
    else
      let%bind region = holding freshness key in
      let upper : Kv.bound =
        match (stop, region.stop) with
        | Endless, bound | bound, Endless -> bound
        | Before a, Before b -> Before (Base.String.min a b)
      in
      let%bind scanned = store.scan region settled ~start:key ~stop:upper in
      match scanned with
      | Reply (Pairs pairs) -> (
          let rec ordered previous = function
            | [] -> Ok ()
            | (pair : Kv.pair) :: rest ->
                if String.compare pair.key previous < 0 || not (before pair.key upper) then Error (`Read (Disordered pair.key))
                else ordered (pair.key ^ "\x00") rest
          in
          let%bind () = ordered key pairs in
          let%bind () = match pairs with [] -> Ok () | _ :: _ -> f pairs in
          let past () = match upper with Endless -> Ok () | Before next -> go next Known 0 settled calm in
          if List.length pairs < limit then past ()
          else
            match Base.List.last pairs with
            | Some last -> go (last.key ^ "\x00") Known 0 settled calm
            | None -> past ())
      | Region_error error ->
          if tries >= attempts then Error (`Read (Region_kept_failing error))
          else (
            wait tries error;
            go key Afresh (tries + 1) settled waiting)
      | Key_error error -> Error (`Read (Key error))
      | Reply (Locks (first, rest)) -> (
          let known version = List.mem version settled.ignored || List.mem version settled.committed in
          let fresh =
            Base.List.dedup_and_sort ~compare:(fun (a : Kv.lock) b -> Int64.compare a.version b.version)
              (List.filter (fun (lock : Kv.lock) -> not (known lock.version)) (first :: rest))
          in
          let%bind next, blocking =
            Base.List.fold_result fresh ~init:(settled, None) ~f:(fun ((settled : Kv.settled), blocking) lock ->
                let%bind verdict = verdict lock ~missing:Leave_if_missing ~sync:As_locked in
                match verdict with
                | Ignored -> Ok ({ settled with ignored = lock.version :: settled.ignored }, blocking)
                | Read_through -> Ok ({ settled with committed = lock.version :: settled.committed }, blocking)
                | Waiting -> Ok (settled, Some (Option.value blocking ~default:lock)))
          in
          let progress = List.length next.ignored + List.length next.committed > List.length settled.ignored + List.length settled.committed in
          match (blocking, fresh) with
          | None, _ :: _ -> go key Known tries next calm
          | Some _, _ | None, [] ->
              let lock = Option.value blocking ~default:first in
              let waiting = if progress then calm else waiting in
              if Core.Time_ns.Span.( >= ) waiting.spent patience then Error (`Read (Still_locked lock))
              else
                let span = backoff waiting.rounds in
                store.pause span;
                go key Known tries next { rounds = waiting.rounds + 1; spent = Core.Time_ns.Span.( + ) waiting.spent span })
  in
  go start Known 0 Kv.nothing_settled calm
