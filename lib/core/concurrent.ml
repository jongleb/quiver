type error = string [@@deriving sexp_of ~portable]
type ('a : value mod contended portable) task = 'a Await.Await.Ivar.t

let start (type (a : value mod contended portable)) (work : unit -> a) =
  let result = Await.Await.Ivar.create () in
  match
    Multicore.spawn
      (fun () -> Await.Await.Ivar.fill_if_empty result (work ()))
      ()
  with
  | Spawned -> Ok result
  | Failed ((), failure, _) -> Error (`Concurrent (Printexc.to_string failure))
  | exception Sys_error reason -> Error (`Concurrent reason)

let join task = Await.Await.Ivar.read Blocking.await task

let each (type (a : value mod contended portable)) count (work : int -> a) :
    a list =
  List.init count (fun (index : int) ->
      match start (fun () -> work index) with
      | Ok task -> `Started task
      | Error (`Concurrent _) -> `Done (work index))
  |> List.map (fun (step : [ `Started of a task | `Done of a ]) ->
         match step with `Started task -> join task | `Done value -> value)

let both (type (a : value mod contended portable)) (first : unit -> a) second =
  match start first with
  | Ok task ->
      let other = second () in
      (join task, other)
  | Error (`Concurrent _) ->
      let other = second () in
      (first (), other)

let meanwhile ~every again work =
  let running = Portable.Atomic.make `Running and every = Core.Time_ns.Span.to_sec every in
  let (), result =
    both
      (fun () ->
        let rec wait asked =
          match Portable.Atomic.get running with
          | `Done -> ()
          | `Running ->
              Unix.sleepf (Float.min 0.05 every);
              let now = Unix.gettimeofday () in
              let asked =
                if now -. asked < every then asked
                else
                  let () = again () in
                  now
              in
              wait asked
        in
        wait (Unix.gettimeofday ()))
      (fun () ->
        let result = work () in
        Portable.Atomic.set running `Done;
        result)
  in
  result

let held ~every ~on ~off ask work =
  let release () = ignore (ask off) in
  match ask on with
  | Error error ->
      release ();
      Error error
  | Ok () ->
      let result = meanwhile ~every (fun () -> ignore (ask on)) work in
      release ();
      Ok result

let spread (type (a : value mod contended portable) (b : value mod contended portable))
    ~workers (items : a iarray) (work : a -> b) : b list =
  let next = Portable.Atomic.make 0 in
  List.concat
    (each
       (Int.max 1 (Int.min workers (Iarray.length items)))
       (fun _ ->
         let rec take taken =
           let index = Portable.Atomic.fetch_and_add next 1 in
           if index < Iarray.length items then
             take ((index, work (Iarray.get items index)) :: taken)
           else taken
         in
         take []))
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map snd
