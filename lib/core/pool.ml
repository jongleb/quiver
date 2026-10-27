type ('a : value mod contended portable) t = {
  waiting : (int * int * 'a) list ref Await.Capsule.Await.With_mutex.t;
      (** Items no thread took yet, with their priority and order of arrival. *)
  tokens : unit option Await.Await.Mpmc_queue.t;  (** One per item, then a [None] per thread to end it. *)
  arrivals : int Portable.Atomic.t;
  threads : unit Concurrent.task list;
}

type submitted = Queued | Unqueued

let start (type (a : value mod contended portable)) ~workers (work : a -> unit) : a t =
  let tokens = Await.Await.Mpmc_queue.create () in
  let waiting = Await.Capsule.Await.With_mutex.create (fun () -> (ref [] : (int * int * a) list ref)) in
  let rec take () =
    match Await.Await.Mpmc_queue.pop Blocking.await tokens with
    | None -> ()
    | Some () ->
        let first =
          Guarded.hold waiting (fun items ->
              let first =
                Base.List.max_elt !items ~compare:(fun (p, a, _) (q, b, _) ->
                    match Int.compare p q with 0 -> Int.compare b a | order -> order)
              in
              Option.iter
                (fun (_, arrival, _) -> items := List.filter (fun (_, other, _) -> other <> arrival) !items)
                first;
              Option.map (fun (_, _, item) -> item) first)
        in
        Option.iter work first;
        take ()
  in
  {
    waiting;
    tokens;
    arrivals = Portable.Atomic.make 0;
    threads = List.filter_map (fun _ -> Result.to_option (Concurrent.start take)) (List.init (Int.max 0 workers) Fun.id);
  }

let submit t ~priority item =
  match t.threads with
  | [] -> Unqueued
  | _ :: _ ->
      let arrival = Portable.Atomic.fetch_and_add t.arrivals 1 in
      Guarded.hold t.waiting (fun items -> items := (priority, arrival, item) :: !items);
      Await.Await.Mpmc_queue.push t.tokens (Some ());
      Queued

let stop t =
  List.iter (fun _ -> Await.Await.Mpmc_queue.push t.tokens None) t.threads;
  List.iter Concurrent.join t.threads
