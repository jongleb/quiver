type ('a : value mod contended portable) t = {
  queue : 'a option Await.Await.Mpmc_queue.t;  (** Items, then a [None] per thread to end it. *)
  threads : unit Concurrent.task list;
}

type submitted = Queued | Unqueued

let start (type (a : value mod contended portable)) ~workers (work : a -> unit) : a t =
  let queue = Await.Await.Mpmc_queue.create () in
  let rec take () =
    match Await.Await.Mpmc_queue.pop Blocking.await queue with
    | None -> ()
    | Some item ->
        work item;
        take ()
  in
  {
    queue;
    threads = List.filter_map (fun _ -> Result.to_option (Concurrent.start take)) (List.init (Int.max 0 workers) Fun.id);
  }

let submit t item =
  match t.threads with
  | [] -> Unqueued
  | _ :: _ ->
      Await.Await.Mpmc_queue.push t.queue (Some item);
      Queued

let stop t =
  List.iter (fun _ -> Await.Await.Mpmc_queue.push t.queue None) t.threads;
  List.iter Concurrent.join t.threads
