type t = {
  free : Batch.t Free_list.t;
  available : Await.Await.Semaphore.t;
  waited : int Portable.Atomic.t;  (** Nanoseconds takers were blocked. *)
}

let create ~batches =
  let batches = Int.max 1 batches in
  let free = Free_list.create () in
  Base.Fn.apply_n_times ~n:batches
    (fun () -> Free_list.put free (Batch.create []))
    ();
  {
    free;
    available = Await.Await.Semaphore.create batches;
    waited = Portable.Atomic.make 0;
  }

let take t ltypes =
  let () =
    match Await.Await.Semaphore.try_acquire t.available with
    | Acquired -> ()
    | Would_block ->
        let blocked = Stopwatch.start () in
        Await.Await.Semaphore.acquire Blocking.await t.available;
        Portable.Atomic.add t.waited
          (Core.Time_ns.Span.to_int_ns (Stopwatch.elapsed blocked))
  in
  let rec pop () =
    match Free_list.take t.free with
    | This batch -> batch
    | Null ->
        Domain.cpu_relax ();
        pop ()
  in
  let batch = pop () in
  Batch.reshape (borrow_ batch) ltypes;
  batch

let give t batch =
  Free_list.put t.free batch;
  Await.Await.Semaphore.release t.available

let waited t = Core.Time_ns.Span.of_int_ns (Portable.Atomic.get t.waited)
