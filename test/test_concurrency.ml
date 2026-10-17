(** One step of a single-threaded mailbox scenario. *)
type step = Add of int | Take | Close

let print_step = function
  | Add value -> Printf.sprintf "add %d" value
  | Take -> "take"
  | Close -> "close"

let mailbox_model =
  QCheck2.Test.make ~count:2000
    ~name:"a mailbox behaves as a queue that close stops filling"
    ~print:(fun steps -> String.concat "; " (List.map print_step steps))
    QCheck2.Gen.(
      list_size (int_range 0 60)
        (oneof_weighted
           [
             (4, map (fun value -> Add value) nat);
             (4, pure Take);
             (1, pure Close);
           ]))
    (fun steps ->
      let mailbox = Quiver_core.Mailbox.create () in
      let model = Queue.create () and closed = ref false in
      List.for_all
        (fun step ->
          match step with
          | Add value when !closed -> (
              match Quiver_core.Mailbox.add mailbox value with
              | Closed -> true
              | Delivered -> false)
          | Add value -> (
              Queue.push value model;
              match Quiver_core.Mailbox.add mailbox value with
              | Delivered -> true
              | Closed -> false)
          | Take when !closed || not (Queue.is_empty model) ->
              Option.equal Int.equal
                (Quiver_core.Mailbox.take mailbox)
                (Queue.take_opt model)
          | Take -> true
          | Close ->
              closed := true;
              Quiver_core.Mailbox.close mailbox;
              true)
        steps)

let mailbox_threads =
  QCheck2.Test.make ~count:300
    ~name:
      "every delivered message is taken once, in each producer's order, across \
       domains"
    ~print:(fun (producers, sent, wanted) ->
      Printf.sprintf "%d producers × %d, consumers want %s" producers sent
        (String.concat "," (List.map string_of_int wanted)))
    QCheck2.Gen.(
      let* producers = int_range 1 4 and* sent = int_range 0 200 in
      let+ wanted =
        list_size (int_range 1 3) (int_range 0 (producers * sent / 3))
      in
      (producers, sent, wanted))
    (fun (producers, sent, wanted) ->
      let (mailbox : (int * int) Quiver_core.Mailbox.t) =
        Quiver_core.Mailbox.create ()
      in
      let producing =
        List.init producers (fun producer ->
            Result.get_ok
              (Quiver_core.Concurrent.start (fun () : (int * int) list ->
                   List.filter_map
                     (fun index ->
                       match Quiver_core.Mailbox.add mailbox (producer, index) with
                       | Delivered -> Some (producer, index)
                       | Closed -> None)
                     (List.init sent Fun.id))))
      in
      let taken =
        Quiver_core.Concurrent.each (List.length wanted)
          (fun consumer : (int * int) list ->
            List.init (List.nth wanted consumer) (fun _ ->
                Option.value (Quiver_core.Mailbox.take mailbox) ~default:(-1, -1)))
      in
      Quiver_core.Mailbox.close mailbox;
      let delivered =
        List.concat_map
          (fun (task : (int * int) list Quiver_core.Concurrent.task) ->
            Quiver_core.Concurrent.join task)
          producing
      in
      let rec drain left =
        match Quiver_core.Mailbox.take mailbox with
        | Some message -> drain (message :: left)
        | None -> List.rev left
      in
      let rest = drain [] in
      let ordered messages =
        List.for_all
          (fun producer ->
            let indices =
              List.filter_map
                (fun (from, index) ->
                  if from = producer then Some index else None)
                messages
            in
            List.equal Int.equal indices (List.sort_uniq Int.compare indices))
          (List.init producers Fun.id)
      in
      let sorted = List.sort compare in
      List.for_all ordered (rest :: taken)
      && List.equal ( = )
           (sorted (List.concat (rest :: taken)))
           (sorted delivered))

(** What a registry scenario does with one number. *)
type entry = Published of int | Published_twice of int * int | Never

let registry =
  QCheck2.Test.make ~count:300
    ~name:
      "an awaited number gets its first published value, or nothing once the \
       registry is closed"
    ~print:(fun (entries, awaiters) ->
      Printf.sprintf "%s; awaiters %s"
        (String.concat ","
           (List.mapi
              (fun number -> function
                | Published value -> Printf.sprintf "%d=%d" number value
                | Published_twice (value, again) ->
                    Printf.sprintf "%d=%d,%d" number value again
                | Never -> Printf.sprintf "%d=_" number)
              entries))
        (String.concat "|"
           (List.map
              (fun numbers ->
                String.concat "," (List.map string_of_int numbers))
              awaiters)))
    QCheck2.Gen.(
      let* count = int_range 1 30 in
      let* entries =
        list_size (pure count)
          (oneof_weighted
             [
               (3, map (fun value -> Published value) nat);
               (1, map2 (fun value again -> Published_twice (value, again)) nat nat);
               (1, pure Never);
             ])
      in
      let+ awaiters =
        list_size (int_range 1 4)
          (list_size (int_range 0 10) (int_range 0 (count - 1)))
      in
      (entries, awaiters))
    (fun (entries, awaiters) ->
      let registry = Quiver_net.Registry.create () in
      let expected number =
        match List.nth entries number with
        | Published value | Published_twice (value, _) -> Some value
        | Never -> None
      in
      let awaiting =
        List.map
          (fun (numbers : int list) ->
            Result.get_ok
              (Quiver_core.Concurrent.start (fun () : int option list ->
                   List.map
                     (fun number -> Quiver_net.Registry.await registry number)
                     numbers)))
          awaiters
      in
      let publishing =
        List.init 3 (fun publisher ->
            Result.get_ok
            @@ Quiver_core.Concurrent.start (fun () ->
                List.iteri
                  (fun number entry ->
                    if number mod 3 = publisher then
                      match entry with
                      | Published value ->
                          Quiver_net.Registry.publish registry number value
                      | Published_twice (value, again) ->
                          Quiver_net.Registry.publish registry number value;
                          Thread.yield ();
                          Quiver_net.Registry.publish registry number again
                      | Never -> ())
                  entries))
      in
      List.iter Quiver_core.Concurrent.join publishing;
      Quiver_net.Registry.close registry;
      let early = List.map Quiver_core.Concurrent.join awaiting in
      let late =
        List.mapi
          (fun number _ ->
            let first = Quiver_net.Registry.await registry number in
            Quiver_net.Registry.forget registry number;
            (first, Quiver_net.Registry.await registry number))
          entries
      in
      let show results =
        String.concat ","
          (List.map
             (Option.fold ~none:"_" ~some:string_of_int)
             results)
      in
      List.iter2
        (fun numbers results ->
          if
            not
              (List.equal (Option.equal Int.equal) results
                 (List.map expected numbers))
          then QCheck2.Test.fail_reportf "an awaiter got %s" (show results))
        awaiters early;
      List.iteri
        (fun number (first, forgotten) ->
          if not (Option.equal Int.equal first (expected number)) then
            QCheck2.Test.fail_reportf "%d got %s after close" number
              (show [ first ]);
          if Option.is_some forgotten then
            QCheck2.Test.fail_reportf "%d got %s once forgotten" number
              (show [ forgotten ]))
        late;
      true)

let buffers =
  QCheck2.Test.make ~count:100
    ~name:"a pooled buffer has one holder at a time, across domains"
    ~print:(fun (domains, rounds) ->
      Printf.sprintf "%d domains × %d rounds" domains rounds)
    QCheck2.Gen.(pair (int_range 2 4) (int_range 1 400))
    (fun (domains, rounds) ->
      let pool = Quiver_core.Buffer_pool.create ()
      and clashes = Portable.Atomic.make 0 in
      let running =
        List.init domains (fun domain ->
            match
              Quiver_core.Concurrent.start
                (fun () ->
                  for round = 1 to rounds do
                    let buffer = Quiver_core.Buffer_pool.take pool in
                    let mark = Printf.sprintf "%d:%d;" domain round in
                    if Quiver_core.Byte_buffer.length (borrow_ buffer) <> 0 then
                      Portable.Atomic.incr clashes;
                    Quiver_core.Byte_buffer.append_string (borrow_ buffer) mark;
                    for _ = 1 to 1 + (round mod 64) do
                      Domain.cpu_relax ()
                    done;
                    if
                      not
                        (String.equal
                           (Quiver_core.Byte_buffer.contents (borrow_ buffer))
                           mark)
                    then Portable.Atomic.incr clashes;
                    Quiver_core.Buffer_pool.give pool buffer
                  done)
            with
            | Ok running -> running
            | Error (`Concurrent _) -> QCheck2.Test.fail_report "no domain")
      in
      List.iter Quiver_core.Concurrent.join running;
      Portable.Atomic.get clashes = 0)

let pool_waits =
  QCheck2.Test.make ~count:50
    ~name:"taking from a pool counts only the time it was blocked"
    ~print:(fun (batches, delay) ->
      Printf.sprintf "%d batches, one more given after %d ms" batches delay)
    QCheck2.Gen.(pair (int_range 1 4) (int_range 0 3))
    (fun (batches, delay) ->
      let pool = Quiver_core.Batch_pool.create ~batches
      and clock = Quiver_core.Stopwatch.start ()
      and delaying = Portable.Atomic.make 0 in
      let now () =
        Core.Time_ns.Span.to_int_ns (Quiver_core.Stopwatch.elapsed clock)
      in
      let rec hold left =
        if left = 0 then (
          let before = Quiver_core.Batch_pool.waited pool in
          let giving =
            Result.get_ok
              (Quiver_core.Concurrent.start (fun () ->
                   Portable.Atomic.set delaying (now ());
                   Thread.delay (float_of_int delay /. 1000.);
                   Quiver_core.Batch_pool.give pool
                     (Quiver_core.Batch.create [])))
          in
          let blocked = Quiver_core.Stopwatch.start () in
          let taking = now () in
          let batch = Quiver_core.Batch_pool.take pool [ Int64 ] in
          let took = Quiver_core.Stopwatch.elapsed blocked in
          Quiver_core.Batch_pool.give pool batch;
          Quiver_core.Concurrent.join giving;
          ( before,
            Quiver_core.Batch_pool.waited pool,
            took,
            Int.max 0 (taking - Portable.Atomic.get delaying) ))
        else
          let batch = Quiver_core.Batch_pool.take pool [ Int64 ] in
          let waits = hold (left - 1) in
          Quiver_core.Batch_pool.give pool batch;
          waits
      in
      let before, after, took, late = hold batches in
      let slack = 1_000_000 in
      Core.Time_ns.Span.equal before Core.Time_ns.Span.zero
      && Core.Time_ns.Span.to_int_ns after >= (delay * 1_000_000) - late - slack
      && Core.Time_ns.Span.( <= ) after took)

let each =
  QCheck2.Test.make ~count:200
    ~name:"each runs every work once, all at the same time, in their order"
    ~print:string_of_int (QCheck2.Gen.int_range 0 8)
    (fun count ->
      let started = Portable.Atomic.make 0 in
      let results =
        Quiver_core.Concurrent.each count (fun index : (int * bool) ->
            Portable.Atomic.incr started;
            let rec everyone waits =
              Portable.Atomic.get started >= count
              || (waits > 0
                 && (Thread.delay 0.001;
                     everyone (waits - 1)))
            in
            let wait_seconds = 5 in
            (index, everyone (wait_seconds * 1000)))
      in
      List.equal Int.equal (List.map fst results) (List.init count Fun.id)
      && List.for_all snd results
      && Portable.Atomic.get started = count)

let spread =
  QCheck2.Test.make ~count:200
    ~name:
      "spread runs every work once, on at most its workers at a time, and \
       returns in item order"
    ~print:(fun (items, workers) ->
      Printf.sprintf "%d items, %d workers" items workers)
    QCheck2.Gen.(pair (int_range 0 40) (int_range 1 6))
    (fun (items, workers) ->
      let calls = Portable.Atomic.make 0
      and running = Portable.Atomic.make 0
      and peak = Portable.Atomic.make 0 in
      let results =
        Quiver_core.Concurrent.spread ~workers
          (Iarray.init items Fun.id)
          (fun item ->
            Portable.Atomic.incr calls;
            let now = Portable.Atomic.fetch_and_add running 1 + 1 in
            let rec raise_peak () =
              let seen = Portable.Atomic.get peak in
              match now <= seen with
              | true -> ()
              | false -> (
                  match
                    Portable.Atomic.compare_and_set peak ~if_phys_equal_to:seen
                      ~replace_with:now
                  with
                  | Set_here -> ()
                  | Compare_failed -> raise_peak ())
            in
            raise_peak ();
            Thread.delay 0.0002;
            Portable.Atomic.decr running;
            2 * item)
      in
      List.equal Int.equal results (List.init items (fun item -> 2 * item))
      && Portable.Atomic.get calls = items
      && Portable.Atomic.get peak <= workers)

let tests =
  List.map QCheck_alcotest.to_alcotest
    [ mailbox_model; mailbox_threads; registry; buffers; pool_waits; each; spread ]
