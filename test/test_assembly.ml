open Quiver_core

(** A sink whose calls only take time. *)
module Sleeper = struct
  type error = Injected

  let sexp_of_error Injected = Sexplib0.Sexp.Atom "injected"

  type t = { render : int; append : int; close : int }
  (** Milliseconds each call sleeps. *)

  type part = { delays : t }
  type renderer = { taking : t }
  type rendered = Rendered

  let sleep milliseconds = Thread.delay (float_of_int milliseconds /. 1000.)
  let format _ = Output_format.Sql
  let database _ _ ~ddl:_ = Ok ()
  let table _ _ ~ddl:_ ~parts:_ = Ok ()
  let objects _ _ _ = Ok ()
  let part delays _ ~index:_ = Ok { delays }
  let renderer { delays } = { taking = delays }

  let render { taking } (_ : Batch.sealed @ local) =
    sleep taking.render;
    Ok Rendered

  let append { delays } Rendered =
    sleep delays.append;
    Ok ()

  let close { delays } answer =
    sleep delays.close;
    answer
      (Ok
         {
           Manifest.file = File_path.Part.of_string "part";
           rows = 0;
           bytes = 0;
           stored_bytes = 0;
           checksum = Xxh3_64 0L;
         })

  let discard _ = ()
  let finish _ ~source:_ ~tables:_ ~manifest:_ = Ok ()
end

module Part = Assembly.Make (Sleeper)

(** Batches delivered one by one, in this order, [gap] milliseconds apart;
    sealed after [sealed_after] deliveries. *)
type scenario = {
  delays : Sleeper.t;
  order : int list;
  gap : int;
  sealed_after : int;
}

let timed =
  QCheck2.Test.make ~count:100
    ~name:
      "a part's stage times cover what its sink spent, and reordering the \
       waits for earlier batches"
    ~print:(fun { delays = { render; append; close }; order; gap; sealed_after } ->
      Printf.sprintf
        "render %d append %d close %d ms, gap %d ms, order %s, sealed after %d"
        render append close gap
        (String.concat "," (List.map string_of_int order))
        sealed_after)
    QCheck2.Gen.(
      let* batches = int_range 1 6 in
      let* order = shuffle_list (List.init batches Fun.id)
      and* render = int_range 0 1
      and* append = int_range 0 1
      and* close = int_range 0 2
      and* gap = int_range 0 2 in
      let+ sealed_after = int_range 0 batches in
      { delays = { render; append; close }; order; gap; sealed_after })
    (fun { delays; order; gap; sealed_after } ->
      let milliseconds span = Core.Time_ns.Span.to_int_ns span / 1_000_000 in
      let batches = List.length order in
      let sealed =
        match Batch.seal (Batch.create [ Int64 ]) with
        | Ok sealed -> sealed
        | Error _ -> QCheck2.Test.fail_report "an empty batch does not seal"
      in
      let part =
        match
          Part.create delays (Support.table [ ("id", Int64, Not_null) ]) ~index:0
        with
        | Ok part -> part
        | Error Injected -> QCheck2.Test.fail_report "a sleeping part fails"
      in
      let ended = Await.Await.Ivar.create () in
      let seal () =
        Part.seal part ~batches (fun outcome ->
            Await.Await.Ivar.fill_if_empty ended outcome)
      in
      let wall = Stopwatch.start () in
      let entered = Array.make batches 0 and left = Array.make batches 0 in
      let now () = Core.Time_ns.Span.to_int_ns (Stopwatch.elapsed wall) in
      List.iteri
        (fun step number ->
          if step = sealed_after then seal ();
          let rendered = Part.render part sealed in
          entered.(step) <- now ();
          Part.deliver part number rendered;
          left.(step) <- now ();
          Sleeper.sleep gap)
        order;
      if sealed_after = batches then seal ();
      let outcome = Await.Await.Ivar.read Blocking.await ended in
      let wall = milliseconds (Stopwatch.elapsed wall) in
      let arrival = Array.make batches 0 in
      List.iteri (fun step number -> arrival.(number) <- step) order;
      let least, most =
        List.fold_left
          (fun (least, most) number ->
            let arrived = arrival.(number) in
            match
              List.fold_left max arrived
                (List.init number (fun earlier -> arrival.(earlier)))
            with
            | appended when appended = arrived -> (least, most)
            | appended ->
                ( least + entered.(appended) - left.(arrived),
                  most + left.(appended) - entered.(arrived) ))
          (0, 0) (List.init batches Fun.id)
      in
      match outcome with
      | Failed _ -> QCheck2.Test.fail_report "a sleeping part failed"
      | Closed { writing = { rendering; appending; reordering; committing }; _ }
        ->
          let rendering = milliseconds rendering
          and appending = milliseconds appending
          and committing = milliseconds committing in
          let within total least = total >= least && total <= wall in
          within rendering (batches * delays.render)
          && within appending (batches * delays.append)
          && within committing delays.close
          && Core.Time_ns.Span.to_int_ns reordering >= least
          && Core.Time_ns.Span.to_int_ns reordering <= most)

let tests = List.map QCheck_alcotest.to_alcotest [ timed ]
