open Sexplib0.Sexp_conv

(** Tables with their caps and file sizes, the loading sessions, and the seed
    that picks which loading file finishes next. *)
type scenario = {
  tables : (Quiver_load.Schedule.cap * int list) list;
  sessions : int;
  seed : int;
}
[@@deriving sexp_of]

let scenario =
  QCheck2.Gen.(
    let cap =
      oneof
        [
          return Quiver_load.Schedule.Alone;
          map (fun most -> Quiver_load.Schedule.At_most most) (int_range 1 4);
          map (fun most -> Quiver_load.Schedule.First_alone most) (int_range 1 4);
        ]
    in
    let+ tables = list_size (int_range 1 6) (pair cap (list_size (int_range 0 6) (int_range 0 1000)))
    and+ sessions = int_range 1 6
    and+ seed = int in
    { tables; sessions; seed })

(** Runs the schedule with sessions that finish in a random order, checking
    every decision against the rules; the broken ones. *)
let simulate { tables; sessions; seed } =
  let n = List.length tables in
  let caps = Array.of_list (List.map fst tables) in
  let counts = Array.of_list (List.map (fun (_, sizes) -> List.length sizes) tables) in
  let given = Array.make n 0 and active = Array.make n 0 and loaded = Array.make n 0 in
  let schedule =
    Quiver_load.Schedule.create
      (List.mapi
         (fun t (cap, sizes) -> (cap, List.mapi (fun f size -> ((t, f), size)) sizes))
         tables)
  in
  let random = Random.State.make [| seed |] in
  let errors = ref [] in
  let fail fmt = Printf.ksprintf (fun error -> errors := error :: !errors) fmt in
  let room t =
    let shared most = if active.(t) < most then `Within else `Liftable in
    match caps.(t) with
    | Alone -> if active.(t) = 0 then `Within else `Full
    | At_most most -> shared most
    | First_alone most -> if given.(t) > 0 && loaded.(t) = 0 then `Full else shared most
  in
  let pool () =
    let pending = List.filter (fun t -> given.(t) < counts.(t)) (List.init n Fun.id) in
    match List.filter (fun t -> room t = `Within) pending with
    | [] -> List.filter (fun t -> room t = `Liftable) pending
    | within -> within
  in
  let rec fill flight =
    if List.length flight >= sessions then flight
    else
      let pool = pool () in
      match Quiver_load.Schedule.take schedule with
      | None ->
          if not (List.is_empty pool) then
            fail "a session idles while table %d could take it" (List.hd pool);
          flight
      | Some (slot, (t, f)) ->
          if not (List.mem t pool) then fail "table %d given past its cap" t;
          if f <> given.(t) then fail "table %d gave file %d before file %d" t f given.(t);
          if active.(t) > 0 && List.exists (fun u -> active.(u) = 0) pool then
            fail "table %d shared while another nobody loads could take the session" t;
          given.(t) <- given.(t) + 1;
          active.(t) <- active.(t) + 1;
          fill ((slot, t) :: flight)
  in
  let rec run flight =
    match fill flight with
    | [] -> ()
    | flight ->
        let k = Random.State.int random (List.length flight) in
        let slot, t = List.nth flight k in
        Quiver_load.Schedule.release schedule slot;
        active.(t) <- active.(t) - 1;
        loaded.(t) <- loaded.(t) + 1;
        run (List.filteri (fun i _ -> i <> k) flight)
  in
  run [];
  Array.iteri
    (fun t count ->
      if given.(t) <> count then fail "table %d gave %d of its %d files" t given.(t) count)
    counts;
  List.rev !errors

let tests =
  List.map QCheck_alcotest.to_alcotest
    [
      QCheck2.Test.make ~count:2000
        ~name:
          "every file is given once and in its table's order, within the caps, \
           to a table nobody loads first, and no session idles while one may \
           take a file"
        ~print:(fun scenario -> Sexplib0.Sexp.to_string_hum (sexp_of_scenario scenario))
        scenario
        (fun scenario ->
          match simulate scenario with
          | [] -> true
          | errors -> QCheck2.Test.fail_reportf "%s" (String.concat "\n" errors));
    ]
