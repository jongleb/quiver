open Sexplib0.Sexp_conv
open Quiver_load

(** A text of the part, and whether an idle loader is there to borrow before it. *)
type text = { rows : int; bytes : int; idle : bool } [@@deriving sexp_of]

(** What a loader runs, as the target sees it. *)
type loader = {
  mutable held : bool;  (** By the part: its own or borrowed. *)
  mutable statement : (int * int) option;  (** The open [LOAD DATA]: rows and bytes sent. *)
  mutable given : int;  (** Times given back to the idle ones. *)
}

let spread =
  QCheck2.Test.make ~count:3000
    ~name:
      "a part's texts go in turn to its own loader and the idle ones it borrows, each statement \
       ends with the rows sent to it once it reaches the bytes asked, and each borrowed loader \
       is given back once"
    ~print:(fun (most, texts) -> Sexplib0.Sexp.to_string_hum ([%sexp_of: int option * text list] (most, texts)))
    QCheck2.Gen.(
      pair
        (option (int_range 1 60))
        (list_size (int_range 0 60)
           (let+ rows = int_range 0 5 and+ bytes = int_range 0 25 and+ idle = bool in
            { rows; bytes; idle })))
    (fun (most, texts) ->
      let transactions = Option.fold most ~none:Fanout.Per_part ~some:(fun most -> Fanout.Every most) in
      let loaders = Array.init (List.length texts + 1) (fun _ -> { held = false; statement = None; given = 0 }) in
      let fail = ref [] in
      let check reason ok = if not ok then fail := reason :: !fail in
      let fanout = Fanout.create transactions 0 in
      loaders.(0).held <- true;
      loaders.(0).statement <- Some (0, 0);
      let committed = ref 0 and borrowed = ref 0 and previous = ref None in
      List.iter
        (fun { rows; bytes; idle } ->
          let () =
            match Fanout.wish fanout with
            | Enough -> check "borrows without statements ending" (Option.is_none most)
            | Borrow ->
                check "borrows nothing though statements end" (Option.is_some most);
                if idle then begin
                  incr borrowed;
                  let loader = loaders.(!borrowed) in
                  loader.held <- true;
                  loader.statement <- Some (0, 0);
                  Fanout.add fanout !borrowed
                end
          in
          let others = Array.fold_left (fun n { held; _ } -> if held then n + 1 else n) 0 loaders > 1 in
          let at, step = Fanout.send fanout ~bytes ~rows in
          let loader = loaders.(at) in
          check "sent to a loader the part does not hold" loader.held;
          check "the same loader twice in a row though others are held"
            (not (others && Option.equal Int.equal !previous (Some at)));
          previous := Some at;
          let sent_rows, sent_bytes = Option.value loader.statement ~default:(0, 0) in
          let sent_rows = sent_rows + rows and sent_bytes = sent_bytes + bytes in
          let full = Option.fold most ~none:false ~some:(fun most -> sent_bytes >= most) in
          match step with
          | Open ->
              check "a full statement left open" (not full);
              loader.statement <- Some (sent_rows, sent_bytes)
          | Ended { rows; after } -> (
              check "a statement ended before it is full" full;
              check "ended with other rows than sent" (rows = sent_rows);
              committed := !committed + rows;
              match after with
              | Again ->
                  check "the own loader given away" (at = 0);
                  loader.statement <- Some (0, 0)
              | Back ->
                  check "a borrowed loader kept" (at <> 0);
                  loader.statement <- None;
                  loader.held <- false;
                  loader.given <- loader.given + 1))
        texts;
      let own, rest = Fanout.ends fanout in
      check "the own statement is not the own loader's" (own.loader = 0);
      List.iter
        (fun ({ loader; rows } : Fanout.statement) ->
          let state = loaders.(loader) in
          check "an open statement on a loader not held" state.held;
          check "an open statement with other rows than sent"
            (Option.equal Int.equal (Option.map fst state.statement) (Some rows));
          committed := !committed + rows;
          if loader <> 0 then begin
            state.held <- false;
            state.given <- state.given + 1
          end)
        (own :: rest);
      check "rows lost or doubled" (!committed = Base.List.sum (module Base.Int) texts ~f:(fun { rows; _ } -> rows));
      Array.iteri
        (fun i { held; given; _ } ->
          if i > 0 then check "a borrowed loader not given back once" ((not held) && given = (if i <= !borrowed then 1 else 0)))
        loaders;
      match !fail with
      | [] -> true
      | reasons -> QCheck2.Test.fail_report (String.concat "; " (List.sort_uniq String.compare reasons)))

let tests = [ QCheck_alcotest.to_alcotest spread ]
