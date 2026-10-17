open Quiver_core

type domain = {
  ltype : Ltype.t;
  order : int64 -> int64 -> int;
  lowest : int64;
  highest : int64;
}

let exact_cover domain =
  let integer (bound : Key.t) =
    match (bound :> Key.value * Key.value list) with
    | Unsigned value, [] when domain.ltype = Uint64 -> value
    | Signed value, [] when domain.ltype <> Uint64 -> value
    | _ -> Alcotest.fail "not a single integer bound of the key's signedness"
  in
  let simple (chunk : Chunk.t) =
    match chunk with
    | Whole _ -> `Whole
    | Range { range = Below hi; _ } -> `Below (integer hi)
    | Range { range = Between (lo, hi); _ } -> `Between (integer lo, integer hi)
    | Range { range = From lo; _ } -> `From (integer lo)
  in
  let contains order chunk v =
    match chunk with
    | `Whole -> true
    | `Below hi -> order v hi < 0
    | `Between (lo, hi) -> order lo v <= 0 && order v hi < 0
    | `From lo -> order lo v <= 0
  in
  let gen =
    let open QCheck2.Gen in
    let edge =
      oneof_list
        [
          domain.lowest;
          domain.highest;
          0L;
          1L;
          -1L;
          Int64.min_int;
          Int64.max_int;
        ]
    in
    let value = oneof_weighted [ (3, int64); (1, edge) ] in
    let bounds =
      oneof_weighted
        [
          (2, pair value value);
          ( 2,
            map2
              (fun lo width -> (lo, Int64.add lo (Int64.of_int width)))
              value (int_range 0 200) );
        ]
    in
    map3
      (fun (a, b) (target, limit) probes ->
        let lo, hi = if domain.order a b <= 0 then (a, b) else (b, a) in
        (lo, hi, (target, limit), probes))
      bounds
      (pair (int_range 1 1000) (int_range 0 64))
      (list_size (int_range 0 20) value)
  in
  QCheck2.Test.make ~count:2000
    ~print:(fun (lo, hi, (target, limit), _) ->
      Printf.sprintf "min=%Lu max=%Lu target=%d limit=%d" lo hi target limit)
    ~name:
      (Format.asprintf
         "%a chunks bisected on any estimates cover the domain exactly"
         (fun ppf value -> Sexplib0.Sexp.pp_hum ppf (Ltype.sexp_of_t value))
         domain.ltype)
    gen
    (fun (min, max, (target, limit), probes) ->
      let table =
        Support.table [ ("id", domain.ltype, Schema.Field.Not_null) ]
      in
      let bound value =
        match domain.ltype with
        | Uint64 -> Key.Unsigned value
        | _ -> Key.Signed value
      in
      let chunks =
        List.map simple
          (Support.get_ok
             (Chunk.of_bounds table
                (List.map
                   (fun value -> [ bound value ])
                   (Result.get_ok
                      (Bisect.boundaries ~workers:3
                         ~weigh:(fun lo hi ->
                           Ok (Hashtbl.hash (lo, hi) mod 1000))
                         ~middle:(fun lo hi ->
                           Ok (Bisect.integer_middle ~min ~max lo hi))
                         ~target ~limit)))))
      in
      let order = domain.order in
      let bounds =
        List.concat_map
          (function
            | `Whole -> []
            | `Below b | `From b -> [ b ]
            | `Between (lo, hi) -> [ lo; hi ])
          chunks
      in
      let probes =
        min :: max :: Int64.pred min :: Int64.succ max :: domain.lowest
        :: domain.highest
        :: (probes @ bounds @ List.map Int64.pred bounds)
      in
      let covered_once v =
        List.length (List.filter (fun span -> contains order span v) chunks) = 1
      in
      let piece = function
        | `Whole -> (min, max)
        | `Below hi -> (min, Int64.pred hi)
        | `Between (lo, hi) -> (lo, Int64.pred hi)
        | `From lo -> (lo, max)
      in
      let pieces = List.map piece chunks in
      let inside (lo, hi) =
        order min lo <= 0 && order lo hi <= 0 && order hi max <= 0
      in
      List.length chunks <= limit + 1
      && List.for_all covered_once probes
      && List.for_all inside pieces)

let tuple_cover =
  QCheck2.Test.make ~count:2000
    ~name:"composite-key chunks cover tuples exactly by their predicates"
    ~print:(fun (arity, bounds, probes) ->
      Printf.sprintf "arity=%d bounds=%s probes=%s" arity
        (String.concat ";"
           (List.map
              (fun b -> String.concat "," (List.map string_of_int b))
              bounds))
        (String.concat ";"
           (List.map
              (fun b -> String.concat "," (List.map string_of_int b))
              probes)))
    QCheck2.Gen.(
      let* arity = int_range 1 3 in
      let tuple = list_size (pure arity) (int_range (-3) 3) in
      let+ bounds = list_size (int_range 0 12) tuple
      and+ probes = list_size (int_range 1 30) tuple in
      (arity, List.sort_uniq compare bounds, probes))
    (fun (arity, bounds, probes) ->
      let names = List.init arity (Printf.sprintf "k%d") in
      let table =
        Support.table
          ~key:
            (match names with
            | first :: rest -> Primary (first, rest)
            | [] -> No_key)
          (List.map
             (fun name -> (name, Ltype.Int64, Schema.Field.Not_null))
             names)
      in
      let chunks =
        Support.get_ok
          (Chunk.of_bounds table
             (List.map (List.map (fun v -> Key.Signed (Int64.of_int v))) bounds))
      in
      let rec holds key (predicate : Predicate.t) =
        match predicate with
        | Always -> true
        | Compare { field; comparison; value = Signed v } -> (
            let index =
              Option.value ~default:(-1)
                (List.find_index
                   (fun name -> name = (field.name :> string))
                   names)
            in
            let k = Int64.of_int (List.nth key index) in
            match comparison with
            | Less -> Int64.compare k v < 0
            | Equal -> Int64.equal k v
            | Greater -> Int64.compare k v > 0
            | At_least -> Int64.compare k v >= 0)
        | Compare _ -> false
        | All (first, rest) -> List.for_all (holds key) (first :: rest)
        | Any (first, rest) -> List.exists (holds key) (first :: rest)
      in
      List.length chunks = List.length bounds + 1
      && List.for_all
           (fun key ->
             List.length
               (List.filter
                  (fun chunk -> holds key (Predicate.of_chunk chunk))
                  chunks)
             = 1)
           (probes @ bounds))

let expect_error name expected result =
  Alcotest.test_case name `Quick (fun () ->
      match result with
      | Ok _ -> Alcotest.fail "split succeeded"
      | Error (`Key error) ->
          Alcotest.failf "%a"
            (fun ppf value ->
              Sexplib0.Sexp.pp_hum ppf (Key.sexp_of_error value))
            error
      | Error (`Chunk error) ->
          Alcotest.check
            (Alcotest.testable
               (fun ppf value ->
                 Sexplib0.Sexp.pp_hum ppf (Chunk.sexp_of_error value))
               Chunk.equal_error)
            "error" expected error)

let tests =
  [
    Alcotest.test_case "chunks print as table and key range" `Quick (fun () ->
        let table =
          Support.table
            ~key:(Primary ("id", []))
            [ ("id", Ltype.Bytes, Schema.Field.Not_null) ]
        in
        let chunks =
          Support.get_ok (Chunk.of_bounds table [ [ Key.Binary "\192\001" ] ])
        in
        Alcotest.(check (list string))
          "printed"
          [ "db.t, id below X'C001'"; "db.t, id from X'C001'" ]
          (List.map (Format.asprintf "%a" Chunk.pp) chunks));
    QCheck_alcotest.to_alcotest
      (exact_cover
         {
           ltype = Int64;
           order = Int64.compare;
           lowest = Int64.min_int;
           highest = Int64.max_int;
         });
    QCheck_alcotest.to_alcotest tuple_cover;
    QCheck_alcotest.to_alcotest
      (exact_cover
         {
           ltype = Uint64;
           order = Int64.unsigned_compare;
           lowest = 0L;
           highest = -1L;
         });
    expect_error "no key" (Chunk.Unsplittable_key Support.name)
      (Chunk.of_bounds
         (Support.table ~key:No_key [ ("id", Int64, Nullable) ])
         [ [ Key.Signed 1L ] ]);
  ]
