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
         Sexplib0.Sexp.pp_hum
         (Ltype.sexp_of_t domain.ltype))
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
        Base.List.count chunks ~f:(fun span -> contains order span v) = 1
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
            Sexplib0.Sexp.pp_hum
            (Key.sexp_of_error error)
      | Error (`Chunk error) ->
          Alcotest.check
            (Support.testable Chunk.sexp_of_error Chunk.equal_error)
            "error" expected error)

let indexed_cover =
  QCheck2.Test.make ~count:300
    ~name:
      "a table read through an index: every row, however many share its value, falls in \
       exactly one chunk, and ranges are read through the index"
    ~print:(fun (bounds, rows) ->
      Printf.sprintf "bounds=%s rows=%s"
        (String.concat "," (List.map string_of_int bounds))
        (String.concat "," (List.map string_of_int rows)))
    QCheck2.Gen.(
      let+ bounds = list_size (int_range 0 8) (int_range (-20) 20)
      and+ rows = list_size (int_range 1 60) (int_range (-25) 25) in
      (List.sort_uniq compare bounds, rows))
    (fun (bounds, rows) ->
      let table =
        Support.table
          ~key:(Indexed { index = Support.ident "i"; field = "k" })
          [ ("v", Ltype.Int64, Schema.Field.Nullable); ("k", Ltype.Int64, Schema.Field.Not_null) ]
      in
      let rec holds k (predicate : Predicate.t) =
        match predicate with
        | Always -> true
        | Compare { field = { name; _ }; value = Signed v; comparison }
          when String.equal (name :> string) "k" -> (
            let k = Int64.of_int k in
            match comparison with
            | Less -> Int64.compare k v < 0
            | Equal -> Int64.equal k v
            | Greater -> Int64.compare k v > 0
            | At_least -> Int64.compare k v >= 0)
        | Compare _ -> false
        | All (first, rest) -> List.for_all (holds k) (first :: rest)
        | Any (first, rest) -> List.exists (holds k) (first :: rest)
      in
      let chunks =
        Support.get_ok
          (Chunk.of_bounds table (List.map (fun v -> [ Key.Signed (Int64.of_int v) ]) bounds))
      in
      let lowest = List.fold_left min max_int rows and highest = List.fold_left max min_int rows in
      let closed =
        Chunk.closed (Chunk.whole table) (Key.Signed (Int64.of_int lowest))
          (Key.Signed (Int64.of_int highest))
      in
      let sql chunk = Quiver_mysql.Query.sql (Chunk { chunk; sources = [] }) in
      let through chunk =
        let text = sql chunk in
        match chunk with
        | Range _ ->
            Base.String.is_substring text ~substring:"FORCE INDEX (`i`)"
            && not (Base.String.is_substring text ~substring:"ORDER BY")
        | Whole _ -> not (Base.String.is_substring text ~substring:"FORCE INDEX")
      in
      List.for_all
        (fun k ->
          List.length (List.filter (fun chunk -> holds k (Predicate.of_chunk chunk)) chunks) = 1
          && Option.fold ~none:false
               ~some:(fun chunk -> holds k (Predicate.of_chunk chunk))
               closed)
        rows
      && List.for_all through (Chunk.whole table :: Option.to_list closed @ chunks)
      && Option.is_some (Chunk.integer_key (Chunk.whole table)))

let chosen_key =
  QCheck2.Test.make ~count:500
    ~name:
      "a table with a primary key is chunked by it or read whole; without one, by the first \
       usable unique index, else whole or, when asked, by the best index to force"
    ~print:(fun (keyless_read, fields, keyable, indexes) ->
      Printf.sprintf "%s fields=%s keyable=%s indexes=%s"
        (Quiver_mysql.Provider.string_of_keyless_read keyless_read)
        (String.concat "," (List.map (fun (name, integer) -> name ^ if integer then ":int" else "") fields))
        (String.concat "," keyable)
        (String.concat ";"
           (List.map
              (fun (index, columns, unique, whole, cardinality, forceable) ->
                Printf.sprintf "%s(%s)%s%s%s#%d" index
                  (String.concat "," (List.map (Option.value ~default:"<expr>") columns))
                  (if unique then " unique" else "")
                  (if whole then "" else " prefix")
                  (if forceable then "" else " unforceable")
                  cardinality)
              indexes)))
    QCheck2.Gen.(
      let names = [ "a"; "b"; "c"; "d" ] in
      let column = oneof_weighted [ (6, map Option.some (oneof_list names)); (1, return None) ] in
      let index name =
        let+ columns = list_size (int_range 1 2) column
        and+ unique = oneof_weighted [ (1, return true); (3, return false) ]
        and+ whole = oneof_weighted [ (4, return true); (1, return false) ]
        and+ cardinality = int_range 0 3
        and+ forceable = oneof_weighted [ (4, return true); (1, return false) ] in
        (name, columns, unique || String.equal name "PRIMARY", whole, cardinality, forceable)
      in
      let+ fields = flatten_list (List.map (fun name -> map (fun integer -> (name, integer)) bool) names)
      and+ keyable = map (List.filter_map Fun.id) (flatten_list (List.map (fun name -> option ~ratio:0.8 (return name)) names))
      and+ primary = option (index "PRIMARY")
      and+ others = flatten_list (List.map index [ "i"; "j"; "k" ]) >>= shuffle_list
      and+ count = int_range 0 3
      and+ keyless_read = oneof_list Quiver_mysql.Provider.all_of_keyless_read in
      let others = List.filteri (fun i _ -> i < count) others in
      (keyless_read, fields, keyable, Option.to_list primary @ others))
    (fun (keyless_read, fields, keyable, indexes) ->
      let id = Support.ident in
      let key =
        Quiver_mysql.Provider.key keyless_read
          ~fields:
            (List.map
               (fun (name, integer) ->
                 {
                   Schema.Field.name = id name;
                   ltype = (if integer then Ltype.Int32 else Ltype.String);
                   nullability = Not_null;
                 })
               fields)
          ~keyable:(List.map id keyable)
          (List.concat_map
             (fun (index, columns, unique, whole, cardinality, forceable) ->
               List.map
                 (fun column ->
                   {
                     Quiver_mysql.Provider.index = id index;
                     column = Option.map id column;
                     uniqueness = (if unique then `Unique else `Repeating);
                     part = (if whole then `Whole else `Prefix);
                     cardinality;
                     reach = (if forceable then `Forceable else `Unforceable);
                   })
                 columns)
             indexes)
      in
      let usable = function Some column -> List.mem column keyable | None -> false in
      let names columns = List.map (fun column -> id (Option.get column)) columns in
      let rank (index, columns, _, _, cardinality, _) =
        let integer = List.assoc (Option.get (List.hd columns)) fields in
        ((if integer then 0 else 1), -cardinality, index)
      in
      let indexed =
        List.filter
          (fun (_, columns, _, whole, _, forceable) -> whole && forceable && usable (List.hd columns))
          indexes
      in
      let unique =
        List.find_opt
          (fun (_, columns, unique, _, _, _) -> unique && List.for_all usable columns)
          indexes
      in
      let expected : Ident.t Schema.Table.key option =
        match List.find_opt (fun (index, _, _, _, _, _) -> String.equal index "PRIMARY") indexes with
        | Some (_, columns, _, _, _, _) when List.for_all usable columns ->
            Some (Primary (List.hd (names columns), List.tl (names columns)))
        | Some _ -> Some No_key
        | None -> (
            match (unique, indexed, keyless_read) with
            | Some (index, columns, _, _, _, _), _, _ ->
                Some (Unique { index = id index; fields = (List.hd (names columns), List.tl (names columns)) })
            | None, [], _ | None, _, Scan -> Some No_key
            | None, _ :: _, Ranges -> None)
      in
      match (expected, key) with
      | Some expected, key ->
          let sexp = Schema.Table.sexp_of_key Ident.sexp_of_t in
          Sexplib0.Sexp.equal (sexp expected) (sexp key)
      | None, Indexed { index; field } ->
          let best = List.fold_left (fun best entry -> min best (rank entry)) (rank (List.hd indexed)) indexed in
          List.exists
            (fun ((name, columns, _, _, _, _) as entry) ->
              String.equal name (index :> string)
              && Ident.equal (id (Option.get (List.hd columns))) field
              && (let r, c, _ = rank entry and r', c', _ = best in r = r' && c = c'))
            indexed
      | None, (Primary _ | Unique _ | No_key) -> false)

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
    QCheck_alcotest.to_alcotest indexed_cover;
    QCheck_alcotest.to_alcotest chosen_key;
    QCheck_alcotest.to_alcotest
      (exact_cover
         {
           ltype = Uint64;
           order = Int64.unsigned_compare;
           lowest = 0L;
           highest = -1L;
         });
    Alcotest.test_case "an index's field that may be NULL is no key" `Quick (fun () ->
        let field (name, ltype, nullability) =
          { Schema.Field.name = Support.ident name; ltype; nullability }
        in
        match
          Schema.Table.make ~name:Support.name
            ~fields:[ field ("k", Ltype.Int64, Schema.Field.Nullable) ]
            ~generated:[]
            ~key:(Indexed { index = Support.ident "i"; field = Support.ident "k" })
            ~estimate:{ rows = 0; bytes = 0 }
        with
        | Error (`Schema (Schema.Nullable_key_field _)) -> ()
        | Error (`Schema error) ->
            Alcotest.failf "%a" Sexplib0.Sexp.pp_hum (Schema.sexp_of_error error)
        | Ok _ -> Alcotest.fail "a nullable field became the key");
    expect_error "no key" (Chunk.Unsplittable_key Support.name)
      (Chunk.of_bounds
         (Support.table ~key:No_key [ ("id", Int64, Nullable) ])
         [ [ Key.Signed 1L ] ]);
  ]
