type cap = Alone | At_most of int | First_alone of int [@@deriving sexp_of]

type 'a table = {
  cap : cap;
  mutable files : ('a * int) list;
  mutable left : int;  (** Bytes of the files not given out. *)
  mutable active : int;
  mutable given : int;
  mutable loaded : int;
}

type 'a t = 'a table array
type slot = int

(** Whether one more session may load a table now. *)
type room = Within | Liftable | Full

let create tables =
  Array.of_list
    (List.map
       (fun (cap, files) ->
         {
           cap;
           files;
           left = Base.List.sum (module Base.Int) files ~f:snd;
           active = 0;
           given = 0;
           loaded = 0;
         })
       tables)

let take t =
  let room table =
    let shared most = if table.active < Int.max most 1 then Within else Liftable in
    match table.cap with
    | Alone -> if table.active = 0 then Within else Full
    | At_most most -> shared most
    | First_alone most -> if table.given > 0 && table.loaded = 0 then Full else shared most
  in
  let pending =
    List.filter (fun i -> not (List.is_empty t.(i).files)) (List.init (Array.length t) Fun.id)
  in
  let within = List.filter (fun i -> room t.(i) = Within) pending in
  let pool =
    match within with
    | [] -> List.filter (fun i -> room t.(i) = Liftable) pending
    | _ -> within
  in
  let busy = Base.Array.sum (module Base.Int) t ~f:(fun table -> table.active)
  and left = Base.List.sum (module Base.Int) pending ~f:(fun i -> t.(i).left) in
  let untouched = List.filter (fun i -> t.(i).active = 0) pool in
  let chosen =
    match untouched with
    | [] ->
        Base.List.max_elt pool ~compare:(fun a b ->
            let score i = (t.(i).left * busy) - (t.(i).active * left) in
            Base.Comparable.lexicographic
              [
                (fun a b -> Int.compare (score a) (score b));
                (fun a b -> Int.compare t.(a).left t.(b).left);
              ]
              a b)
    | _ ->
        Base.List.max_elt untouched ~compare:(fun a b ->
            Base.Comparable.lexicographic
              [
                (fun a b -> Bool.compare (t.(a).given > 0) (t.(b).given > 0));
                (fun a b -> Int.compare t.(a).left t.(b).left);
              ]
              a b)
  in
  Option.bind chosen (fun i ->
      let table = t.(i) in
      match table.files with
      | [] -> None
      | (file, size) :: rest ->
          table.files <- rest;
          table.left <- table.left - size;
          table.active <- table.active + 1;
          table.given <- table.given + 1;
          Some (i, file))

let release t i =
  let table = t.(i) in
  table.active <- table.active - 1;
  table.loaded <- table.loaded + 1
