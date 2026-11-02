type transactions = Per_part | Every of int

type stream = { loader : int; mutable rows : int; mutable bytes : int }

type t = {
  transactions : transactions;
  own : stream;
  mutable borrowed : stream list;
  mutable left : stream list;  (** Borrowed ones yet to take a text this round, before the own. *)
}

type wish = Borrow | Enough
type step = Open | Ended of { rows : int; after : after }
and after = Again | Back
type statement = { loader : int; rows : int }

let create transactions loader =
  { transactions; own = { loader; rows = 0; bytes = 0 }; borrowed = []; left = [] }

let wish t = match t.transactions with Per_part -> Enough | Every _ -> Borrow

let add t loader =
  let stream = { loader; rows = 0; bytes = 0 } in
  t.borrowed <- t.borrowed @ [ stream ];
  t.left <- stream :: t.left

let send t ~bytes ~rows =
  let stream, after =
    match t.left with
    | [] ->
        t.left <- t.borrowed;
        (t.own, Again)
    | stream :: rest ->
        t.left <- rest;
        (stream, Back)
  in
  let rows = stream.rows + rows and bytes = stream.bytes + bytes in
  match t.transactions with
  | Every most when bytes >= most ->
      stream.rows <- 0;
      stream.bytes <- 0;
      let () =
        match after with
        | Again -> ()
        | Back -> t.borrowed <- List.filter (fun other -> other != stream) t.borrowed
      in
      (stream.loader, Ended { rows; after })
  | Every _ | Per_part ->
      stream.rows <- rows;
      stream.bytes <- bytes;
      (stream.loader, Open)

let ends t =
  let statement ({ loader; rows; _ } : stream) = { loader; rows } in
  (statement t.own, List.map statement t.borrowed)
