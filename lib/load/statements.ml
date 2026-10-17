type error = string

let sexp_of_error text =
  Sexplib0.Sexp.Atom (Printf.sprintf "unterminated statement: %S" text)

type pending = {
  mutable data : Bytes.t;
  mutable start : int;
  mutable stop : int;
}
(** Read text not yet handed out: [data] from [start] to [stop]. *)

let iter read f =
  let pending = { data = Bytes.create 65536; start = 0; stop = 0 } in
  let rec statements from =
    if from + 1 >= pending.stop then Ok from
    else if
      Bytes.unsafe_get pending.data from = ';'
      && Bytes.unsafe_get pending.data (from + 1) = '\n'
    then (
      let statement =
        Bytes.sub_string pending.data pending.start (from - pending.start)
      in
      pending.start <- from + 2;
      match f statement with
      | Ok () -> statements (from + 2)
      | Error error -> Error error)
    else statements (from + 1)
  in
  let room from =
    match pending.stop < Bytes.length pending.data with
    | true -> from
    | false ->
        let kept = pending.stop - pending.start in
        let target =
          match pending.start with
          | 0 -> Bytes.create (2 * Bytes.length pending.data)
          | _ -> pending.data
        in
        let shift = pending.start in
        Bytes.blit pending.data pending.start target 0 kept;
        pending.data <- target;
        pending.start <- 0;
        pending.stop <- kept;
        from - shift
  in
  let rec next from =
    let from = room from in
    match
      read pending.data ~pos:pending.stop
        ~len:(Bytes.length pending.data - pending.stop)
    with
    | Error error -> Error error
    | Ok 0 -> (
        match
          String.trim
            (Bytes.sub_string pending.data pending.start
               (pending.stop - pending.start))
        with
        | "" -> Ok ()
        | rest -> Error (`Statements (Base.String.prefix rest 80)))
    | Ok length -> (
        pending.stop <- pending.stop + length;
        match statements from with
        | Ok from -> next from
        | Error error -> Error error)
  in
  next 0
