module type Consumer = sig
  type error

  val ship : Batch.sealed @ unique -> Batch_pool.t -> (unit, error) result
end

type 'e t = {
  pool : Batch_pool.t;
  ltypes : Ltype.t list;
  limit_bytes : int;
  consumer : (module Consumer with type error = 'e);
}

type next = Row | End

let create ~limit_bytes pool consumer ltypes =
  { pool; ltypes; limit_bytes; consumer }

let fill t produce =
  let { pool; ltypes; limit_bytes; _ } = t in
  let consume (type e) ({ consumer = (module Consumer); _ } : e t)
      (sealed : Batch.sealed @ unique) =
    Consumer.ship sealed pool
  in
  let ship batch =
    match Batch.seal batch with
    | Ok sealed -> consume t sealed
    | Error error -> Error error
  in
  let rec loop batch next_check =
    match
      let view = borrow_ batch in
      produce (Batch.columns view)
    with
    | Error error ->
        Batch_pool.give pool batch;
        Error error
    | Ok End -> (
        match Batch.rows (borrow_ batch) with
        | 0 ->
            Batch_pool.give pool batch;
            Ok ()
        | _ -> ship batch)
    | Ok Row -> (
        Batch.commit_row (borrow_ batch);
        let rows = Batch.rows (borrow_ batch) in
        if rows < next_check then loop batch next_check
        else
          let size = Batch.byte_size (borrow_ batch) in
          if size >= limit_bytes then
            match ship batch with
            | Ok () -> loop (Batch_pool.take pool ltypes) 1
            | Error error -> Error error
          else
            loop batch
              (rows
              + Int.max 1 ((limit_bytes - size) / (2 * Int.max 1 (size / rows)))
              ))
  in
  loop (Batch_pool.take pool ltypes) 1 [@nontail]
