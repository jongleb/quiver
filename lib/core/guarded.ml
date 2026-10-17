let create (type a) (type (e : value mod contended portable))
    (build : unit -> (a, e) result) : (a Await.Capsule.Await.With_mutex.t, e) result =
  let (P mutex) = Await.Capsule.Await.Mutex.create () in
  match
    Await.Capsule.Await.Mutex.with_lock Blocking.await mutex ~f:(fun access ->
        match build () with
        | Ok value -> Ok (Await.Capsule.Data.wrap ~access value)
        | Error error -> Error error)
  with
  | Ok data -> Ok (Await.Capsule.Await.With_mutex.P { data; mutex })
  | Error error -> Error error

let hold guarded f =
  Await.Capsule.Await.With_mutex.with_lock Blocking.await guarded ~f
