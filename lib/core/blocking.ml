let await =
  Await.Await.Expert.create ~sync:Await.Sync.blocking
    ~terminator:(Await.Terminator.Expert.create ())

let sockets =
  Base.Portable_lazy.from_fun (fun () ->
      Sys.Safe.set_signal Sys.sigpipe Signal_ignore)

type direction = Readable | Writable
type readiness = Ready | Timed_out

(** How one poll ended; built by [quiver_poll]. *)
type outcome = Became_ready | Expired | Interrupted [@@warning "-37"]

external poll : Unix.file_descr -> direction -> int -> outcome @@ portable
  = "quiver_poll"

let wait ?seconds direction fd =
  let milliseconds =
    Option.fold ~none:(-1)
      ~some:(fun seconds -> Float.to_int (seconds *. 1000.))
      seconds
  in
  let rec go () =
    match poll fd direction milliseconds with
    | Became_ready -> Ready
    | Expired -> Timed_out
    | Interrupted -> go ()
  in
  go ()
