type delivery = Delivered | Closed

(** Whether messages are still accepted. *)
type state = Open | Shut

type 'a contents = { messages : 'a Queue.t; mutable state : state }

type ('a : value mod contended portable) t = {
  contents : 'a contents Await.Capsule.Await.With_mutex.t;
  ready : Await.Await.Semaphore.t;
      (** One permit per message queued, and one more once closed. *)
}

let create () =
  {
    contents =
      Await.Capsule.Await.With_mutex.create (fun () ->
          { messages = Queue.create (); state = Open });
    ready = Await.Await.Semaphore.create 0;
  }

let add (type (a : value mod contended portable)) (t : a t) (message : a) =
  match
    Guarded.hold t.contents (fun contents ->
        match contents.state with
        | Shut -> Closed
        | Open ->
            Queue.push message contents.messages;
            Delivered)
  with
  | Closed -> Closed
  | Delivered ->
      Await.Await.Semaphore.release t.ready;
      Delivered

let take (type (a : value mod contended portable)) (t : a t) : a option =
  Await.Await.Semaphore.acquire Blocking.await t.ready;
  match Guarded.hold t.contents (fun contents -> Queue.take_opt contents.messages) with
  | Some message -> Some message
  | None ->
      Await.Await.Semaphore.release t.ready;
      None

let close t =
  match
    Guarded.hold t.contents (fun contents ->
        let was = contents.state in
        contents.state <- Shut;
        was)
  with
  | Open -> Await.Await.Semaphore.release t.ready
  | Shut -> ()
