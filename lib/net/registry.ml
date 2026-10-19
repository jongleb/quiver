(** Whether a registry still waits for values. *)
type state = Open | Closed

type ('a : value mod contended portable) t = {
  slots : (int, 'a option Await.Await.Ivar.t) Portable_lockfree_htbl.t;
  state : state Portable.Atomic.t;
}

let create () =
  {
    slots = Portable_lockfree_htbl.create (module Base.Int);
    state = Portable.Atomic.make Open;
  }

let slot t number =
  let fresh = Await.Await.Ivar.create () in
  match Portable_lockfree_htbl.add t.slots ~key:number ~data:fresh with
  | Null -> fresh
  | This existing -> existing

let publish t number value =
  Await.Await.Ivar.fill_if_empty (slot t number) (Some value)

let await t number =
  let slot = slot t number in
  (match Portable.Atomic.get t.state with
  | Closed -> Await.Await.Ivar.fill_if_empty slot None
  | Open -> ());
  Await.Await.Ivar.read Quiver_core.Blocking.await slot

let forget t number = ignore (Portable_lockfree_htbl.remove t.slots number)

let close t =
  Portable.Atomic.set t.state Closed;
  List.iter
    (fun (_, slot) -> Await.Await.Ivar.fill_if_empty slot None)
    (Portable_lockfree_htbl.to_alist t.slots)
