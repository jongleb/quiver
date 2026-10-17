type 'a t = 'a Unique.Lockfree_stack.t

let create () = Unique.Lockfree_stack.create ()

let take t = Handoff.own (Unique.Lockfree_stack.pop_or_null t)
let put t value = Unique.Lockfree_stack.push t value
