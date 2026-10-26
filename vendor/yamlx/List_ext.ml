(** Tail-recursive replacements for [List] functions that are not tail-recursive
    in OCaml 4.14 and can stack-overflow on large inputs.

    [List.rev_map] and [List.rev] are both tail-recursive in every OCaml
    version, so [map f l = List.rev (List.rev_map f l)] is safe regardless of
    list length. *)

let map f l = List.rev (List.rev_map f l)

let filter f l =
  List.rev (List.fold_left (fun acc x -> if f x then x :: acc else acc) [] l)

let filter_map f l =
  List.rev
    (List.fold_left
       (fun acc x ->
         match f x with
         | Some y -> y :: acc
         | None -> acc)
       [] l)
