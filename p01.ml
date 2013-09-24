(* Problem 01
 *
 * Write a function `last : 'a list -> 'a option` that returns the last
 * element of a list. *)

let rec last xs =
  match xs with
  | []      -> None
  | [x]     -> Some x
  | _ :: ys -> last ys;;


assert(last [ `a ; `b ; `c ; `d ] = Some `d);;
assert(last [] = None);;
