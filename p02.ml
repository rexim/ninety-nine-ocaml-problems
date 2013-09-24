(* Problem 02
 * 
 * Find the last but one (last and penultimate) elements of a list *)

let rec last_two xs =
  match xs with
  | []      -> None
  | [_]     -> None
  | [a; b]  -> Some (a, b)
  | _ :: ys -> last_two ys;;

assert(last_two [`a; `b; `c; `d] = Some (`c, `d));;
assert(last_two [`a] = None);;
