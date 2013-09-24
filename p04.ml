(* Problem 04
 *
 * Find the number of elements of a list.
 *
 * OCaml standard library has `List.length` but we ask that you
 * reimplement it. Bonus for a tail recursive solution *)

let rec length xs =
  match xs with
  | []      -> 0
  | _ :: ys -> 1 + length ys;;

assert (length [`a; `b; `c] = 3);;
assert (length [] = 0);;

(* Got the bonus :3 *)
let length_tail_recursive xs =
  let rec length_impl acc xs =
    match xs with
    | []      -> acc
    | _ :: ys -> length_impl (acc + 1) ys
  in length_impl 0 xs;;

assert (length_tail_recursive [`a; `b; `c] = 3);;
assert (length_tail_recursive [] = 0);;
