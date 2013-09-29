(* Problem 02
 * 
 * Find the last but one (last and penultimate) elements of a list *)

open Simpletest;;

let rec last_two xs =
  match xs with
  | [] | [_] -> None
  | [a; b]   -> Some (a, b)
  | _ :: ys  -> last_two ys;;

test (fun () ->
      assert(last_two [`a; `b; `c; `d] = Some (`c, `d));
      assert(last_two [`a] = None));;
