(* Problem 01
 *
 * Write a function `last : 'a list -> 'a option` that returns the last
 * element of a list. *)

open Simpletest;;

let rec last xs =
  match xs with
  | []      -> None
  | [x]     -> Some x
  | _ :: ys -> last ys;;

test "Problem 01"
     (fun () ->
      assert(Some `d = last [`a ; `b ; `c ; `d]);
      assert(None = last []));;
