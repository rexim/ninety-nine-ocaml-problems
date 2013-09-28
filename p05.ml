(* Problem 05
 *
 * Reverse a list.
 * 
 * OCaml standard library has `List.rev` but we ask that you reimplement
 * it.
 *)

open Simpletest;;

let rev xs =
  let rec rev_impl xs rs = 
    match xs with
    | []      -> rs
    | y :: ys -> rev_impl ys (y :: rs) 
  in rev_impl xs [];;

test "Problem 05"
     (fun () ->
      assert ([`c; `b; `a] = rev [`a; `b; `c]);
      assert ([] = rev []));;
