(* Problem 03

   Find the k'th element of a list. *)

open Simpletest

(* Solution *)

let rec at i xs =
  if i > 0
  then match xs with
       | []      -> None
       | y :: ys -> if i = 1
                    then Some y
                    else at (i - 1) ys
  else None

(* Testing *)

let _ =
  test (fun () -> assert (at 3 [`a; `b; `c; `d; `e] = Some `c));
  test (fun () -> assert (at 3 [`a] = None))
