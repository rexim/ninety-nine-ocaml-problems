(* Problem 04

   Find the number of elements of a list.

   OCaml standard library has `List.length` but we ask that you
   reimplement it. Bonus for a tail recursive solution *)

open Simpletest

(* Solution *)

let rec length xs =
  match xs with
  | []      -> 0
  | _ :: ys -> 1 + length ys

let length_tail_recursive xs =
  let rec length_impl acc xs =
    match xs with
    | []      -> acc
    | _ :: ys -> length_impl (acc + 1) ys
  in length_impl 0 xs

(* Testing *)

let _ =
  test (fun () -> assert (length [`a; `b; `c] = 3));
  test (fun () -> assert (length [] = 0));
  test (fun () -> assert (length_tail_recursive [`a; `b; `c] = 3));
  test (fun () -> assert (length_tail_recursive [] = 0))
