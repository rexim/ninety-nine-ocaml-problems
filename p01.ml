(* Problem 01

   Write a function `last : 'a list -> 'a option` that returns the
   last element of a list. *)

open Simpletest

(* Solution *)

let rec last xs =
  match xs with
  | []      -> None
  | [x]     -> Some x
  | _ :: ys -> last ys

(* Testing *)

let _ =
  test (fun () -> assert (Some `d = last [`a ; `b ; `c ; `d]));
  test (fun () -> assert (None = last []))
