(* Problem 21.

   Insert an element at a given position into a list.*)

open Simpletest

(* Solution *)

let rec insert_at x n xs =
  match xs with
  | [] -> []
  | t :: ts -> if n <= 0
               then x :: xs
               else t :: insert_at x (n - 1) ts

(* Testing *)

let _ =
  test (fun () ->
        assert ([`a; `alfa; `b; `c; `d] = insert_at `alfa 1 [`a; `b; `c; `d]))
