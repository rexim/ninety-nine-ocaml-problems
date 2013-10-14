(* Problem 14.

   Duplicate the elements of a list. *)

open Simpletest

let rec duplicate xs =
  match xs with
  | [] -> []
  | t :: ts -> t :: t :: duplicate ts

let _ =
  test (fun () ->
        assert([`a; `a; `b; `b; `c; `c; `c; `c] = duplicate [`a; `b; `c; `c]))
