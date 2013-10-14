(* Problem 06

   Find out whether a list is a palindrome. *)

open Simpletest

let is_palindrome xs = xs = List.rev xs

let _ =
  test (fun () -> assert (is_palindrome [`x; `a; `m; `a; `x]));
  test (fun () -> assert (not (is_palindrome [`a; `b])))
