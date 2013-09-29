(* Problem 20.
 *
 * Remove the K'th element from a list. *)

open Simpletest;;

(* Solution *)

let rec remove_at n xs =
  match xs with
  | [] -> []
  | t :: ts -> if n <= 0 then ts else t :: remove_at (n - 1) ts;;

(* Testing *)

test (fun () ->
      assert ([`a; `c; `d] = remove_at 1 [`a; `b; `c; `d]));;
