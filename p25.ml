(* Problem 25.
 *
 * Generate a random permutatino of the elements of a list *)

open Simpletest;;

(* Solution *)

let rec take_at n xs =
  match xs with
  | []      -> raise Not_found
  | t :: ts -> if n <= 0 then t else take_at (n - 1) ts;;

let rec remove_at n xs =
  match xs with
  | []      -> []
  | t :: ts -> if n <= 0 then ts else t :: remove_at (n - 1) ts;;

let rand_select xs n =
  let rec rand_select_aux xs n result =
    if n = 0
    then result
    else let len = List.length xs in
         let i = Random.int len in
         rand_select_aux (remove_at i xs)
                         (n - 1)
                         ((take_at i xs) :: result)
  in rand_select_aux xs n [];;

let permutation xs =
  rand_select xs (List.length xs);;

(* Testing *)

test (fun () ->
      let xs = [`a; `b; `c; `d; `e; `f] in
      let result = permutation xs in
      assert (List.sort Pervasives.compare result = List.sort Pervasives.compare xs));;
