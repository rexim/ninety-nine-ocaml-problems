(* Problem 23.
 *
 * Extract a given number of randomly selected elements from a list.
 *
 * The selected items shall be returned in a list. We use the `Random`
 * module but do not initialize it with `Random.self_init` for
 * reproducibility. *)

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

(* Testing *)

test (fun () ->
      (* NOTE(rexim): was tested on OCaml 3.12.1. May not work on
      other versions due to differences between implementations of
      Random.int. *)
      assert ([`g; `d; `a] = rand_select [`a;`b;`c;`d;`e;`f;`g;`h] 3));;
