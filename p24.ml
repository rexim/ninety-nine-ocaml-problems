(* Problem 24.
 *
 * Lotto: Draw N different random numbers from the set 1..M. *)

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

let range first last =
  let step = if first < last then 1 else (-1) in
  let rec range_aux i result =
    if i = last
    then (i :: result)
    else range_aux (i + step) (i :: result)
  in
  List.rev (range_aux first []);;

let lotto_select n m = rand_select (range 1 m) n;;

(* Testing *)

test (fun () ->
      let n = 6 in
      let m = 49 in
      let result = lotto_select n m in
      assert (List.length result = n);
      assert (List.for_all (fun x -> (1 <= x) && (x <= m)) result));;
