(* Problem 22.
 *
 * Create a list containing all integers within a given range.
 *
 * If first argument is smaller than second, produce a list in decreasing order. *)

open Simpletest;;

(* Solution *)

let range first last =
  let step = if first < last then 1 else (-1) in
  let rec range_aux i result =
    if i = last
    then (i :: result)
    else range_aux (i + step) (i :: result)
  in
  List.rev (range_aux first []);;
    
(* Testing *)

test "Problem 22"
     (fun () ->
      assert ([4; 5; 6; 7; 8; 9] = range 4 9);
      assert ([9; 8; 7; 6; 5; 4] = range 9 4);
      assert ([10] = range 10 10));;
