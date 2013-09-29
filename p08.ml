(* Problem 08
 * 
 * Eliminate consecutive duplicates of list elements. *)

open Simpletest;;

(* My solution. *)

let rec compress xs =
  let rec drop_value x xs = 
    match xs with
    | []      -> []
    | y :: ys -> if x = y
                 then drop_value x ys
                 else xs
  in match xs with
     | []      -> []
     | y :: ys -> y :: compress (drop_value y xs);;

(* Testing *)

test (fun () ->
      let test_data =
        [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `e; `e; `e; `e]
      in
      let expected_result =
        [`a; `b; `c; `a; `d; `e]
      in
      assert(expected_result = compress test_data));;
