(* Problem 08
 * 
 * Eliminate consecutive duplicates of list elements. *)

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

(* Solution from the link from the README.  *)

let rec compress_improved xs =
  match xs with
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | x -> x;;

(* Testing *)

let test_data =
  [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `e; `e; `e; `e];;

let expected_result =
  [`a; `b; `c; `a; `d; `e];;

assert(expected_result = compress test_data);;
assert(expected_result = compress_improved test_data);;
