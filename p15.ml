(* Problem 15.
 *
 * Replicate the elements of a list a given number of lines. *)

(* Solution *)

let replicate xs n =
  let rec clone_element x n result =
    if n = 0
    then result
    else clone_element x (n - 1) (x :: result)
  in 
  let rec replicate_aux xs n result =
    match xs with
    | [] -> result
    | t :: ts -> replicate_aux ts n (clone_element t n result)
  in List.rev (replicate_aux xs n []);;

(* Testing *)

let test_data =
  [`a; `b; `c];;

let expected_result =
  [`a; `a; `a; `b; `b; `b; `c; `c; `c];;

assert (expected_result = replicate test_data 3);;
