(* Problem 15.

   Replicate the elements of a list a given number of lines. *)

open Simpletest

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
  in List.rev (replicate_aux xs n [])

(* Testing *)

let _ =
  test (fun () ->
        let test_data =
          [`a; `b; `c]
        in
        let expected_result =
          [`a; `a; `a; `b; `b; `b; `c; `c; `c]
        in
        assert (expected_result = replicate test_data 3))
