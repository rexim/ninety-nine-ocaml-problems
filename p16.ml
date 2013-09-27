(* Problem 16.
 *
 * Drop every N'th element from a list. *)

(* Solution *)

let drop xs n =
  let rec drop_aux xs i =
    match xs with
    | [] -> []
    | t :: ts -> if i = n
                 then drop_aux ts 1
                 else t :: drop_aux ts (i + 1)
  in drop_aux xs 1;;

(* Testing *)

let test_data =
  [`a; `b; `c; `d; `e; `f; `g; `h; `i; `j];;

let expected_result =
  [`a; `b; `d; `e; `g; `h; `j];;

assert(expected_result = drop test_data 3);;
