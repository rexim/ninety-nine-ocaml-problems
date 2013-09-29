(* Problem 17.
 *
 * Split a list into two parts; the length of the first part
 * is given.
 *
 * If the length of the first part is longer the the entire
 * list, then the first part is the list and the second part
 * is empty. *)

open Simpletest;;

(* Solution *)

let split xs n =
  let rec split_aux xs n result =
    match xs with
    | [] -> (List.rev result, xs)
    | t :: ts -> if n = 0
                 then (List.rev result, xs)
                 else split_aux ts (n - 1) (t :: result)
  in split_aux xs n [];;

(* Testing *)

let test1 () =
  let xs = [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] in
  let n = 3 in
  let expected = ([`a; `b; `c],
                  [`d; `e; `f; `g; `h; `i; `j]) in
  assert (expected = split xs n);;

let test2 () =
  let xs = [`a;`b;`c;`d] in
  let n = 5 in
  let expected = ([`a; `b; `c; `d], []) in
  assert (expected = split xs n);;

test (fun () ->
      test1 ();
      test2 ());;
