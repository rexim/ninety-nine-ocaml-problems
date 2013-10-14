(* Problem 18.

   Extract a slice from a list.

   Given two indices, `i` and `k`, the slice is the list containing
   the elements between the i'th abnd k'th elements of the original
   list (both limits included). Start counting the elements with 0
   (this is way the `List` module numbers elements. *)

open Simpletest

(* Solution *)

let slice xs i k =
  let rec drop xs n =
    match xs with
    | [] -> []
    | _ :: ts -> if n = 0
                 then xs
                 else drop ts (n - 1)
  in
  let rec take xs n =
    match xs with
    | [] -> []
    | t :: ts -> if n = 0
                 then []
                 else t :: take ts (n - 1)
  in
  take (drop xs i) (k - i + 1)

(* Testing *)

let _ =
  test (fun () ->
        let xs = [`a;`b;`c;`d;`e;`f;`g;`h;`i;`j] in
        let i = 2 in
        let k = 6 in
        let expected = [`c; `d; `e; `f; `g] in
        assert (expected = slice xs i k))
