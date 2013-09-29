(* Problem 19.
 *
 * Rotate a list N places to the left.*)

open Simpletest;;

(* Solution *)

let rec take n xs =
  match xs with
  | [] -> []
  | t :: ts -> if n <= 0 then [] else t :: take (n - 1) ts;;

let rec drop n xs =
  match xs with
  | [] -> []
  | _ :: ts -> if n <= 0 then xs else drop (n - 1) ts;;

let rotate xs n =
  match xs with
  | [] -> []
  | _  -> let m = List.length xs in
          let s = ((n mod m) + m) mod m in
          let left = take s xs in
          let right = drop s xs in
          List.concat [right; left];;

(* Testing *)

test (fun () ->
      assert (["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"] = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3);
      assert (["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"] = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2));
      assert (["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 8);
      assert (["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] = rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-8));
      assert ([] = rotate [] 100));;
