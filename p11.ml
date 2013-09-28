(* Problem 11
 *
 * Modified run-length encoding.
 *
 * Modify the result of the previous problem in such a way that if an
 * element has no duplicates it is simply copied into the result
 * list. Only elements with duplicates are transferred as (N E) lists. *)

open Simpletest;;

(* Solution *)

type 'a rle =
  | One of 'a
  | Many of (int * 'a);;

let encode xs =
  let rec encode_aux xs current result =
    match (xs, current) with
    | ([], _) -> current :: result
    | (t1 :: ts, One t2) -> if t1 = t2
                            then encode_aux ts (Many (2, t2)) result
                            else encode_aux ts (One t1) ((One t2) :: result)
    | (t1 :: ts, Many (c, t2)) -> if t1 = t2
                                  then encode_aux ts (Many (c + 1, t2)) result
                                  else encode_aux ts (One t1) ((Many (c, t2)) :: result)
  in
  match xs with
  | [] -> []
  | t :: ts -> List.rev (encode_aux ts (One t) []);;

(* Testing *)

test "Problem 11"
     (fun () ->
      let test_data =
        [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e]
      in
      let expected_result =
        [Many (4, `a); One `b;
         Many (2, `c); Many (2, `a);
         One `d; Many (4, `e)]
      in
      assert (expected_result = encode test_data);
      assert ([] = encode []);
      assert ([One `a] = encode [`a]);
      assert ([Many (2, `b)] = encode [`b; `b]));;
