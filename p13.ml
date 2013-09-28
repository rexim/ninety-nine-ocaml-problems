(* Problem 13
 *
 * Run-length encoding of a list (direct solution).
 *
 * Implement the so-called run-length encoding data compression method
 * directly. I.e. don't explicitly create the sublists containing the
 * duplicates, as in problem "Pack consecutive duplicates of list
 * elements into sublists", but only count them. As in problem "Modified
 * run-length encoding", simplify the result list by replacing the
 * singleton lists (1 X) by X. *)

(* What?! I've already solved this problem that way! See my solution
 * of problem 11. I'll just copy it here. *)

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

test "Problem 13"
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
