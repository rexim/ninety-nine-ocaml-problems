(* Problem 09
 *
 * Pack consecutive duplicates of list elements into sublists. *)

open Simpletest;;

(* Solution *)

let pack xs =
  let rec pack_aux xs current result =
    match xs with
    | [] -> []
    | [t] -> (t :: current) :: result
    | t1 :: t2 :: ts -> if t1 = t2
                        then pack_aux (t2 :: ts) (t1 :: current) result
                        else pack_aux (t2 :: ts) [] ((t1 :: current) :: result)
  in List.rev (pack_aux xs [] []);;

(* Testing *)

test (fun () ->
      let test_data =
        [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`d;`e;`e;`e;`e]
      in
      let expected_result =
        [[`a; `a; `a; `a];
         [`b];
         [`c; `c];
         [`a; `a];
         [`d; `d];
         [`e; `e; `e; `e]]
      in
      assert (expected_result = pack test_data));;
