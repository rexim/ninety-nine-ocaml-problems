(* Problem 10
 *
 * Run-length encoding of a list *)

(* Solution *)

let encode xs =
  let rec encode_aux xs current result =
    match (xs, current) with
    | ([], _) -> current :: result
    | (t1 :: ts, (c, t2)) -> if t1 = t2
                             then encode_aux ts (c + 1, t2) result
                             else encode_aux ts (1, t1) ((c, t2) :: result)
  in
  match xs with
  | [] -> []
  | t :: ts -> List.rev (encode_aux ts (1, t) []);;

(* Testing *)

let test_data =
  [`a;`a;`a;`a;`b;`c;`c;`a;`a;`d;`e;`e;`e;`e];;

let expected_result =
  [(4, `a); (1, `b);
   (2, `c); (2, `a);
   (1, `d); (4, `e)];;

assert (expected_result = encode test_data);;
