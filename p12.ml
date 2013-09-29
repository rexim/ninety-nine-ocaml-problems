(* Problem 12
 *
 * Decode a run-length encoded list.
 *
 * Given a run-length code list generated as specified in the previous
 * problem, construct its uncompressed version *)

open Simpletest;;

(* Solution *)

type 'a rle =
  | One of 'a
  | Many of (int * 'a);;

let decode xs =
  let rec decode_node x result =
    match x with
    | One t -> t :: result
    | Many (2, t) -> decode_node (One t) (t :: result)
    | Many (c, t) -> decode_node (Many (c - 1, t)) (t :: result)
  in
  let rec decode_aux xs result =
    match xs with
    | [] -> result
    | t :: ts -> decode_aux ts (decode_node t result)
  in List.rev (decode_aux xs []);;

(* Testing *)

test (fun () ->
      let test_data =
        [Many (4,`a); One `b; Many (2,`c); Many (2,`a); One `d; Many (4,`e)]
      in
      let expected_result =
        [`a; `a; `a; `a; `b; `c; `c; `a; `a; `d; `e; `e; `e; `e]
      in
      assert (expected_result = decode test_data);
      assert ([] = decode []));;
