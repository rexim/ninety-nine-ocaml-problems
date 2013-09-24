(* Problem 07
 * 
 * Flatten a nested list structure *)

(* There is no nested list type in OCaml, so we need to define one
 * first. A node of a nested list is either an element, or a list of
 * nodes. *)
type 'a node =
  | One of 'a
  | Many of 'a node list;;

let flatten nodes =
  let rec flatten nodes result = 
    match nodes with
    | One x   :: restNodes -> flatten restNodes (x :: result)
    | Many xs :: restNodes -> flatten restNodes (flatten xs result)
    | []                   -> result
  in List.rev (flatten nodes []);;

(* Testing *)

let test_data =
  [ One `a ; Many [ One `b ; Many [ One `c ; One `d ] ; One `e ] ];;

let expected_result =
  [`a; `b; `c; `d; `e];;

assert(flatten test_data = expected_result);;
