(* Problem 06
 * 
 * Find out whether a list is a palindrome. 
 *)

let is_palindrome xs = xs = List.rev xs ;;
assert (is_palindrome [`x; `a; `m; `a; `x]) ;;
assert (not (is_palindrome [`a; `b])) ;;
