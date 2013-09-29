open Printf;;

let test test_function =
  try
    test_function ()
  with
  | Assert_failure (filename, row, column) ->
     printf "%s:%d:%d: assert failure\n" filename row column;;
