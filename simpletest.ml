open Printf;;

let test test_name test_function =
  try
    print_endline test_name;
    test_function ()
  with
  | Assert_failure (filename, row, column) ->
     printf "%s:%d:%d: assert failure\n" filename row column;;
