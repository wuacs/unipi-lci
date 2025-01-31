open Minifun

let staticly_evaluated = Value (Integer 5)

let execute_static () =
  Printf.printf "Output: %s\n"
    (string_of_value (eval_fun_ast staticly_evaluated))

let info_string =
  "This executable will evaluate the MiniFun program,\n\
   more precisely its AST, which is given in the variable\n\
   staticly_evaluated defined in this executable and prints the Minifun.value\n\
   in a string format as given by Minifun.string_of_value \n\n\n\
   To use simply execute\n\n\n\
   dune exec untyfun static\n\n"

let () =
  let fail_helper =
   fun _ ->
    Printf.printf "%s" info_string;
    failwith "invalid arguments"
  in
  if Array.length Sys.argv <= 1 then fail_helper ()
  else
    let choice = Sys.argv.(1) in
    match choice with "static" -> execute_static () | _ -> ()
