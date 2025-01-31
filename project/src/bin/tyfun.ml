open Minityfun

let execute_miniimp_from_file () =
  let filename = Sys.argv.(2) in
  let v, t = interpret_from_file filename in
  Printf.printf "Evaluation of %s produced %s of type %s \n" filename
    (TyFunAst.get_string_of_value v)
    (TyFunAst.get_string_of_type t)

let info_string =
  "To use executable tyfun pass one of the following pattern of arguments:\n\n\
   To interpret a MiniTyFun program written somewhere in a file and output its \
   resultto stdout use:\n\
   dune exec tyfun interpret_file input_file_path\n\n"

let () =
  let fail_helper =
   fun more_info ->
    Printf.printf "Failure Explanation: %s\n\n%s" more_info info_string;
    failwith "invalid arguments"
  in
  let num_arguments = Array.length Sys.argv in
  if num_arguments < 2 then
    fail_helper
      "You need to specify which functionality to use according as suggested \
       below"
  else
    let choice = Sys.argv.(1) in
    match choice with
    | "interpret_file" ->
        if num_arguments <> 3 then
          fail_helper "You need to specify an input file"
        else execute_miniimp_from_file ()
    | _ -> fail_helper "Functionality specified is not currently supported"
