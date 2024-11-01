open Miniimpinterpreter

let () = 
  match parse_with_errors (Lexing.from_channel stdin) with
  | Some(x) -> (match eval x (int_of_string Sys.argv.(1)) with | Some(y) -> print_int y | _ -> print_string "error in evaluation")
  | None -> print_string "error in parsing"