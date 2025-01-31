open Miniimp

let execute_miniimp_from_file () =
  let input_channel = open_in Sys.argv.(2) in
  Fun.protect
    ~finally:(fun _ -> close_in input_channel)
    (fun _ ->
      let value_in_input = Sys.argv.(3) in
      match parse_with_errors (Lexing.from_channel input_channel) with
      | Some prog -> (
          let res = eval prog (int_of_string value_in_input) in
          match res with
          | Some v -> print_int v
          | None -> print_string "Evaluation failed")
      | None -> print_string "Error while parsing")

let info_string =
  "To use executable imp_interpreter pass one of the following pattern of \
   arguments:\n\n\n\
   To execute a MiniImp program written in some file and output its result\n\
   on stdout, instead, use the following:\n\n\n\
   dune exec imp_interpreter execute input_file_path integer_in_input\n\n"

let () =
  let fail_helper =
   fun _ ->
    Printf.printf "%s" info_string;
    failwith "invalid arguments"
  in
  let choice = Sys.argv.(1) in
  match choice with
  | "execute" ->
      if Array.length Sys.argv <> 4 then fail_helper ()
      else execute_miniimp_from_file ()
  | _ -> fail_helper ()
