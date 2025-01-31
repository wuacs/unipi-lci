open Cfg
open Miniimp
open Data_flow_analysis

let check_for_undefinedness () =
  let ic = open_in Sys.argv.(2) in
  Fun.protect
    ~finally:(fun _ -> close_in ic)
    (fun _ ->
      match parse_with_errors (Lexing.from_channel ic) with
      | Some prog ->
          let result =
            check_for_undefinedness
              (miniimp_cfg_to_minirisc (translate_miniimp prog))
          in
          print_string
            (if result then "Possible undefined variable\n"
             else "No undefined variable\n")
      | None -> print_string "Error: Parsing failed\n")

let info_string =
  "Usage instructions:\n\n\n\
  \   To check for undefined variables in a MiniImp program and output\n\
  \   whether one such variable was found use:\n\
   dune exec dfa check_undefined input_file_path\n\n"

let () =
  let fail_helper =
   fun more_info ->
    Printf.printf "Failure Explanation: %s\n\n%s" more_info info_string;
    failwith "invalid arguments"
  in
  if Array.length Sys.argv <= 1 then fail_helper "No command specified"
  else
    let command = Sys.argv.(1) in
    match command with
    | "check_undefined" ->
        if Array.length Sys.argv <> 3 then
          fail_helper "Usage: check_undefined input_file_path"
        else check_for_undefinedness ()
    | _ -> fail_helper "Invalid command"
