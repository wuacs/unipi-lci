open Miniimp
open Cfg
open Target_code

let default_number_of_registers = 4

let compile_minirisc_to_file () =
  let register_number_option = "registers=" in
  let undefinedness_option = "undefined_check" in
  (* Default values for optional arguments *)
  let reg_num = ref default_number_of_registers in
  let undefined_check = ref false in
  (* Parse optional arguments *)
  Array.iter
    (fun arg ->
      if String.starts_with ~prefix:register_number_option arg then
        reg_num :=
          int_of_string
            (String.sub arg
               (String.length register_number_option)
               (String.length arg - String.length register_number_option))
      else if arg = undefinedness_option then undefined_check := true)
    Sys.argv;
  let input_file = Sys.argv.(2) in
  let output_file = Sys.argv.(3) in
  generate_target_code_file input_file ~target_file_path:output_file
    ~register_number:!reg_num ~check_undefinedness:!undefined_check

let eval_target_code () =
  let input_file = Sys.argv.(2) in
  let input_channel = open_in input_file in
  Fun.protect
    ~finally:(fun _ -> close_in input_channel)
    (fun _ ->
      let input_value = Sys.argv.(3) in
      match parse_with_errors (Lexing.from_channel input_channel) with
      | Some prog ->
          Printf.printf "Value is: %d\n"
            (eval_risc_cfg
               (miniimp_cfg_to_minirisc (translate_miniimp prog))
               ~registers:10
               ~value:(int_of_string input_value))
      | None -> print_string "Error while parsing")

let info_string =
  "To generate target code for a MiniImp program, use the following command.\n\n\
   The following options are available: \n\n\
   `registers=x` forces an upperbound on the number of registers the target \
   code is allowed to use, e.g registers=4 will use 4 registers\n\
   (R0, R1, R2, R3).\n\
   Specifying a value under 4 results in a program crash.\n\
   When left unspecified, the value is assumed 4. \n\n\
   `undefined_check` to enable check for undefined variables in the MiniImp \
   program, if such a variable is found the compilation fails.\n\n\
   dune exec target build input_file_path output_file_path [registers=x] \
   [undefined_check]\n\n\n\
   To evaluate target code for a MiniImp program with a chosen input value  \n\
   and output to stdout a string representation of its output, use:\n\
   (The number of registers used is 4 and no undefined check is done)\n\n\
   dune exec target eval input_file_path x\n\n"

let () =
  let fail_helper ?(additional_info = "Unspecified error") () =
    Printf.printf "%s" info_string;
    failwith (Printf.sprintf "Invalid arguments: %s" additional_info)
  in
  if Array.length Sys.argv < 2 then
    fail_helper ~additional_info:"Command not specified" ();
  match Sys.argv.(1) with
  | "build" ->
      if Array.length Sys.argv > 5 then
        fail_helper ~additional_info:"Build takes at most 4 parameters" ()
      else compile_minirisc_to_file ()
  | "eval" ->
      if Array.length Sys.argv <> 4 then
        fail_helper ~additional_info:"Evaluation takes 2 parameters" ()
      else eval_target_code ()
  | _ -> fail_helper ()
