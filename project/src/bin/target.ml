open Miniimp
open Cfg
open Target_code

let compile_minirisc_to_file () =
  let register_number_option = "registers=" in
  let undefinedness_option = "undefined_check" in
  (* Default values for optional arguments *)
  let reg_num = ref 4 in
  let undefined_check = ref false in  
  (* Parse optional arguments *)
  Array.iter (fun arg ->
    if String.starts_with ~prefix:register_number_option arg then
      reg_num := int_of_string (String.sub arg (String.length register_number_option) (String.length arg - (String.length register_number_option)))
    else if arg = undefinedness_option then
      undefined_check := true
  ) Sys.argv;
  let input_file = Sys.argv.(2) in
  let output_file = Sys.argv.(3) in
  generate_target_code_file input_file ~target_file_path:output_file ~register_number:(!reg_num) ~check_undefinedness:(!undefined_check) 

let eval_target_code () =
  let input_channel = open_in Sys.argv.(2) in
  Fun.protect ~finally:(fun _ -> close_in input_channel) (
    fun _ ->
      let output_file = Sys.argv.(3) in
      let input_value = Sys.argv.(4) in
      let reg_num = Sys.argv.(5) in      
      match parse_with_errors (Lexing.from_channel input_channel) with
      | Some prog ->
          let oc = open_out output_file in
          Fun.protect ~finally:(fun _ -> close_out oc)
          (fun _ ->
            Printf.fprintf oc "%d"
              (Target_code.eval_risc_cfg
                (miniimp_cfg_to_minirisc (translate_miniimp prog))
                ~registers:(int_of_string reg_num) ~value:(int_of_string input_value)))
      | None -> print_string "Error while parsing"
  )

let info_string =
"To generate target code for a MiniImp program, use the following command.\n
The following options are available: \n
registers=x a number of registers you want your target code to use, e.g -registers=4 will use 4 registers
(R0, R1, R2, R3).
Default value is 4. \n
undefined_check to enable check for undefined variables in the MiniImp program, if such a variable is found the compilation fails.\n
dune exec imp_interpreter build input_file_path output_file_path\n\n
To evaluate target code for a MiniImp program, use:\n
dune exec imp_interpreter eval input_file_path output_file_path input_val reg_num\n\n"

let () =
  let fail_helper ?(additional_info = "Unspecified error") () =
    Printf.printf "%s" info_string;
    failwith (Printf.sprintf "Invalid arguments: %s" additional_info) 
  in
  if Array.length Sys.argv < 2 then fail_helper ~additional_info:"Command not specified" ();
  match Sys.argv.(1) with
  | "build" -> if Array.length Sys.argv > 5 then fail_helper ~additional_info:"Build takes at most 5 parameters" () else compile_minirisc_to_file ()
  | "eval" -> if Array.length Sys.argv <> 6 then fail_helper  ~additional_info:"Evaluation takes 6 parameters" () else eval_target_code ()
  | _ -> fail_helper ()
