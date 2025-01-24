open Cfg
open Miniimp
open Data_flow_analysis

let compute_live_analysis_and_create_dot () = 
  let channel = open_in Sys.argv.(2) in
  let filename = Sys.argv.(3) in
  Fun.protect ~finally:(fun _ -> close_in channel) (fun _ ->
    match parse_with_errors (Lexing.from_channel channel) with
    | Some prog ->
        let oc = open_out filename in
        Fun.protect ~finally:(fun _ -> close_out oc) (fun _ ->
          Printf.fprintf oc "%s" (live_analysis_dot (miniimp_cfg_to_minirisc (translate_miniimp prog)))
        )
    | None -> print_string "Error: Parsing failed\n"
  )

let compute_defined_analysis_and_create_dot () =
  let ic = open_in Sys.argv.(2) in
  let out_file_name = Sys.argv.(3) in
  Fun.protect ~finally:(fun _ -> close_in ic) (fun _ ->
    match parse_with_errors (Lexing.from_channel ic) with
    | Some prog ->
        let oc = open_out out_file_name in
        Fun.protect ~finally:(fun _ -> close_out oc) (fun _ ->
          Printf.fprintf oc "%s" (defined_analysis_dot (miniimp_cfg_to_minirisc (translate_miniimp prog)))
        )
    | None -> print_string "Error: Parsing failed\n"
  )

let check_for_undefinedness () =
  let ic = open_in Sys.argv.(2) in
  Fun.protect ~finally:(fun _ -> close_in ic) (fun _ ->
    match parse_with_errors (Lexing.from_channel ic) with
    | Some prog ->
        let result = check_for_undefinedness (miniimp_cfg_to_minirisc (translate_miniimp prog)) in
        print_string (if result then "Possible undefined variable\n" else "No undefined variable\n")
    | None -> print_string "Error: Parsing failed\n"
  )

let info_string =
  "Usage instructions:\n\n\
   To compute live analysis and create a DOT file:\n\
   dune exec dfa live_analysis input_file_path output_file_path\n\n\
   To compute defined analysis and create a DOT file:\n\
   dune exec dfa defined_analysis input_file_path output_file_path\n\n\
   To check for undefined variables:\n\
   dune exec dfa check_undefined input_file_path\n\n"

let () =
  let fail_helper = (fun more_info -> 
    Printf.printf "Failure Explanation: %s\n\n%s" more_info info_string;
    failwith "invalid arguments")
  in
  if (Array.length Sys.argv) <= 1 then fail_helper "No command specified" else
  let command = Sys.argv.(1) in
  match command with
  | "live_analysis" ->
      if Array.length Sys.argv <> 4 then fail_helper "Usage: live_analysis input_file_path output_file_prefix"
      else compute_live_analysis_and_create_dot ()
  | "defined_analysis" ->
      if Array.length Sys.argv <> 4 then fail_helper "Usage: defined_analysis input_file_path output_file_prefix"
      else compute_defined_analysis_and_create_dot ()
  | "check_undefined" ->
      if Array.length Sys.argv <> 3 then fail_helper "Usage: check_undefined input_file_path"
      else check_for_undefinedness ()
  | _ ->
      fail_helper "Invalid command"
