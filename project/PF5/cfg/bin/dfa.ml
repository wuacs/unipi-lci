open Cfg
open Miniimp
open Data_flow_analysis

let compute_live_analysis_and_create_dot () = 
  let channel = open_in Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./dot/" ^ filename ^ "-liveness.dot") in
      Printf.fprintf oc "%s" (live_analysis_dot (miniimp_cfg_to_minirisc (translate_miniimp prog)));
      close_out oc
  | None -> print_string "no good"

let compute_defined_analysis_and_create_dot ()  =
  let channel = open_in Sys.argv.(1) in
  let filename = Sys.argv.(2) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./dot/" ^ filename ^ "-defined.dot") in
      Printf.fprintf oc "%s" (defined_analysis_dot (miniimp_cfg_to_minirisc (translate_miniimp prog)));
      close_out oc
  | None -> print_string "no good"

let check_for_undefinedness () = 
  let channel = open_in Sys.argv.(1) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      print_string (match (Data_flow_analysis.check_for_undefinedness  (miniimp_cfg_to_minirisc (translate_miniimp prog))) with
      | false -> "No undefined variable\n"
      | true -> "Possible undefined variable\n")
  | None -> print_string "no good"

let () = check_for_undefinedness ()