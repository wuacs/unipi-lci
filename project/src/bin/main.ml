open Cfg
open Miniimp

(*let print_first_instruction () = 
  let channel = open_in Sys.argv.(1) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog -> print_string (match (List.nth (Cfg.NodeMap.find (Cfg.Label 0) (Cfg.translate_miniimp prog).code) 0) with
  | Skip -> "SKIP"
  | _ -> "other")
  | None -> print_string "error"*)

let translate_miniimp_to_dot_minirisc () = 
  let channel = open_in Sys.argv.(1) in
  let namefile = Sys.argv.(2) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./dot/"^ namefile ^ "-minirisc.dot") in
      Printf.fprintf oc "%s" (minirisc_cfg_to_dot (miniimp_cfg_to_minirisc (translate_miniimp prog)));
      close_out oc
  | None -> print_string "no good"

(*
let translate_fibonacci_miniimp_cfg_to_minirisc () = 
  let channel = open_in Sys.argv.(1) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out "./dot/minirisc-fibonacci.dot" in
      Printf.fprintf oc "%s" (minirisc_cfg_to_dot (miniimp_cfg_to_minirisc (translate_miniimp prog)));
      close_out oc
  | None -> print_string "no good"
*)

(*
let compute_live_analysis_and_create_dot () = 
  let channel = open_in Sys.argv.(1) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out "./dot/liveness.dot" in
      Printf.fprintf oc "%s" (live_analysis_dot (miniimp_cfg_to_minirisc (translate_miniimp prog)));
      close_out oc
  | None -> print_string "no good"
*)

let () = translate_miniimp_to_dot_minirisc()



(*let translate_fibonacci_miniimp_cfg () = 
  let channel = open_in Sys.argv.(1) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out "./dot/miniimp-fibonacci.dot" in
      Printf.fprintf oc "%s" (miniimp_cfg_to_dot (translate_miniimp prog));
      close_out oc
  | None -> print_string "no good"*)

