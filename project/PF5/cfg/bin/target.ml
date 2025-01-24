(*let generate_target_code () = 
  let channel = open_in Sys.argv.(1) in
  let namefile = Sys.argv.(2) in
  let reg_num = Sys.argv.(3) in
  match Miniimp.parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./target/"^ namefile ^ ".minirisc") in
      Printf.fprintf oc "%s" 
      (Target_code.generate_target_code_string (Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog)) (int_of_string reg_num));
      close_out oc
  | None -> print_string "no good"

let eval_target_code () = 
  let channel = open_in Sys.argv.(1) in
  let namefile = Sys.argv.(2) in
  let reg_num = Sys.argv.(3) in
  match Miniimp.parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./target/"^ namefile ^ ".out") in
      Printf.fprintf oc "%d" 
      (Target_code.eval (Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog)) (int_of_string reg_num) 4);
      close_out oc
  | None -> print_string "no good"

let () = eval_target_code ()*)