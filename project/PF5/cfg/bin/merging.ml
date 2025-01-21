let translate_and_merge () = 
  let channel = open_in Sys.argv.(1) in
  let namefile = Sys.argv.(2) in
  let reg_num = Sys.argv.(3) in
  match Miniimp.parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./dot/"^ namefile ^ "-merged.dot") in
      let (optimized_cfg, _, _) = (Target_code.chaitin_briggs_algorithm 
      (Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog)) (int_of_string reg_num)) in
      Printf.fprintf oc "%s" 
      (Cfg.minirisc_cfg_to_dot
      (optimized_cfg));
      close_out oc
  | None -> print_string "no good"
(*
let print_to_dot_interference_graph () = 
  let channel = open_in Sys.argv.(1) in
  let namefile = Sys.argv.(2) in
  match Miniimp.parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./dot/"^ namefile ^ "-interference.dot") in
      Printf.fprintf oc "%s" 
      (Target_code.get_live_ranges_dot_format (Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog)));
      close_out oc
  | None -> print_string "no good"
*)
let () = translate_and_merge()