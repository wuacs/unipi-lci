open Cfg
open Miniimp

let translate_miniimp_to_dot_minirisc () = 
  let channel = open_in Sys.argv.(1) in
  let namefile = Sys.argv.(2) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./dot/"^ namefile ^ "-miniimp.dot") in
      Printf.fprintf oc "%s" (miniimp_cfg_to_dot ((translate_miniimp prog)));
      close_out oc
  | None -> print_string "no good"

let () = translate_miniimp_to_dot_minirisc ()