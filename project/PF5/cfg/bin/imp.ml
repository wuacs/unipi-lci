open Miniimp
open Cfg

let translate_miniimp_to_dot_minirisc () = 
  let channel = open_in Sys.argv.(1) in
  let namefile = Sys.argv.(2) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let oc = open_out ("./dot/"^ namefile ^ "-miniimp.dot") in
      Printf.fprintf oc "%s" (miniimp_cfg_to_dot ((translate_miniimp prog)));
      close_out oc
  | None -> print_string "no good"

let execute_miniimp () = 
  let channel = open_in Sys.argv.(1) in
  let inp = Sys.argv.(2) in
  match parse_with_errors (Lexing.from_channel channel) with
  | Some prog ->
      let res = (eval prog (int_of_string inp)) in
      begin
      match res with
      | Some v -> print_int v
      | None -> print_string "ciao"
    end
  | None -> print_string "no good"

let () = execute_miniimp ()