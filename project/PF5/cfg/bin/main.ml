open Cfg
open Miniimp

let () =  
    let channel = open_in Sys.argv.(1) 
  in
    match parse_with_errors (Lexing.from_channel channel) with
    | Some prog -> let oc = open_out "./dot/fibonacci.dot" 
                  in
                    Printf.fprintf oc "%s" (miniimp_cfg_to_dot (translate_miniimp prog)) ;
                  close_out oc
  
    | None -> print_string "no good"