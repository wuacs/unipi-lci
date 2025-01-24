open Miniimp
open Cfg

let translate_miniimp_to_dot () = 
  let input_channel = open_in Sys.argv.(2) in
  Fun.protect ~finally: (fun _ -> close_in input_channel) (
    fun _ ->
      let output_file = Sys.argv.(3) in
      match parse_with_errors (Lexing.from_channel input_channel) with
      | Some prog ->
          let oc = open_out output_file in
          Fun.protect ~finally:(fun _ -> close_out oc) 
          (fun _ -> Printf.fprintf oc "%s" (miniimp_cfg_to_dot (translate_miniimp prog)))
      | None -> print_string "Error while parsing")

let execute_miniimp_from_file () = 
  let input_channel = open_in Sys.argv.(2) in
  Fun.protect ~finally: (fun _ -> close_in input_channel) (
    fun _ -> (
      let value_in_input = Sys.argv.(3) in
      match parse_with_errors (Lexing.from_channel input_channel) with
      | Some prog ->
          let res = (eval prog (int_of_string value_in_input)) in
          begin
          match res with
          | Some v -> print_int v
          | None -> print_string "Evaluation failed"
        end
      | None -> print_string "Error while parsing"
    ) 
  )

let info_string = 
"To use executable imp_interpreter pass one of the following pattern of arguments:\n\n
To obtain a Dot compliant file showing the program's CFG use:\n\n
dune exec imp_interpreter dot input_file_path output_file_path\n\n
To execute a MiniImp program written in some file and output its result
on stdout, instead, use the following:\n\n
dune exec imp_interpreter execute input_file_path integer_in_input\n\n"

let () = 
  let fail_helper = (fun _ -> 
    Printf.printf "%s" info_string;
    failwith "invalid arguments")
  in
  let choice = Sys.argv.(1) in
  match choice with
  | "dot" -> if (Array.length (Sys.argv)) <> 4 then Printf.printf "ciao" else translate_miniimp_to_dot ()
  | "execute" -> if (Array.length (Sys.argv)) <> 4 then fail_helper () else execute_miniimp_from_file ()
  | _ -> fail_helper ()
