open Cfg
open Miniimp
let get_cfg_dot_of_miniimp () = 
  let ic = open_in Sys.argv.(2) in
  Fun.protect ~finally:(fun _ -> close_in ic)
  (fun _ -> 
    match parse_with_errors (Lexing.from_channel ic) with
    | Some prog -> 
      begin
      let oc = open_out Sys.argv.(3) in
      Fun.protect ~finally:(fun _ -> close_out oc)
      (fun _ -> Printf.fprintf oc "%s" (miniimp_cfg_to_dot (translate_miniimp prog)))
      end
    | None -> failwith "Error while parsing MiniIMP")

let get_cfg_dot_of_minirisc_from_miniimp () = 
  let ic = open_in Sys.argv.(2) in
  Fun.protect ~finally:(fun _ -> close_in ic)
  (fun _ -> 
    match parse_with_errors (Lexing.from_channel ic) with
    | Some prog -> 
      begin
      let oc = open_out Sys.argv.(3) in
      Fun.protect ~finally:(fun _ -> close_out oc)
      (fun _ -> Printf.fprintf oc "%s" (minirisc_cfg_to_dot (miniimp_cfg_to_minirisc ((translate_miniimp prog)))))
      end
    | None -> failwith "Error while parsing MiniIMP")
  

let info_string =
"This executable is for generating .Dot compliant resources.\n

Every dot file obtained by one of the following invocation can be rendered visually, \
as for example a .pdf, with the instruction:

`dot -Tpdf file_name`

For more information refer to the Graphitz package. 

If you wish to produce a .dot compliant file containing the control flow graph \
of a MiniImp program then use:

dune exec miniimp input_file output_file

If you wish to produce a .dot compliant file containing the control flow graph \
of the MiniRISC equivalent of a MiniImp program in input use: 

dune exec minirisc input_file output_file"

let () =
  let fail_helper ?(additional_info = "Unspecified error") () =
    Printf.printf "%s" info_string;
    failwith (Printf.sprintf "Invalid arguments: %s" additional_info) 
  in
  if Array.length Sys.argv < 2 then fail_helper ~additional_info:"Command not specified" ();
  match Sys.argv.(1) with
  | "miniimp" -> if Array.length Sys.argv <> 4 then fail_helper ~additional_info:"`miniimp` takes 4 parameters" () else get_cfg_dot_of_miniimp ()
  | "minirisc" -> if Array.length Sys.argv <> 4 then fail_helper ~additional_info:"`minirisc` takes 4 parameters" () else get_cfg_dot_of_minirisc_from_miniimp ()
  | _ -> fail_helper ()