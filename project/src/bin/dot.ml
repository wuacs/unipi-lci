open Cfg
open Miniimp
open Data_flow_analysis

let get_cfg_dot_of_miniimp () =
  let ic = open_in Sys.argv.(2) in
  Fun.protect
    ~finally:(fun _ -> close_in ic)
    (fun _ ->
      match parse_with_errors (Lexing.from_channel ic) with
      | Some prog ->
          let oc = open_out Sys.argv.(3) in
          Fun.protect
            ~finally:(fun _ -> close_out oc)
            (fun _ ->
              Printf.fprintf oc "%s"
                (miniimp_cfg_to_dot (translate_miniimp prog)))
      | None -> failwith "Error while parsing MiniIMP")

let get_cfg_dot_of_minirisc_from_miniimp () =
  let ic = open_in Sys.argv.(2) in
  Fun.protect
    ~finally:(fun _ -> close_in ic)
    (fun _ ->
      match parse_with_errors (Lexing.from_channel ic) with
      | Some prog ->
          let oc = open_out Sys.argv.(3) in
          Fun.protect
            ~finally:(fun _ -> close_out oc)
            (fun _ ->
              Printf.fprintf oc "%s"
                (minirisc_cfg_to_dot
                   (miniimp_cfg_to_minirisc (translate_miniimp prog))))
      | None -> failwith "Error while parsing MiniIMP")

let get_optimized_riscfg_from_miniimp () =
  let ic = open_in Sys.argv.(2) in
  Fun.protect
    ~finally:(fun _ -> close_in ic)
    (fun _ ->
      let namefile = Sys.argv.(3) in
      let reg_num = Sys.argv.(4) in
      match parse_with_errors (Lexing.from_channel ic) with
      | Some prog ->
          let oc = open_out namefile in
          Fun.protect
            ~finally:(fun _ -> close_out oc)
            (fun _ ->
              let optimized_cfg, _, _ =
                Target_code.chaitin_briggs_algorithm
                  (Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog))
                  (int_of_string reg_num) Target_code.cost_metric
              in
              Printf.fprintf oc "%s" (Cfg.minirisc_cfg_to_dot optimized_cfg))
      | None -> failwith ("Error while parsing file " ^ namefile))

let print_to_dot_interference_graph () =
  let ic = open_in Sys.argv.(2) in
  Fun.protect
    ~finally:(fun _ -> close_in ic)
    (fun _ ->
      let namefile = Sys.argv.(3) in
      match Miniimp.parse_with_errors (Lexing.from_channel ic) with
      | Some prog ->
          let oc = open_out namefile in
          Fun.protect
            ~finally:(fun _ -> close_out oc)
            (fun _ ->
              Printf.fprintf oc "%s"
                (Target_code.interference_graph_dot
                   (Cfg.miniimp_cfg_to_minirisc (Cfg.translate_miniimp prog))))
      | None -> failwith ("Error while parsing file " ^ namefile))

let compute_live_analysis_and_create_dot () =
  let channel = open_in Sys.argv.(2) in
  let filename = Sys.argv.(3) in
  Fun.protect
    ~finally:(fun _ -> close_in channel)
    (fun _ ->
      match parse_with_errors (Lexing.from_channel channel) with
      | Some prog ->
          let oc = open_out filename in
          Fun.protect
            ~finally:(fun _ -> close_out oc)
            (fun _ ->
              Printf.fprintf oc "%s"
                (live_analysis_dot
                   (miniimp_cfg_to_minirisc (translate_miniimp prog))))
      | None -> print_string "Error: Parsing failed\n")

let compute_defined_analysis_and_create_dot () =
  let ic = open_in Sys.argv.(2) in
  let out_file_name = Sys.argv.(3) in
  Fun.protect
    ~finally:(fun _ -> close_in ic)
    (fun _ ->
      match parse_with_errors (Lexing.from_channel ic) with
      | Some prog ->
          let oc = open_out out_file_name in
          Fun.protect
            ~finally:(fun _ -> close_out oc)
            (fun _ ->
              Printf.fprintf oc "%s"
                (defined_analysis_dot
                   (miniimp_cfg_to_minirisc (translate_miniimp prog))))
      | None -> print_string "Error: Parsing failed\n")

let info_string =
  "This executable is for generating dot compliant resources.\n\n\n\
   Every dot file obtained by one of the following invocation can be rendered \
   visually, as for example a .pdf, with the instruction:\n\n\n\
   `dot -Tpdf file_name -o file_output`\n\n\n\
   For more information refer to the Graphitz package. \n\n\n\
   If you wish to produce a dot compliant file containing the control flow \
   graph of a MiniImp program then use:\n\n\n\
   dune exec dot miniimp input_file output_file\n\n\n\
   If you wish to produce a dot compliant file containing the control flow \
   graph of the MiniRISC equivalent of a MiniImp program in input use: \n\n\n\
   dune exec dot minirisc input_file output_file\n\n\n\
   If you wish to produce a dot compilant file showing the interference graph \
   of a \n\
   unoptimized translation of a MiniImp program to MiniRISC, use:\n\n\
   dune exec dot interference input_file output_file\n\n\n\
   If you wish to produce a dot compliant file showing the control flow graph \
   of a optimizedtranslation of a MiniImp program to MiniRISC for a target \
   architecture with at most `registers` \n\
   number of registers then use:\n\n\n\
   dune exec dot optimized input_file_path output_file_path registers\n\n\
   If you wish to produce a dot compliant file representing the CFG of a \
   MiniImp program translated to MiniRISC where each node contains the set of \
   registers\n\
   live in and live out of a node, then use:\n\n\n\
   dune exec dot liveness input_file_path output_file_path\n\n\n\
   If you wish to produce a dot compliant file representing the CFG of a \
   MiniImp program translated to MiniRisc where each node contains the set of \
   registers\n\
   certainly defined at the start and after the block, then use:\n\n\
   dune exec dot undefinedness input_file_path output_file_path"

let () =
  let fail_helper ?(additional_info = "Unspecified error") () =
    Printf.printf "%s" info_string;
    failwith (Printf.sprintf "Invalid arguments: %s" additional_info)
  in
  if Array.length Sys.argv < 2 then
    fail_helper ~additional_info:"Command not specified" ();
  match Sys.argv.(1) with
  | "miniimp" ->
      if Array.length Sys.argv <> 4 then
        fail_helper ~additional_info:"`miniimp` takes 2 parameters" ()
      else get_cfg_dot_of_miniimp ()
  | "minirisc" ->
      if Array.length Sys.argv <> 4 then
        fail_helper ~additional_info:"`minirisc` takes 2 parameters" ()
      else get_cfg_dot_of_minirisc_from_miniimp ()
  | "interference" ->
      if Array.length Sys.argv <> 4 then
        fail_helper ~additional_info:"`interference` takes 2 parameters" ()
      else print_to_dot_interference_graph ()
  | "optimized" ->
      if Array.length Sys.argv <> 5 then
        fail_helper ~additional_info:"`optimized` takes 3 parameters" ()
      else get_optimized_riscfg_from_miniimp ()
  | "undefinedness" ->
      if Array.length Sys.argv <> 4 then
        fail_helper ~additional_info:"`undefinedness takes 2 parameters" ()
      else compute_defined_analysis_and_create_dot ()
  | "liveness" ->
      if Array.length Sys.argv <> 4 then
        fail_helper ~additional_info:"`liveness takes 2 parameters`" ()
      else compute_live_analysis_and_create_dot ()
  | _ -> fail_helper ()
