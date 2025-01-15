module Utils = Data_flow_utils
module LiveAnalysis = Liveness_analysis
module DefinedAnalysis = Defined_analysis

let node_to_string (node : Cfg.node) : string =
  match node with Label n -> string_of_int n

(** Computes the live analysis and a string in the Dot language 
in which each node contains the live_in and live_out sets *)
let live_analysis_dot (cfg : Minirisc.scomm Cfg.control_flow_graph) : string =
  let pretty_print_live_sets (bas : Data_flow_utils.block_analysis_state) : string = 
    "Live-IN: " ^ Minirisc.RegisterSet.fold (fun reg acc -> acc ^ "R" ^ (string_of_int reg) ^ "\\n") bas.in_set "[ " ^ " ]
    \\n Live-OUT: " ^ Minirisc.RegisterSet.fold (fun reg acc -> acc ^ "R" ^ (string_of_int reg) ^ "\\n") bas.out_set "[ " ^ " ]"
  in
  let liveness_result = LiveAnalysis.liveness_analysis cfg in
  let nodes_str =
    Cfg.NodeSet.fold
      (fun node acc ->
        acc
        ^ Printf.sprintf "  %s [label=\"%s\"];\n" (node_to_string node)
            (pretty_print_live_sets (Cfg.NodeMap.find node liveness_result)))
      cfg.nodes ""
  in

  let edges_str =
    Cfg.NodeMap.fold
      (fun src next_codomain acc ->
        acc ^ Cfg.next_codomain_to_string src next_codomain)
      cfg.edges ""
  in

  let entry_str = node_to_string cfg.entry in
  let exit_str = node_to_string cfg.exit in

  (* Return the full graph's DOT string *)
  Printf.sprintf
    "digraph G {\n\
    \  // Entry node\n\
    \  %s [shape=ellipse, color=green];\n\
    \  // Exit node\n\
    \  %s [shape=ellipse, color=red];\n\
    %s%s}\n"
    entry_str exit_str nodes_str edges_str

let defined_analysis_dot (cfg : Minirisc.scomm Cfg.control_flow_graph) : string =
  let pretty_print_defined_sets (bas : Data_flow_utils.block_analysis_state) : string =
      "Defined-IN: " ^ Minirisc.RegisterSet.fold (fun reg acc -> acc ^ "R" ^ (string_of_int reg) ^ "\\n") bas.in_set "[ " ^ " ]
      \\n Defined-OUT: " ^ Minirisc.RegisterSet.fold (fun reg acc -> acc ^ "R" ^ (string_of_int reg) ^ "\\n") bas.out_set "[ " ^ " ]"
  in
  let defined_result = DefinedAnalysis.defined_analysis cfg in
  let nodes_str =
    Cfg.NodeSet.fold
      (fun node acc ->
        acc
        ^ Printf.sprintf "  %s [label=\"%s\"];\n" (node_to_string node)
            (pretty_print_defined_sets (Cfg.NodeMap.find node defined_result)))
      cfg.nodes ""
  in

  let edges_str =
    Cfg.NodeMap.fold
      (fun src next_codomain acc ->
        acc ^ Cfg.next_codomain_to_string src next_codomain)
      cfg.edges ""
  in

  let entry_str = node_to_string cfg.entry in
  let exit_str = node_to_string cfg.exit in

  (* Return the full graph's DOT string *)
  Printf.sprintf
    "digraph G {\n\
    \  // Entry node\n\
    \  %s [shape=ellipse, color=green];\n\
    \  // Exit node\n\
    \  %s [shape=ellipse, color=red];\n\
    %s%s}\n"
    entry_str exit_str nodes_str edges_str
    
let check_for_undefinedness = DefinedAnalysis.check_for_undefinedness