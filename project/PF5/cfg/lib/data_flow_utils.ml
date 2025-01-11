module RegisterSet = Minirisc.RegisterSet
module Nodemap = Cfg.NodeMap

type block_analysis_state = {
    in_set: Minirisc.RegisterSet.t;
    out_set: Minirisc.RegisterSet.t
}

let initialize_with_bottom (cfg : Minirisc.scomm Cfg.control_flow_graph) : block_analysis_state Cfg.NodeMap.t =
    Cfg.NodeSet.fold (fun node basm -> Cfg.NodeMap.add node {in_set = Minirisc.RegisterSet.empty; out_set = Minirisc.RegisterSet.empty} basm) cfg.nodes Cfg.NodeMap.empty

let extract_defined_registers (stmt : Minirisc.scomm) : RegisterSet.t = 
    match stmt with
    | Minirisc.Load(_, r) -> RegisterSet.add r RegisterSet.empty 
    | Minirisc.LoadI(_, r) -> RegisterSet.add r RegisterSet.empty
    | _ -> RegisterSet.empty

(* Fails if the input node is not present in the cfg, otherwise, it returns
    the registers defined in the node *)
let extract_defined_registers (cfg : Minirisc.scomm Cfg.control_flow_graph)
    (node : Cfg.node) : Minirisc.RegisterSet.t = 
    List.fold_left (fun acc x -> (RegisterSet.union acc (extract_defined_registers x))) RegisterSet.empty (Nodemap.find node cfg.code)
