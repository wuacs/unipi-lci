module RegisterSet = Minirisc.RegisterSet
module Nodemap = Cfg.NodeMap
module Nodeset = Cfg.NodeSet
module RegSet = Minirisc.RegisterSet
type mriscfg = Minirisc.scomm Cfg.control_flow_graph
type node = Cfg.node
type riscomm = Minirisc.scomm
type reg = Minirisc.register

type block_analysis_state = {
  in_set : Minirisc.RegisterSet.t;
  out_set : Minirisc.RegisterSet.t;
}

let extract_read_registers (stmt : riscomm) : RegSet.t =
  try
    match stmt with
    | Minirisc.Load (r1, _) -> RegSet.add r1 RegSet.empty
    | Minirisc.LoadI (_, _) -> RegSet.empty
    | Minirisc.Nop -> RegSet.empty
    | Minirisc.Rtoi (_, r1, _, _) -> RegSet.add r1 RegSet.empty
    | Minirisc.Rtor (_, r1, r2, _) ->
        RegSet.add r2 (RegSet.add r1 RegSet.empty)
    | Minirisc.Rury (_, r1, _) -> RegSet.add r1 RegSet.empty
    | Minirisc.Store (r1, _) -> RegSet.add r1 RegSet.empty
    with e ->
      Printf.eprintf "Error in extract_read_register: %s\n" (Printexc.to_string e);
      raise e

let extract_written_register (stmt : riscomm) : reg Option.t =
  try
    match stmt with
    | Minirisc.Load (_, r) -> Some r
    | Minirisc.LoadI (_, r) -> Some r
    | Minirisc.Rtoi (_, _, _, r) -> Some r
    | Minirisc.Rtor (_, _, _, r) -> Some r
    | Minirisc.Rury (_, _, r) -> Some r
    | Minirisc.Store (_, r) -> Some r
    | Minirisc.Nop -> None
  with e ->
    Printf.eprintf "Error in extract_written_register: %s\n" (Printexc.to_string e);
    raise e

let initialize_with_bottom (cfg : Minirisc.scomm Cfg.control_flow_graph) :
    block_analysis_state Cfg.NodeMap.t =
  Cfg.NodeSet.fold
    (fun node basm ->
      Cfg.NodeMap.add node
        {
          in_set = Minirisc.RegisterSet.empty;
          out_set = Minirisc.RegisterSet.empty;
        }
        basm)
    cfg.nodes Cfg.NodeMap.empty

(* Fails if the input node is not present in the cfg, otherwise, it returns
    the registers defined in the node *)
let defined_registers (cfg : Minirisc.scomm Cfg.control_flow_graph)
    (node : Cfg.node) : Minirisc.RegisterSet.t =
    match (Nodemap.find_opt node cfg.code) with
    | Some t -> (List.fold_left
      (fun acc x -> match extract_written_register x with
      | Some r -> RegisterSet.add r acc 
      | None -> acc) RegisterSet.empty t)
    | None -> failwith "Error in defined_registes function, Ill-formed CFG"

let used_registers (cfg : mriscfg) (node : node) : RegSet.t =
  let code_blk = Nodemap.find node cfg.code in
    snd (List.fold_left
      (fun (written, used) x ->
        let new_used = RegSet.diff (extract_read_registers x) written in
        match extract_written_register x with
        | Some r -> (RegSet.add r written, RegSet.union used new_used)
        | None -> (written, RegSet.union used new_used))
      (RegSet.empty, RegSet.empty)
      code_blk)

(* Compute all the register read in the control flow graph*)
let compute_top (cfg : Minirisc.scomm Cfg.control_flow_graph) : RegisterSet.t =
  Cfg.NodeMap.fold
    (fun _ stmts set ->
      let res =
        List.fold_left
          (fun accumulated_registers stmt ->
            let blk_res = extract_read_registers stmt in
            RegisterSet.union blk_res accumulated_registers)
          RegisterSet.empty stmts
      in
      RegisterSet.union res set)
    cfg.code RegisterSet.empty

let initialize_with_top (cfg : Minirisc.scomm Cfg.control_flow_graph) :
  block_analysis_state Cfg.NodeMap.t =
    let top = compute_top cfg in
    Cfg.NodeSet.fold
      (fun node basm ->
        Cfg.NodeMap.add node { in_set = top; out_set = top } basm)
      cfg.nodes Cfg.NodeMap.empty

let get_top (cfg : Minirisc.scomm Cfg.control_flow_graph) : RegisterSet.t = 
  compute_top cfg 