module RegisterSet = Minirisc.RegisterSet
type block_analysis_state = Data_flow_utils.block_analysis_state

(* Given a minirisc instruction returns in a RegisterSet all the registers 
referenced in the instruction *)
let extract_used_registers (stmt : Minirisc.scomm) : RegisterSet.t =
  match stmt with
  | Minirisc.Load (r1, r2) -> RegisterSet.add r2 (RegisterSet.add r1 RegisterSet.empty)
  | Minirisc.LoadI (_, r) -> RegisterSet.add r RegisterSet.empty
  | Minirisc.Nop -> RegisterSet.empty
  | Minirisc.Rtoi(_, r1, _, r2) -> RegisterSet.add r2 (RegisterSet.add r1 RegisterSet.empty)
  | Minirisc.Rtor(_, r1, r2, r3) -> RegisterSet.add r3 (RegisterSet.add r2 (RegisterSet.add r1 RegisterSet.empty))
  | Minirisc.Rury(_, r1, r2) -> RegisterSet.add r2 (RegisterSet.add r1 RegisterSet.empty)
  | Minirisc.Store(r1, r2) -> RegisterSet.add r2 (RegisterSet.add r1 RegisterSet.empty)

(* Compute all the register used in the control flow graph*)
let compute_top (cfg : Minirisc.scomm Cfg.control_flow_graph) : RegisterSet.t = 
  Cfg.NodeMap.fold 
    (fun _ stmts set -> 
      let res = 
        List.fold_left (fun accumulated_registers stmt -> 
          let blk_res = extract_used_registers stmt  in
          RegisterSet.union blk_res accumulated_registers) 
          RegisterSet.empty stmts
      in 
      RegisterSet.union res set) cfg.code RegisterSet.empty



let update_in 
  (cfg : Minirisc.scomm Cfg.control_flow_graph)
  (node : Cfg.node)
  (bas : block_analysis_state Cfg.NodeMap.t) 
  (reversed : Cfg.node list Cfg.NodeMap.t)
  : block_analysis_state Cfg.NodeMap.t = 
  let blk = Cfg.NodeMap.find node  bas in
    if node == cfg.entry then
      (Cfg.NodeMap.add node ({Data_flow_utils.in_set = (RegisterSet.add Cfg.in_register RegisterSet.empty); out_set = blk.out_set}) bas)
    else
      let precedessors = Cfg.NodeMap.find node reversed in
      Cfg.NodeMap.add node 
      {Data_flow_utils.in_set = List.fold_left (fun set node ->
        RegisterSet.inter set (Cfg.NodeMap.find node bas).out_set) 
        RegisterSet.empty precedessors; out_set = blk.out_set} bas

let update_out 
  (cfg : Minirisc.scomm Cfg.control_flow_graph)
  (node : Cfg.node)
  (bas : block_analysis_state Cfg.NodeMap.t) 
  : block_analysis_state Cfg.NodeMap.t = 
  let blk_state = Cfg.NodeMap.find node bas in
  let declared_registers = Data_flow_utils.extract_defined_registers cfg node in
  Cfg.NodeMap.add node {Data_flow_utils.in_set = blk_state.in_set; out_set = RegisterSet.union blk_state.in_set declared_registers} bas

let initialize_state_with_top
  (cfg: Minirisc.scomm Cfg.control_flow_graph) : block_analysis_state Cfg.NodeMap.t =
  let top = compute_top cfg in
  Cfg.NodeSet.fold (fun node basm -> Cfg.NodeMap.add node {Data_flow_utils.in_set = top; out_set = top} basm) cfg.nodes Cfg.NodeMap.empty

let defined_analysis
  (cfg: Minirisc.scomm Cfg.control_flow_graph) = 
      let top = compute_top cfg 
    in
      let reversed = Cfg.compute_reversed_map cfg 
    in
      let start = initialize_state_with_top cfg 
    in
      let rec compute_fixpoint 
      (cfg : Minirisc.scomm Cfg.control_flow_graph)
      (equal : bool)
      (state: block_analysis_state Cfg.NodeMap.t) : 
      block_analysis_state Cfg.NodeMap.t = 
      match equal with
      | true -> state
      | false -> let res = (Cfg.NodeMap.fold 
                (fun x initial (new_state, propg) -> 
                  let new_global = update_out cfg x (update_in cfg x new_state reversed)
                in
                  let new_local = (Cfg.NodeMap.find x new_global)
                in
                  if  ((RegisterSet.cardinal (new_local.out_set)) != (RegisterSet.cardinal initial.Data_flow_utils.out_set)) ||
                      (RegisterSet.cardinal (new_local.in_set) != (RegisterSet.cardinal initial.Data_flow_utils.in_set)) then
                    (new_global, false)
                else
                    (new_global, propg)) state (state, true)) in
                  compute_fixpoint cfg (snd res) (fst res)
    in
      RegisterSet.equal (Cfg.NodeMap.find cfg.exit (compute_fixpoint cfg false start)).out_set top 
                  