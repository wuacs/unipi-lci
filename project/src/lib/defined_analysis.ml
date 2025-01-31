module RegisterSet = Minirisc.RegisterSet

type block_analysis_state = Data_flow_utils.block_analysis_state

let update_in (cfg : Minirisc.scomm Cfg.control_flow_graph) (node : Cfg.node)
    (bas : block_analysis_state Cfg.NodeMap.t)
    (reversed : Cfg.node list Cfg.NodeMap.t) :
    block_analysis_state Cfg.NodeMap.t =
  let blk = Cfg.NodeMap.find node bas in
  if node == cfg.entry then
    Cfg.NodeMap.add node
      {
        Data_flow_utils.in_set = RegisterSet.singleton Minirisc.in_register;
        out_set = blk.out_set;
      }
      bas
  else
    let precedessors = Cfg.NodeMap.find node reversed in
    Cfg.NodeMap.add node
      {
        Data_flow_utils.in_set =
          List.fold_left
            (fun set node ->
              RegisterSet.inter set (Cfg.NodeMap.find node bas).out_set)
            (Data_flow_utils.get_top cfg)
            precedessors;
        out_set = blk.out_set;
      }
      bas

let update_out (cfg : Minirisc.scomm Cfg.control_flow_graph) (node : Cfg.node)
    (bas : block_analysis_state Cfg.NodeMap.t) :
    block_analysis_state Cfg.NodeMap.t =
  let blk_state = Cfg.NodeMap.find node bas in
  let declared_registers = Data_flow_utils.defined_registers cfg node in
  Cfg.NodeMap.add node
    {
      Data_flow_utils.in_set = blk_state.in_set;
      out_set = RegisterSet.union blk_state.in_set declared_registers;
    }
    bas

let defined_analysis (cfg : Minirisc.scomm Cfg.control_flow_graph) =
  let reversed = Cfg.compute_reversed_map cfg in
  let start = Data_flow_utils.initialize_with_top cfg in
  let rec compute_fixpoint (cfg : Minirisc.scomm Cfg.control_flow_graph)
      (equal : bool) (state : block_analysis_state Cfg.NodeMap.t) :
      block_analysis_state Cfg.NodeMap.t =
    match equal with
    | true -> state
    | false ->
        let res =
          Cfg.NodeMap.fold
            (fun x initial (new_state, propg) ->
              let new_global =
                update_out cfg x (update_in cfg x new_state reversed)
              in
              let new_local = Cfg.NodeMap.find x new_global in
              if
                RegisterSet.cardinal new_local.out_set
                != RegisterSet.cardinal initial.Data_flow_utils.out_set
                || RegisterSet.cardinal new_local.in_set
                   != RegisterSet.cardinal initial.Data_flow_utils.in_set
              then (new_global, false)
              else (new_global, propg))
            state (state, true)
        in
        compute_fixpoint cfg (snd res) (fst res)
  in
  compute_fixpoint cfg false start

let check_for_undefinedness (cfg : Minirisc.scomm Cfg.control_flow_graph) : bool
    =
  let defined_analysis_result = defined_analysis cfg in
  Cfg.NodeMap.fold
    (fun nodeId sets b ->
      match b with
      | true -> b
      | _ -> (
          match
            RegisterSet.subset
              (Data_flow_utils.used_registers cfg nodeId)
              sets.Data_flow_utils.in_set
          with
          | true -> false
          | _ -> true))
    defined_analysis_result false
