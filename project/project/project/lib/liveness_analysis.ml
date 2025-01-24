module Nodemap = Cfg.NodeMap
module Nodeset = Cfg.NodeSet
module RegSet = Minirisc.RegisterSet

exception NodeNotFound of string

type mriscfg = Minirisc.scomm Cfg.control_flow_graph
type node = Cfg.node
type stateMap = Data_flow_utils.block_analysis_state Cfg.NodeMap.t

let find_node_safe (node : node) (map : 'a Nodemap.t) (description : string) : 'a =
  try
    Nodemap.find node map
  with Not_found ->
    raise (NodeNotFound (Printf.sprintf "Node not found in %s: %d" description (match node with
    | Cfg.Label x -> x)))

let update_in (cfg : mriscfg) (node : node) (stateMap : stateMap) : stateMap =
  try
    let local_state = find_node_safe node stateMap "update in" in
    let new_in =
      RegSet.union (Data_flow_utils.used_registers cfg node)
        (RegSet.diff local_state.out_set
           (Data_flow_utils.defined_registers cfg node))
    in
    Nodemap.add node
      { Data_flow_utils.in_set = new_in; out_set = local_state.out_set }
      stateMap
  with e ->
    Printf.eprintf "Error in update_in: %s\n" (Printexc.to_string e);
    raise e

let update_out (cfg : mriscfg) (node : Cfg.node) (bas : stateMap)
    (coalesced : node list Nodemap.t) : stateMap =
  try
    let blk = find_node_safe node bas "update out" in
    if node == cfg.exit then
      Cfg.NodeMap.add node
        {
          Data_flow_utils.in_set = blk.in_set;
          out_set = RegSet.singleton Cfg.out_register;
        }
        bas
    else
      let precedessors = find_node_safe node coalesced "find edges after" in
      Cfg.NodeMap.add node
        {
          Data_flow_utils.in_set = blk.in_set;
          out_set =
            List.fold_left
              (fun set node ->
                RegSet.union set (find_node_safe node bas "union").in_set)
              RegSet.empty precedessors;
        }
        bas
  with e ->
    Printf.eprintf "Error in update_out: %s\n" (Printexc.to_string e);
    raise e

let coalesced_edge_map (cfg : mriscfg) : node list Nodemap.t =
  try
    Nodemap.add cfg.exit [] (Nodemap.fold (fun node next coalesced ->
      Cfg.NodeMap.add
      node (match next with
      | Cfg.Uncond t -> [t]
      | Cfg.Cond(t, t1) -> [t; t1]
      | Cfg.None -> []) coalesced) cfg.edges Nodemap.empty)
  with e ->
    Printf.eprintf "Error in coalesced_edge_map: %s\n" (Printexc.to_string e);
    raise e

let liveness_analysis (cfg : mriscfg) : stateMap =
  try
    let coalesced_map = coalesced_edge_map cfg in
    let start = Data_flow_utils.initialize_with_bottom cfg in
    let rec compute_fixpoint (cfg : mriscfg) (equal : bool) (state : stateMap) : stateMap =
      try
        match equal with
        | true -> state
        | false ->
            let res =
              Cfg.NodeMap.fold
                (fun x initial (new_state, propg) ->
                  let new_global = update_out cfg x (update_in cfg x new_state) coalesced_map in
                  let new_local = find_node_safe x new_global "last" in
                  if
                    RegSet.cardinal new_local.out_set
                    != RegSet.cardinal initial.Data_flow_utils.out_set
                    || RegSet.cardinal new_local.in_set
                       != RegSet.cardinal initial.Data_flow_utils.in_set
                  then  (new_global, false)
                  else (new_global, propg))
                state (state, true)
            in
            compute_fixpoint cfg (snd res) (fst res)
      with e ->
        Printf.eprintf "Error in compute_fixpoint: %s\n" (Printexc.to_string e);
        raise e
    in
    compute_fixpoint cfg false start
  with e ->
    Printf.eprintf "Error in liveness_analysis: %s\n" (Printexc.to_string e);
    raise e