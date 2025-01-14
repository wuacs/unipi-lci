module Nodemap = Cfg.NodeMap
module Nodeset = Cfg.NodeSet
module RegSet = Minirisc.RegisterSet

exception NodeNotFound of string

type mriscfg = Minirisc.scomm Cfg.control_flow_graph
type node = Cfg.node
type stateMap = Data_flow_utils.block_analysis_state Cfg.NodeMap.t
type riscomm = Minirisc.scomm
type reg = Minirisc.register


let find_node_safe (node : node) (map : 'a Nodemap.t) (description : string) : 'a =
  try
    Nodemap.find node map
  with Not_found ->
    raise (NodeNotFound (Printf.sprintf "Node not found in %s: %d" description (match node with
    | Cfg.Label x -> x)))

let extract_read_registers (stmt : riscomm) : RegSet.t =
  match stmt with
  | Minirisc.Load (r1, _) -> (RegSet.add r1 RegSet.empty)
  | Minirisc.LoadI (_, _) -> RegSet.empty
  | Minirisc.Nop -> RegSet.empty
  | Minirisc.Rtoi (_, r1, _, _) -> RegSet.add r1 RegSet.empty
  | Minirisc.Rtor (_, r1, r2, _) ->
      RegSet.add r2 (RegSet.add r1 RegSet.empty)
  | Minirisc.Rury (_, r1, _) -> RegSet.add r1 RegSet.empty
  | Minirisc.Store (r1, _) -> RegSet.add r1 RegSet.empty

let extract_written_register (stmt : riscomm) : reg Option.t =
  match stmt with
  | Minirisc.Load (_, r) -> Some r
  | Minirisc.LoadI (_, r) -> Some r
  | Minirisc.Rtoi (_, _, _, r) -> Some r
  | Minirisc.Rtor (_, _, _, r) -> Some r
  | Minirisc.Rury (_, _, r) -> Some r
  | Minirisc.Store(_, r) -> Some r 
  | Minirisc.Nop -> None

(** The default implementation of the IR uses maximal blocks, thus we need to know in a block
which registers are `used`, that is they are read without being written before by another instruction in the same thread *)
let used_registers (cfg : mriscfg) (node : node) : RegSet.t =
  let code_blk = Nodemap.find node cfg.code in
  snd
    (List.fold_left
       (fun (written, used) x ->
         let new_used = RegSet.diff (extract_read_registers x) written in
         match extract_written_register x with
         | Some r -> (RegSet.add r written, RegSet.union used new_used)
         | None -> (written, RegSet.union used new_used))
       (RegSet.empty, RegSet.empty)
       code_blk)

let update_in (cfg : mriscfg) (node : node) (stateMap : stateMap) : stateMap =
  let local_state = Nodemap.find node stateMap in
  let new_in =
    RegSet.union (used_registers cfg node)
      (RegSet.diff local_state.out_set
         (Data_flow_utils.extract_defined_registers cfg node))
  in
  Nodemap.add node
    { Data_flow_utils.in_set = new_in; out_set = local_state.out_set }
    stateMap
let update_out (cfg : mriscfg) (node : Cfg.node) (bas : stateMap)
    (reversed : node list Nodemap.t) : stateMap =
  let blk = Cfg.NodeMap.find node bas in
  if node == cfg.exit then
    Cfg.NodeMap.add node
      {
        Data_flow_utils.in_set = blk.in_set;
        out_set = RegSet.singleton Cfg.out_register;
      }
      bas
  else
    let precedessors = Cfg.NodeMap.find node reversed in
    Cfg.NodeMap.add node
      {
        Data_flow_utils.in_set = blk.in_set;
        out_set =
          List.fold_left
            (fun set node ->
              RegSet.union set (Cfg.NodeMap.find node bas).in_set)
            RegSet.empty precedessors;
      }
      bas

let coalesced_edge_map (cfg : mriscfg) : node list Nodemap.t =
  Nodemap.fold (fun node list coalesced -> 
    Cfg.NodeMap.add
    node (List.fold_left (fun acc x -> match x with
    | Cfg.Uncond t -> t::acc
    | Cfg.Cond(t, t1) -> t :: t1 :: acc
    | Cfg.None -> acc) [] list) coalesced) cfg.edges Nodemap.empty
  
let liveness_analysis (cfg : mriscfg) : stateMap =
  let coalesced_map = coalesced_edge_map cfg in
  let start = Data_flow_utils.initialize_with_bottom cfg in
  let rec compute_fixpoint (cfg : mriscfg) (equal : bool) (state : stateMap) : stateMap =
    match equal with
    | true -> state
    | false ->
        let res =
          Cfg.NodeMap.fold
            (fun x initial (new_state, propg) ->
              let new_global = update_out cfg x (update_in cfg x new_state) coalesced_map
              in
              let new_local = Cfg.NodeMap.find x new_global in
              if
                RegSet.cardinal new_local.out_set
                != RegSet.cardinal initial.Data_flow_utils.out_set
                || RegSet.cardinal new_local.in_set
                   != RegSet.cardinal initial.Data_flow_utils.in_set
              then(new_global, false)
              else (new_global, propg))
            state (state, true)
        in
        compute_fixpoint cfg (snd res) (fst res)
  in
  compute_fixpoint cfg false start
