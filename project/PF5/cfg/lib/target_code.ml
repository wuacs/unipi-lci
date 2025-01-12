(*module Nodemap = Cfg.NodeMap
module Nodeset = Cfg.NodeSet
module RegSet = Minirisc.RegisterSet
module RegMap = Minirisc.RegisterMap

module EdgeSet = Set.Make(struct
  type t = (Cfg.node * Cfg.node)
  let compare x y = compare x y
end)

type mriscfg = Minirisc.scomm Cfg.control_flow_graph
type node = Cfg.node
type riscomm = Minirisc.scomm
type reg = Minirisc.register
type color = int
type interference_graph = (color * EdgeSet.t) RegMap.t
type heuristic = int RegMap.t
type degree = int
type liveRangeSet = EdgeSet.t RegMap.t
(*
Given a node it computes its live range,

that is, the set of edges l -> l' 

Loops on all nodes (O(|L|)), for every node loops on every node it can be accessed,
thus it is O(|L|+|A|), and for each node it accesses all registers .
For every register it adds
*)
let compute_live_ranges (cfg: mriscfg) : liveRangeSet =
  let mapState = Data_flow_analysis.LiveAnalysis.liveness_analysis cfg in
  Nodeset.fold (fun nodeId liveRangeSet ->
    (* Get the list of edges for the current node *)
    let edges = Nodemap.find nodeId cfg.edges in
    List.fold_left (fun acc edge ->
      RegSet.fold (fun register acc ->
        RegMap.update register (fun set ->
          let current_edges = match set with
            | Some s -> s
            | None -> EdgeSet.empty
          in
          match edge with
          | Cfg.Cond(t, r) -> Some (EdgeSet.add (nodeId, t) (EdgeSet.add (nodeId, r) current_edges))
          | Cfg.Uncond v -> Some (EdgeSet.add (nodeId, v) current_edges)
          | Cfg.None -> Some(current_edges)
        ) acc
      ) (Nodemap.find nodeId mapState).out_set acc
    ) liveRangeSet edges
  ) cfg.nodes RegMap.empty

(* Function to extract and sort by live range size *)
let sort_by_live_range_size (mapEdges : EdgeSet.t RegMap.t) =
  RegMap.bindings mapEdges  (* Extract key-value pairs *)
  |> List.sort (fun (_, e1) (_, e2) -> compare (EdgeSet.cardinal e1) (EdgeSet.cardinal e2))

(** let cost_degree_metric (graph : interference_graph) heuristic = *)
  *)