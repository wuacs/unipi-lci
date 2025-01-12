(*module Nodemap = Cfg.NodeMap
module Nodeset = Cfg.NodeSet
module RegSet = Minirisc.RegisterSet
module RegMap = Minirisc.RegisterMap
module EdgeSet : Set.S with type elt = (Cfg.node * Cfg.node)
type mriscfg = Minirisc.scomm Cfg.control_flow_graph
type node = Cfg.node
type riscomm = Minirisc.scomm
type reg = Minirisc.register
type color = int
type interference_graph = (color * EdgeSet.t) RegMap.t
type heuristic = int RegMap.t
type degree = int
val not_colored : int

(* 
Computes the live range of each register in a MiniRISC CFG 
and returns a map where a register is mapped to, another, mergeable register.
Thus, an unmergeable register, that is, a register which intersect with 
every other register's live range will be mapped to itself.

The mapping is decided through a greedy algorithm: every regiter `r` is sorted
by its live range size, that is, the number of edges l->l' where `r` is live
when entering node l' and then iteratively paired with as many registers
it is mergeable.
*)
val compute_mergeable : mriscfg -> reg Nodemap.t

val compute_live_ranges : mriscfg -> EdgeSet.t Nodemap.t

(* Compute a Spill Metric for each node in the interference graph:
the spill metric is defined as = Cost(Number of writes and reads) / Degree(Number of interfering nodes).

The best node to "spill" is the node with lowest cost and highest degree because
the fact of having low cost would make the number of extra instructions be low and
having a great degree indicates the possibility of making a lot of nodes "colorable". *)
val cost_degree_metric : interference_graph -> heuristic

val compute_degree : interference_graph -> reg -> int

(* Complexity is...*)
val build_interference_graph : mriscfg -> interference_graph

(* The Chaitin-Briggs coloring algorithm which colors an interference graph with the steps

1) Picks any vertex in the interference graph `G` with a degree less than `K`, i.e. what was put in input, and push it on a stack.

2) If `G` is non-empty then, using the heuristic strategy given in input, it adds the node with highest 
value w.r.t the heuristic to the stack and return to step 1.

3) After you have done 1. and 2. pop from the stack node X, if node X can be colored, i.e. it has less than
`K` colors as neighbors then color it with the lowest color not used by any of its neighbors.

*)
val chaitin_briggs_algorithm: mriscfg -> degree -> heuristic -> interference_graph
*)