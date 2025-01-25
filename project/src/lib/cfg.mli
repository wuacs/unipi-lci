type node = Label of int [@@unboxed]

module NodeSet : Set.S with type elt = node
module NodeMap : Map.S with type key = node
module ImpAst = Miniimp.ImpAst

(** A Node may have
+ one outgoing edge, represented by `Uncond`
+ two outgoing edges, representing a conditional choice, represented by `Cond`, where the first
position represents the edge followed on a `True` evaluation of the `Guard`
+ no outgoing edge, represented by `None`
*)
type next_codomain = Uncond of node | Cond of (node * node) | None

type 'a control_flow_graph = {
  nodes : NodeSet.t;
  edges : next_codomain NodeMap.t;
  entry : node;
  exit : node;
  code : 'a list NodeMap.t;
}

(** Taken a miniimp program returns its control flow graph. *)
val translate_miniimp :
  ImpAst.program -> ImpAst.miniimp_simple control_flow_graph

(** 
Converts a Miniimp's control flow graph in a MiniRISC's control flow graph, which assumes
an infinite number of registers. For optimization, see the {b target_code} library which
optimizes the control flow graph to be usable on machines with limited registers. 
*)
val miniimp_cfg_to_minirisc :
  ImpAst.miniimp_simple control_flow_graph -> Minirisc.scomm control_flow_graph

(**
This function returns a string in the Dot language,
representing the MiniImp control flow graph in input.
@see <https://en.wikipedia.org/wiki/DOT_(graph_description_language)>
*)
val miniimp_cfg_to_dot : ImpAst.miniimp_simple control_flow_graph -> string

(**
This function returns a string in the Dot language, 
representing the MiniRISC control flow graph in input.
@see <https://en.wikipedia.org/wiki/DOT_(graph_description_language)>
*)
val minirisc_cfg_to_dot : Minirisc.scomm control_flow_graph -> string

(** Taken a control flow graph {b G}, it returns a mapping where each node {b N} is 
mapped to a list of nodes {b L} and a node {b X} is contained in {b L} iff
either {! Uncond}(N) or {! Cond}(N1, N2) where N1 = N or N2 = N is contained in the mapping
image of node X in the mapping {! control_flow_graph.edges} of G.*)
val compute_reversed_map : 'a control_flow_graph -> node list NodeMap.t

(** Given a node and its outgoing edges this function returns a string
representing the node and its outgoing edges in Dot format. *)
val next_codomain_to_string : node -> next_codomain -> string

(** Given a node, unwraps its label *)
val access_node: node -> int