type node = Label of int

module NodeSet : Set.S with type elt = node
module NodeMap : Map.S with type key = node
module ImpAst = Miniimp.ImpAst

(** A Node may have
1. one outgoing edge, represented by `Uncond`
2. two outgoing edges, representing a conditional choice, represented by `Cond`
3. no outgoing edge, represented by `None`
*)
type next_codomain = Uncond of node | Cond of (node * node) | None

type 'a language = 'a

type 'a control_flow_graph = {
  nodes : NodeSet.t;
  edges : next_codomain list NodeMap.t;
  entry : node;
  exit : node;
  code : 'a list NodeMap.t;
}

type coercable = Boolean of ImpAst.b_exp | Integer of ImpAst.a_exp

val in_register : Minirisc.register
val out_register : Minirisc.register
val guard_register : Minirisc.register

val translate_miniimp :
  ImpAst.program -> ImpAst.miniimp_simple control_flow_graph

val miniimp_cfg_to_minirisc :
  ImpAst.miniimp_simple control_flow_graph -> Minirisc.scomm control_flow_graph

val miniimp_cfg_to_dot : ImpAst.miniimp_simple control_flow_graph -> string

val minirisc_cfg_to_dot : Minirisc.scomm control_flow_graph -> string

(** Function to compute the reversed map of the edge graph of a CFG *)
val compute_reversed_map : 'a control_flow_graph -> node list NodeMap.t

(** Given a node `src` and its outgoing edges `edges` 
this function returns a string representing the node and its outgoing edges
in Dot language compliant string. *)
val next_codomain_to_string : node -> next_codomain list -> string