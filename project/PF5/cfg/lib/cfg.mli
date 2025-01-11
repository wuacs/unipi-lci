type node = Label of int

module NodeSet : Set.S with type elt = node

module NodeMap : Map.S with type key = node

(** A Node may have
1. one outgoing edge, represented by `Uncond`
2. two outgoing edges, representing a conditional choice, represented by `Cond`
3. no outgoing edge, represented by `None`
*)
type next_codomain = |  Uncond of node
                     |  Cond of (node * node)
                     |  None

type 'a language = 'a

type 'a control_flow_graph = {
  nodes : NodeSet.t;
  edges : (next_codomain list) NodeMap.t;
  entry : node;
  exit : node;
  code : ('a list) NodeMap.t;
}

type coercable = Boolean of Miniimp_ast.b_exp
                | Integer of Miniimp_ast.a_exp

val translate_miniimp: Miniimp_ast.program -> Miniimp_ast.miniimp_simple control_flow_graph

val miniimp_cfg_to_minirisc: Miniimp_ast.miniimp_simple control_flow_graph -> Minirisc.scomm control_flow_graph

val miniimp_cfg_to_dot: Miniimp_ast.miniimp_simple control_flow_graph -> string

(* Function to compute the reversed map *)
val compute_reversed_map: 'a control_flow_graph -> node list NodeMap.t

val in_register: Minirisc.register

val out_register: Minirisc.register

val guard_register: Minirisc.register