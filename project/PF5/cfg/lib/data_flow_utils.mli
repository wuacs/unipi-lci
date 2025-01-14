type block_analysis_state = {
  in_set : Minirisc.RegisterSet.t;
  out_set : Minirisc.RegisterSet.t;
}

val initialize_with_bottom :
  Minirisc.scomm Cfg.control_flow_graph -> block_analysis_state Cfg.NodeMap.t
(** Initializes each block input and output state with the empty set *)

val extract_defined_registers :
  Minirisc.scomm Cfg.control_flow_graph -> Cfg.node -> Minirisc.RegisterSet.t
