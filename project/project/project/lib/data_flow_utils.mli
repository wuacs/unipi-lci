type block_analysis_state = {
  in_set : Minirisc.RegisterSet.t;
  out_set : Minirisc.RegisterSet.t;
}

(** Initializes each block input and output state with the set of all registers *)
val initialize_with_top :
  Minirisc.scomm Cfg.control_flow_graph -> block_analysis_state Cfg.NodeMap.t

(** Initializes each block input and output state with the empty set *)
val initialize_with_bottom :
  Minirisc.scomm Cfg.control_flow_graph -> block_analysis_state Cfg.NodeMap.t

(** Extracts the defined registers in the node given as input *)
val defined_registers :
  Minirisc.scomm Cfg.control_flow_graph -> Cfg.node -> Minirisc.RegisterSet.t

(** Extracts the used registers in the node.
A used register is such a register which is read before being written. *)
val used_registers : 
  Minirisc.scomm Cfg.control_flow_graph -> Cfg.node -> Minirisc.RegisterSet.t

(** Gets all the registers defined in the CFG *)
val get_top : 
  Minirisc.scomm Cfg.control_flow_graph -> Minirisc.RegisterSet.t

val extract_written_register :
  Minirisc.scomm -> Minirisc.register option

val extract_read_registers :
  Minirisc.scomm -> Minirisc.RegisterSet.t
