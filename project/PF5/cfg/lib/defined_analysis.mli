module RegisterSet : Set.S with type elt = Minirisc.register

type block_analysis_state = {
    in_set: RegisterSet.t;
    out_set: RegisterSet.t
}

val defined_analysis: Minirisc.scomm Cfg.control_flow_graph -> bool

val update_in: Minirisc.scomm Cfg.control_flow_graph -> Cfg.node -> block_analysis_state Cfg.NodeMap.t -> block_analysis_state Cfg.NodeMap.t

val update_out: Minirisc.scomm Cfg.control_flow_graph -> Cfg.node -> block_analysis_state Cfg.NodeMap.t -> block_analysis_state Cfg.NodeMap.t

val compute_top: Minirisc.scomm Cfg.control_flow_graph -> RegisterSet.t