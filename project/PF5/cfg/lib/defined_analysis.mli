type block_analysis_state = Data_flow_utils.block_analysis_state

val defined_analysis: Minirisc.scomm Cfg.control_flow_graph -> bool

val update_in: Minirisc.scomm Cfg.control_flow_graph -> Cfg.node -> block_analysis_state Cfg.NodeMap.t -> Cfg.node list Cfg.NodeMap.t -> block_analysis_state Cfg.NodeMap.t

val update_out: Minirisc.scomm Cfg.control_flow_graph -> Cfg.node -> block_analysis_state Cfg.NodeMap.t -> block_analysis_state Cfg.NodeMap.t

val compute_top: Minirisc.scomm Cfg.control_flow_graph -> Minirisc.RegisterSet.t