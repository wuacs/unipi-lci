val liveness_analysis: Minirisc.scomm Cfg.control_flow_graph -> Data_flow_utils.block_analysis_state Cfg.NodeMap.t

val update_in: 
    Minirisc.scomm Cfg.control_flow_graph -> 
    Cfg.node ->
    Data_flow_utils.block_analysis_state Cfg.NodeMap.t ->
    Data_flow_utils.block_analysis_state Cfg.NodeMap.t

val update_out: 
    Minirisc.scomm Cfg.control_flow_graph ->
    Cfg.node ->
    Data_flow_utils.block_analysis_state Cfg.NodeMap.t -> 
    Cfg.node list Cfg.NodeMap.t ->
    Data_flow_utils.block_analysis_state Cfg.NodeMap.t