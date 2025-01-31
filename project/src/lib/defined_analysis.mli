type block_analysis_state = Data_flow_utils.block_analysis_state

val defined_analysis :
  Minirisc.scomm Cfg.control_flow_graph ->
  Data_flow_utils.block_analysis_state Cfg.NodeMap.t
(** Returns a {!Data_flow_utils.block_analysis_state} where for any node {!x}
    its {!in_set} represents the defined variables at its start and {!out_set}
    represents the defined variables after the node execution *)

val check_for_undefinedness : Minirisc.scomm Cfg.control_flow_graph -> bool
(** Returns true if the control flow graph contains a, potentially, undefined
    register *)

val update_in :
  Minirisc.scomm Cfg.control_flow_graph ->
  Cfg.node ->
  block_analysis_state Cfg.NodeMap.t ->
  Cfg.node list Cfg.NodeMap.t ->
  block_analysis_state Cfg.NodeMap.t
(** Updates the set {!in_set} corresponding to the node given as parameter, in
    the control flow graph given as parameter, with current global state as
    given by the parameter as

    in_set' = ... *)

val update_out :
  Minirisc.scomm Cfg.control_flow_graph ->
  Cfg.node ->
  block_analysis_state Cfg.NodeMap.t ->
  block_analysis_state Cfg.NodeMap.t
(** Updates the set {!out_set} corresponding to the node given as parameter, in
    the control flow graph given as parameter, with current global state as
    given by the parameter as:

    out_set' = ...*)
