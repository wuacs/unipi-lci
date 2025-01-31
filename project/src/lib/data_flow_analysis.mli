module Utils = Data_flow_utils

val live_analysis_dot : Minirisc.scomm Cfg.control_flow_graph -> string
(** Returns a string in the Dot language in which each node contains the
    {b live_in} and {b live_out} sets of the registers defined in the MiniRISC
    control flow graph in input. This function uses {! liveness_analysis} to
    compute the liveness_analysis.

    For more information about Dot language
    @see <https://en.wikipedia.org/wiki/DOT_(graph_description_language)> *)

val defined_analysis_dot : Minirisc.scomm Cfg.control_flow_graph -> string
(** Returns a string in the Dot language in which each node contains the
    {b defined_in} and {b defined_out} sets of the registers defined in the
    MiniRISC control flow graph in input.

    For more information about Dot language
    @see <https://en.wikipedia.org/wiki/DOT_(graph_description_language)> *)

val check_for_undefinedness : Minirisc.scomm Cfg.control_flow_graph -> bool
(** Returns {b true} if the control flow graph contains might access a register
    which is undefined (not assigned before). *)

val liveness_analysis :
  Minirisc.scomm Cfg.control_flow_graph ->
  Utils.block_analysis_state Cfg.NodeMap.t
(** Returns a mapping from nodes to {!Utils.block_analysis_state}. Here
    {!Utils.block_analysis_state.in_set} represents the {b live_in} set of a
    block while {!Utils.block_analysis_state.out_set} represents the
    {b live_out} set. *)
