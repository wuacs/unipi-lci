module Utils = Data_flow_utils
module LiveAnalysis = Liveness_analysis

(** Computes the live analysis and a string in the Dot language 
in which each node contains the live_in and live_out sets *)
val live_analysis_dot : Minirisc.scomm Cfg.control_flow_graph -> string

(** Computes the defined analysis and a string in the Dot language 
in which each node contains the defined_in and defined_out sets *)
val defined_analysis_dot : Minirisc.scomm Cfg.control_flow_graph -> string