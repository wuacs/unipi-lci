type mriscfg = Minirisc.scomm Cfg.control_flow_graph

val chaitin_briggs_algorithm: mriscfg -> int -> mriscfg

val get_live_ranges_dot_format: mriscfg -> string

val generate_target_code: mriscfg -> Minirisc.comm List.t