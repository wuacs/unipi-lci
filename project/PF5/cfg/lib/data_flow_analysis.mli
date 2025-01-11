module Register_set : Set.S with type elt = Minirisc.register

type local_liveness = {
    in_set: Register_set.t;
    out_set: Register_set.t
}

val liveness_analysis: Minirisc.scomm Cfg.control_flow_graph -> local_liveness Cfg.NodeMap.t

val defined_analysis: Minirisc.scomm Cfg.control_flow_graph -> bool