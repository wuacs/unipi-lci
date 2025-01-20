type mriscfg = Minirisc.scomm Cfg.control_flow_graph

val chaitin_briggs_algorithm: mriscfg -> int -> mriscfg

val get_live_ranges_dot_format: mriscfg -> string

(** This function takes a {! mriscfg} and the number of registers
available in the target architecture, and returns a tuple consisting of 

+ A list of Minirisc's commands, where the first position in the list represents the starting point for 
the execution of the program. 
+ A mapping from {! Minirisc.label}s to integers, which are in reality only naturals,
representing the starting instruction position of the label in the list.
*)
val generate_target_code: mriscfg -> int -> (Minirisc.comm List.t * int Minirisc.LabelMap.t)

(** Utility function which translates a mriscfg into a parsable string of MiniRISC code
which uses only the limited number of registers given as second argument *)
val generate_target_code_string: mriscfg -> int -> string