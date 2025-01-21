type mriscfg = Minirisc.scomm Cfg.control_flow_graph

(** The label assigned to the exiting label, i.e. miniRISC will assume, 
once arrived at this label, the program has ended *)
val exit_label : Minirisc.label

val chaitin_briggs_algorithm: mriscfg -> int -> (mriscfg * Minirisc.memory_loc * Minirisc.memory_loc)

(**
Given a MiniRISC's Control Flow Graph it returns a string in the DOT language
which represents the interfernce graph of the registers defined in the graph. *)
val interference_graph_dot: mriscfg -> string

(** 
Generates the target code for the given MiniRISC Control Flow Graph using at most {b k} registers.

Note:
- We return memory locations for the input and output variables because this function 
does arbitrary optimization on the control flow graph instructions, in particular the Chaitin-Briggs
coloring algorithm is used to merge non-conflicting registers.

It returns a tuple of 4 elements which are, in order:

+ The list of MiniRISC instructions, on which head we have the first instruction to execute
+ A {!LabelMap} to integers mapping any given label to the instruction that label is applied to.
+ A {!memory_loc} representing the memory location of the input variable
+ A {!memory_loc} representing the memory location the output variable
*)
val translate_cfg_to_target: mriscfg -> int -> (Minirisc.comm List.t * int Minirisc.LabelMap.t * Minirisc.memory_loc * Minirisc.memory_loc)

(** Utility function which translates a mriscfg into a parsable string of MiniRISC code
which uses only the limited number of registers given as second argument *)
val generate_target_code_string: mriscfg -> int -> string

(** Taken a MiniRISC's CFG, a given number of registers (>= 4) and an integer value for the input
variable this function may
+ return the value of the "out" variable if the CFG was generated properly, see the {! Cfg} library
+ loop indefinitely
+ fail, if the CFG is ill-formed
+ return an unspecified value, if the program accesses areas of memory illegaly, as in reading an unitialized variable *)
val eval: mriscfg -> int -> int -> int