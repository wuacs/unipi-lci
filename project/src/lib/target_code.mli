type mriscfg = Minirisc.scomm Cfg.control_flow_graph

val main_label : Minirisc.label

(**

*)
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
val translate_cfg_to_target: mriscfg -> int -> (Minirisc.comm List.t * int Minirisc.LabelMap.t)

(** Utility function which translates a mriscfg into a parsable string of MiniRISC code
which uses only the limited number of registers given as second argument *)
val generate_target_code_string: mriscfg -> int -> string

(**
Takes as first argument the path of a MiniImp's file and
+ An optionally a number of registers, the target code will then suppose only
such an amount of registers, which will be labelled from 0 to ({b register_number})-1.
The number must be >= 4, otherwise this function fails. The default value is 4.
+ A boolean indicating whether or not to perform a static analysis to check for undefined variables.
If a variable is deemed possibly undefined, this function will fail.
+ The file path {b target_file_path} where the function will write the MiniRISC code.
*)
val generate_target_code_file: 
    ?register_number:int ->
    string ->
    check_undefinedness:bool ->
    target_file_path:string -> unit

(** Taken a MiniRISC's CFG, a given number of registers (>= 4) and an integer value for the input
variable this function may
+ return the value of the "out" variable if the CFG was generated properly, see the {! Cfg} library
+ loop indefinitely
+ fail, if the CFG is ill-formed
+ return an unspecified value, if the program accesses areas of memory illegaly, as in reading an unitialized variable *)
val eval_risc_cfg: mriscfg -> registers:int -> value:int -> int

(**
See {! generate_target_code_file} for the parameters meaning. 

This function, if it does not fail or the program does not diverge, 
returns the integer corresponding to the {b out} variable.
*)
val compile_and_run_imp_from_file: 
    ?register_number:(int) ->
    string ->
    input:int ->
    check_undefinedness:bool ->
    int
