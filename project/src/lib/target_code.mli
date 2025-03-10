open Minirisc

type mriscfg = scomm Cfg.control_flow_graph
type spill_metric = mriscfg -> register -> int

val chaitin_briggs_algorithm :
  mriscfg -> int -> spill_metric -> mriscfg * memory_loc * memory_loc
(**
Implementation of the chaitin-briggs algorithm @see <https://en.wikipedia.org/wiki/Chaitin%27s_algorithm> wikipedia.

- The first argument is the MiniRISC Control Flow Graph which needs to optimize the number of registers used.
- The second argument is the amount of registers we wish to compress to. 
- The third argument is the spill metric by which registers will be discriminated when choosing spilling,
@see <https://en.wikipedia.org/wiki/Spill_metric> wikipedia.
*)

val interference_graph_dot : mriscfg -> string
(** Given a MiniRISC's Control Flow Graph it returns a string in the DOT
    language which represents the interfernce graph of the registers defined in
    the graph. *)

val cost_metric : spill_metric
(** Simple heuristic which can be fed to {! chaitin_briggs_algorithm} to choose
    a different metric for spilling.

    This metric simply associates the register with the amount of read and write
    operations done on it throughout whole control flow graph. Since write
    operations require more instructions they are counted twice. *)

val translate_cfg_to_target : mriscfg -> int -> program
(** Generates the target code for the given MiniRISC Control Flow Graph using at
    most {b k} registers.

    Note:
    - We return memory locations for the input and output variables because this
      function does arbitrary optimization on the control flow graph
      instructions, in particular the Chaitin-Briggs coloring algorithm is used
      to merge non-conflicting registers.

    It returns a tuple of 4 elements which are, in order:

    + The list of MiniRISC instructions, on which head we have the first
      instruction to execute
    + A {!LabelMap} to integers mapping any given label to the instruction that
      label is applied to.
    + A {!memory_loc} representing the memory location of the input variable
    + A {!memory_loc} representing the memory location the output variable *)

val generate_target_code_file :
  ?register_number:int ->
  string ->
  check_undefinedness:bool ->
  target_file_path:string ->
  unit
(** Takes as first argument the path of a MiniImp's file and
    + An optional argument regarding the number of registers:
      the target code will then suppose only such an amount of registers, 
      which will be labelled from 0 to ({b register_number})-1.
      The number must be >= 4, otherwise this function fails. 
      The default value is 4.
    + A boolean indicating whether or not to perform a static analysis to check
      for undefined variables. If a variable is deemed possibly undefined, this
      function will fail.
    + The file path {b target_file_path} where the function will write the
      MiniRISC code. *)

val eval_risc_cfg : mriscfg -> registers:int -> value:int -> int
(** Taken a MiniRISC's CFG, a given number of registers (>= 4) and an integer
    value for the input variable this function may
    + return the value of the "out" variable if the CFG was generated properly,
      see the {! Cfg} library
    + loop indefinitely
    + fail, if the CFG is ill-formed
    + return an unspecified value, if the program accesses areas of memory
      illegaly, as in reading an unitialized variable *)

val compile_and_run_imp_from_file :
  ?register_number:int -> string -> input:int -> check_undefinedness:bool -> int
(** See {! generate_target_code_file} for the parameters meaning.

    This function, if it does not fail or the program does not diverge, returns
    the integer corresponding to the {b out} variable. *)

val interpret_from_file :
  input_file:string -> 
  value:int ->
  int
(** Takes a filepath of a MiniRISC file and runs the MiniRISC program and prints the result to stdout *)