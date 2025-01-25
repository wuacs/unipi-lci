exception NotEnoughRegisters of string
exception IllFormedCfg of string

open Minirisc
open Cfg

type mriscfg = scomm control_flow_graph
type spill_metric = mriscfg -> register -> int
type vertex = {id: register; degree: int; cost: int; color: int}

let first (f, _, _) = f
let second (_, s, _) = s
let third (_, _, t) = t

let main_label = Minirisc.Label 0
let exit_label = Minirisc.Label (-1)

module VertexStack = (struct
  type t = vertex Stack.t
  let create = Stack.create
  let push = Stack.push
  let pop = Stack.pop
  let is_empty = Stack.is_empty
end)

(** A set of Vertexes which ordering is based upon their degree, and
if they are equal on that value, upon their degree. *)
module VertexSetByDegree = Set.Make(struct
  type t = vertex
  let compare v1 v2 = 
    if compare v1.degree v2.degree = 0 then compare v1.id v2.id else compare v1.degree v2.degree
end)

module VertexSetByColor = Set.Make(struct
  type t = vertex
  let compare v1 v2 =
    if compare v1.color v2.color = 0 then compare v1.id v2.id else compare v1.color v2.color
end)

(**
Vertex set ordered by a metric which takes into account the cost,
in terms of the numbers of reads and writes of a register, 
and divides it by its degree. 

This metric is a lower is better metric, that is, the best
node to spill is that which is not used much, so that we push
less boilerplate instructions, and its remove would remove 
the most interference, which is what having an high degree means.
*)
module VertexSetBySpillMetric = Set.Make(struct
  type t = vertex
  let compare v1 v2 = 
    let cost1 = v1.cost in
    let cost2 = v2.cost in
    if cost1 = cost2 then 
      compare v1.id v2.id
    else
      compare cost1 cost2 
end)

let cost_metric (cfg : mriscfg) (reg: register): int = 
  NodeMap.fold (fun _ codeblk curr -> curr + 
  (List.fold_left 
  (fun acc instruction -> 
    acc + 
    (match 
    (RegisterSet.find_opt reg (Data_flow_analysis.Utils.extract_read_registers instruction)) with
      | Some _ -> 1
      | _ -> 0) +
    (match (Data_flow_analysis.Utils.extract_written_register instruction) with
    | Some x -> if x = reg then 2 else 0
    | _ -> 0)) 0 codeblk)) cfg.code 0

(** Removes a vertex from a vertex set ordered by degree. Such set is used
for determining the current state of interference, i.e. the interference graph
passed as input tells us the interference, the vertex set keeps track for each node
of the remaining nodes in the interference graph. *)
let remove_vertex (interference_graph: RegisterSet.t RegisterMap.t) (gi: VertexSetByDegree.t)
  (regId: register) = 
    VertexSetByDegree.filter_map (fun v -> 
    if v.id = regId then 
      None
  else 
    match RegisterSet.find_opt regId (RegisterMap.find v.id interference_graph) with
    | Some _ -> if v.degree > 0 then Some({v with degree = v.degree - 1}) else None
    | None -> Some v) gi

(** Computes the live ranges of the registers present
in the control flow graph in input. The registers in the graph are
retrieved using {! Data_flow_analysis.Utils.get_top} *)
let compute_live_ranges (cfg: mriscfg) =
  let mapState = Data_flow_analysis.liveness_analysis cfg in
  let graph = RegisterSet.fold (fun reg liveRangeSet ->
    RegisterMap.add reg RegisterSet.empty liveRangeSet
  ) (Data_flow_analysis.Utils.get_top cfg) RegisterMap.empty 
  in
  let add_edge_to_graph from_register to_register graph =
    if to_register = from_register then
      graph
    else
      begin
      RegisterMap.update from_register
      (fun edges -> match edges with
      | Some set -> Some (RegisterSet.add to_register set)
      | None -> Some (RegisterSet.singleton to_register)) graph
    end
  in
  NodeSet.fold (fun nodeId liveRangeSet ->
    let live_now = (NodeMap.find nodeId mapState).out_set in
    let res = List.fold_right (fun instruction (live_now, graph) ->
      let written = Data_flow_analysis.Utils.extract_written_register instruction in
      match written with
      | Some written -> 
        (RegisterSet.remove written (RegisterSet.union live_now (Data_flow_analysis.Utils.extract_read_registers instruction)), (RegisterSet.fold 
        (fun reg graph -> 
        (add_edge_to_graph reg written (add_edge_to_graph written reg graph))) live_now graph))
      | None -> (live_now, graph)) (NodeMap.find nodeId cfg.code) (live_now, liveRangeSet)
      in (snd res)
  ) cfg.nodes graph

(** Given a miniRISC instruction it replaces each reference to 
register {b x} in the instruction passed as first argument with its mapping according to
the second argument. *)
let replace_register
  (instruction : scomm)
  (mapping : register -> register) =
  match instruction with
  | Rtor (op, reg1, reg2, reg3) ->
      Rtor (op, mapping reg1, mapping reg2, mapping reg3)
  | Rtoi (op, reg1, imm, reg2) ->
      Rtoi (op, mapping reg1, imm, mapping reg2)
  | Rury (op, reg1, reg2) ->
      Rury (op, mapping reg1, mapping reg2)
  | Load (reg1, reg2) ->
      Load (mapping reg1, mapping reg2)
  | LoadI (imm, reg1) ->
      LoadI (imm, mapping reg1)
  | Store (reg1, reg2) ->
      Store (mapping reg1, mapping reg2)
  | Nop -> Nop

(** 
This function should be injective, as, together with {b unflag_spill_register}
need to satisfy the following, for any register {b r} with valid positive identifier.

unflag_spill_register(flag_spilled_register(r)) = r for every r AND
the register retuned from {b flag_spilled_register} should 
be identified with a negative number, as to indicate that it is an unvalid register.
*)
let flag_spilled_register reg = 
  Id (- (get_reg_id reg + 1))

let unflag_spill_register reg = 
  Id ((- get_reg_id reg) - 1)

let rec chaitin_briggs_step1 
  (interference_graph : RegisterSet.t RegisterMap.t) 
  (registers_left : RegisterSet.t)
  (gi: VertexSetByDegree.t) 
  (gh : VertexSetBySpillMetric.t)
  (stack : VertexStack.t) 
  (k : int)
  (current_coloring : VertexSetByColor.t)
  (spilled_registers : RegisterSet.t) :
  (VertexSetByColor.t * RegisterSet.t) =
    let lowest = VertexSetByDegree.min_elt_opt gi in
    match lowest with
    | Some v -> 
      if (v.degree < k) then
        let () = VertexStack.push v stack in
        (chaitin_briggs_step1 interference_graph (RegisterSet.remove v.id registers_left) (remove_vertex interference_graph gi v.id) gh stack k current_coloring spilled_registers)
      else
        (chaitin_briggs_step2 interference_graph registers_left gi gh stack k current_coloring spilled_registers)
    | None -> (chaitin_briggs_step3 interference_graph registers_left gi gh stack k current_coloring spilled_registers)
and chaitin_briggs_step2
  (interference_graph : RegisterSet.t RegisterMap.t) 
  (registers_left: RegisterSet.t)
  (gi: VertexSetByDegree.t)
  (gh: VertexSetBySpillMetric.t)
  (stack : VertexStack.t) 
  (k : int)
  (current_coloring : VertexSetByColor.t) 
  (spilled_registers : RegisterSet.t) :
  (VertexSetByColor.t * RegisterSet.t) = 
  let rec find_max_by_metric setmetric = 
    match VertexSetBySpillMetric.min_elt_opt setmetric with
    | Some x -> (match RegisterSet.mem x.id registers_left with
                | true -> (x, setmetric)
                | false -> find_max_by_metric (VertexSetBySpillMetric.remove x setmetric))
    | None -> failwith "There are no nodes in the graph, thus step2 should have not been invoked"
  in
  let (best, gh) = find_max_by_metric gh
  in
  let () = VertexStack.push best stack 
  in
  chaitin_briggs_step1 interference_graph (RegisterSet.remove best.id registers_left) (remove_vertex interference_graph gi best.id) gh stack k current_coloring spilled_registers
and chaitin_briggs_step3 
  (interference_graph : RegisterSet.t RegisterMap.t) 
  (registers_left: RegisterSet.t)
  (gi: VertexSetByDegree.t) 
  (gh: VertexSetBySpillMetric.t)
  (stack : VertexStack.t) 
  (k : int)
  (current_coloring : VertexSetByColor.t) 
  (spilled_registers : RegisterSet.t) :
  (VertexSetByColor.t * RegisterSet.t) = 
  match (VertexStack.is_empty stack) with
  | true -> (current_coloring, spilled_registers)
  | false -> 
    (let v = (VertexStack.pop stack) in
    let v_set = (RegisterMap.find v.id interference_graph) in
    let (color, _) = (VertexSetByColor.fold (fun x (acc, b) ->
      match b with
      | false -> (match (RegisterSet.mem x.id v_set) with 
                | true -> if (x.color < acc) then (acc, false) else 
                  begin 
                    if x.color = acc then
                      (acc+1, false)
                    else
                      (acc, true)
                    end  
                  | false -> (acc, b))
      | true -> (acc, b)) current_coloring (0, false)) in
    if color < k then
      chaitin_briggs_step3 
      interference_graph 
      registers_left gi gh stack k 
      (VertexSetByColor.add {v with color = color } 
      (VertexSetByColor.remove v current_coloring)) spilled_registers
    else
      let spilled_id = (flag_spilled_register v.id) in 
      chaitin_briggs_step1 interference_graph (RegisterSet.remove v.id registers_left)
      gi gh stack k (VertexSetByColor.add {v with color = (get_reg_id spilled_id) }
      (VertexSetByColor.remove v current_coloring)) (RegisterSet.add spilled_id spilled_registers))


(** Given a CFG in input, each register that is included in the 
{! spilled_registers} set is considered as a register that needs to be spilled.
Each implicit read of any such register {b R} in any instruction {b X}
requires the insertion of the following minirisc instructions:

+ LOADI M Q
+ LOAD Q Q
+ X but with references to R replaced with Q

where {b Q} is choosen randomly among the free registers in that point in the code and {b M} is a
memory address which is given randomly to each spilled register, starting from 0.
In addition, for every operation X' which writes to {b R} the following instructions are produced:

+ X' but with references to R replaced by Q'
+ LOADI M Q''
+ STORE Q' Q''

{b Note: } if instruction {b i} both reads and writes to {b R} then both list of instructions are produced and concatenated
with the exception of the redundant copy of {b i} with references to {b R} updated and Q is reused which means Q' = Q.


{b Warning:} The available registers are considered to be numbered from  0 to {b register_number} - 1, if the 
number of available register is not enough to spill registers (because, for example, there are no reserved temporary registers 
or other choices this function fails with)
*)
let apply_spilling_to_cfg  (cfg : mriscfg) (spilled_registers : RegisterSet.t) (register_number : int) =
  RegisterSet.iter (fun x -> Printf.printf "Spilled register %d\n" (get_reg_id x)) spilled_registers;
  let liveness_result = Data_flow_analysis.liveness_analysis cfg in
  let spill_register_local
  (register: register) (blk: node) (cfg : mriscfg) (memory_location: memory_address): scomm list =
  match (NodeMap.find_opt blk cfg.code) with
  | Some blk_code -> 
      let reversed_instructions = List.rev blk_code in
      let available_registers = RegisterSet.diff (RegisterSet.remove register (RegisterSet.add_seq (Seq.init register_number (fun x -> Id x)) RegisterSet.empty)) (NodeMap.find blk liveness_result).out_set in
      fst (List.fold_left (fun (new_list, available_registers) instruction -> 
        let read = Data_flow_analysis.Utils.extract_read_registers instruction in 
        let written = Data_flow_analysis.Utils.extract_written_register instruction in
        let available_registers = RegisterSet.diff available_registers read in
        let (prefix_list, reading_register) = 
          if RegisterSet.mem register read then 
            begin
            match RegisterSet.choose_opt available_registers with
            | Some r ->
              ([Minirisc.LoadI(get_memory_address memory_location, r); 
                Minirisc.Load(r, r); 
                replace_register instruction (fun x -> if x = register then r else x)] @ new_list, Some r)
            | None -> raise (NotEnoughRegisters "There are not enough registers to read spilled register")
            end
          else
            (new_list, None)
        in
        let (complete_list, available_registers) =
          match written with
          | Some r when r = register -> 
              begin 
                match reading_register with
              | Some rr -> 
                  (match RegisterSet.choose_opt (RegisterSet.remove rr available_registers) with
                  | Some address -> 
                      ([Minirisc.LoadI(get_memory_address memory_location, address); Minirisc.Store(rr, address)] @ prefix_list,
                       available_registers)
                  | None -> raise (NotEnoughRegisters "Not enough registers to write spilled register"))
              | None -> 
                  let write_in_register = 
                    match RegisterSet.choose_opt available_registers with
                    | Some r -> r
                    | None -> raise (NotEnoughRegisters "Not enough registers for writing spilled register")
                  in
                  let address_register = 
                    match RegisterSet.choose_opt (RegisterSet.remove write_in_register available_registers) with
                    | Some r -> r
                    | None -> raise (NotEnoughRegisters "Not enough registers for writing address for storing")
                  in
                  ( [replace_register instruction (fun x -> if x = register then write_in_register else x); 
                    Minirisc.LoadI(get_memory_address memory_location, address_register);
                    Minirisc.Store(write_in_register, address_register)] @ prefix_list, 
                   available_registers)
              end
          | Some r when ((RegisterSet.mem r read) <> true) && (get_reg_id r) >= 0 -> (prefix_list, RegisterSet.add r available_registers)
          | _ -> (prefix_list, available_registers)
        in
        if List.length new_list = List.length complete_list then
          (instruction :: complete_list, available_registers)
        else
          (complete_list, available_registers)
        ) ([], available_registers) reversed_instructions)
  | None -> raise (IllFormedCfg "The Cfg passed as input does not contain the needed block of code")
  in
  let (_, mapping, blocks) = 
    (RegisterSet.fold 
    (fun register (current_address, spilled_mapping, blocks) -> 
      (current_address + 1, 
      RegisterMap.add (unflag_spill_register register) (Address current_address) spilled_mapping,
      NodeMap.fold 
      (fun node _ map -> 
          (NodeMap.add node (spill_register_local register node {cfg with code = map} (Address current_address)) map))
        blocks 
        blocks)) spilled_registers (0, RegisterMap.empty, cfg.code))
  in
    (blocks, mapping)

(** Returns the CFG in input with each register changed to the register it is mapped in {! color_map},

- If the register is not present in {! color_map} the register is left unchanged.
*)
let apply_color_map_to_cfg
  (cfg : mriscfg)
  (color_map : register RegisterMap.t) : mriscfg = 
  let replace_register_with_map reg =
    match RegisterMap.find_opt reg color_map with
    | Some new_reg -> new_reg
    | None -> failwith "There should be a mapping" 
  in
  let replace_block blk_code =
    List.map (Fun.flip replace_register (fun x -> replace_register_with_map x)) blk_code
  in
  let new_code = NodeMap.fold (fun blk blk_code acc ->
    NodeMap.add blk (replace_block blk_code) acc
  ) cfg.code NodeMap.empty in
  { cfg with code = new_code }

(** Returns the optimized Control Flow Graph, and two memory locations,
which are, in order, of the input variable and output variable. 
The memory locations are needed since the optimization may have mapped them
in arbitrary locations, so that one can generate working target code once 
by loading the wanted values in the input memory location and extracting the
output value by reading from the output location.
*)
let chaitin_briggs_algorithm (cfg : mriscfg) (k : int) (heuristic : spill_metric) : (mriscfg * memory_loc * memory_loc) = 
    if (k < 4) then failwith "Register number must be at least 4" else
    let uncolored_register = Id 10000 in
    let g = compute_live_ranges cfg in
    let (vertexesByDegree, vertexesByMetric, vertexesByColor) = RegisterMap.fold (fun reg neighbors (setByDegree, setByMetric, setByColor) -> 
      let vertex = {id = reg; degree = (RegisterSet.cardinal neighbors); cost = (heuristic cfg reg); color = (get_reg_id uncolored_register)} in
      (VertexSetByDegree.add vertex setByDegree, VertexSetBySpillMetric.add vertex setByMetric, VertexSetByColor.add vertex setByColor))
    g (VertexSetByDegree.empty, VertexSetBySpillMetric.empty, VertexSetByColor.empty) in
    let (colors, spilled_registers) = chaitin_briggs_step1 g (Data_flow_analysis.Utils.get_top cfg) vertexesByDegree vertexesByMetric (VertexStack.create ()) (k-2) vertexesByColor RegisterSet.empty in
    let color_map = VertexSetByColor.fold (fun v map -> RegisterMap.add v.id (Id v.color) map) colors RegisterMap.empty in
    let merged_cfg = apply_color_map_to_cfg cfg color_map in
    let (spilled_code, spilled_memory_mapping) = apply_spilling_to_cfg merged_cfg spilled_registers k in
    let spilled_cfg = {merged_cfg with code = spilled_code} in
    let output_location = 
    match RegisterMap.find_opt out_register spilled_memory_mapping with
    | Some addr -> Memory addr
    | None -> Register (RegisterMap.find out_register color_map) in
    let input_location = 
    match RegisterMap.find_opt in_register spilled_memory_mapping with
    | Some addr -> Memory addr
    | None -> Register (RegisterMap.find in_register color_map)
    in
    (spilled_cfg, input_location, output_location)

    
let interference_graph_dot (cfg : mriscfg) : string = 
  let g = compute_live_ranges cfg in
  let other_registers start set =
    RegisterSet.fold (fun x acc -> acc ^ (Printf.sprintf "%d -> %d\n" (get_reg_id start) (get_reg_id x))) set "" 
  in
  let nodes_str =
    RegisterSet.fold
      (fun reg acc ->
        acc
        ^ Printf.sprintf "  %d [label=\"%d\"];\n" (get_reg_id reg) (get_reg_id reg))
      (Data_flow_analysis.Utils.get_top cfg) ""
  in

  let edges_str =
    RegisterMap.fold
      (fun src interf acc ->
        acc ^ other_registers src interf)
        g ""
  in

  let entry_str = (string_of_int (get_reg_id in_register)) in
  let exit_str = (string_of_int (get_reg_id out_register)) in

  (* Return the full graph's DOT string *)
  Printf.sprintf
    "digraph G {\n\
    \  // Entry node\n\
    \  %s [shape=ellipse, color=green];\n\
    \  // Exit node\n\
    \  %s [shape=ellipse, color=red];\n\
    %s%s}\n"
    entry_str exit_str nodes_str edges_str

(** Generates the target code for the given MiniRISC Control Flow Graph using at most {b k} registers.

Note:
- We return memory locations for the input and output variables because this function 
does arbitrary optimization on the control flow graph instructions, in particular the Chaitin-Briggs
coloring algorithm is used to merge non-conflicting registers.

It returns a tuple of 2 elements which are, in order:

+ The list of MiniRISC instructions, on which head we have the first instruction to execute
+ A {!LabelMap} to integers mapping any given label to the instruction that label is applied to.
*)
let translate_cfg_to_target (cfg : mriscfg) (k : int):
  (comm list * int LabelMap.t)  =
  let generate_input_instructions input_loc free = 
    match input_loc with
    | Memory m -> [LoadI(get_memory_address m, free); Store(free, in_register)]
    | Register r -> [Rury(Copy, in_register, r)] in 
  let generate_output_instructions output_loc free = 
    match output_loc with
    | Memory m -> [LoadI(get_memory_address m, free); Load(free, free); Rury(Copy, free, out_register)]
    | Register r -> [Rury(Copy, r, out_register)]
  in
  let remove_garbage (scomm : Minirisc.scomm) : Minirisc.scomm option = 
    match scomm with
    | Minirisc.Rury(op, r1, r2) -> 
      begin
      match op with
      | Minirisc.Copy -> if ((get_reg_id r1) = (get_reg_id r2)) then None else Some(scomm)
      | _ -> Some (scomm)
      end
    | Minirisc.Rtoi(op, r1, i, r2) ->
      begin
      match op with
      | Minirisc.AddI | Minirisc.SubI -> if (i = 0 && (get_reg_id r1) = (get_reg_id r2)) then None else Some(scomm) 
      | _ -> Some (scomm)
      end
    | _ -> Some(scomm)
  in
  (* Labels are the nodes identifiers! 
  Since nodes identifiers are positive numbers the exit label is set to a negative number.*)
  let rec rec_helper 
  (cfg: mriscfg)
  (mapping : int Minirisc.LabelMap.t) 
  (node : node) 
  (curr_pos : int) : 
  (Minirisc.comm list * int Minirisc.LabelMap.t * int) =
    match node with
    | Label l -> 
      begin
        if (Minirisc.LabelMap.mem (Minirisc.Label l) mapping) then 
          ([], Minirisc.LabelMap.empty, curr_pos - 1)
        else
          begin
            let blk_reversed = List.rev (NodeMap.find node cfg.code) in (* reversing for efficiency *)
            (* removing garbage instructions and promoting simple instructions to instructions *)
            let blk_mapped_reversed = (List.filter_map (fun x -> match (remove_garbage x) with
            | Some i -> Some(Minirisc.Simple(i))
            | None -> None) blk_reversed)
            in
            let blk_len = List.length blk_mapped_reversed in
            (* flag the current block with a label, corresponding to its node identifier*)
            let mapping = LabelMap.add (Minirisc.Label l) curr_pos mapping in 
            match (NodeMap.find node cfg.edges) with
            | Uncond x -> 
              begin
                let xblock = rec_helper cfg mapping x (curr_pos + blk_len + 1) in (* make the block start after the current one *)
                ((first xblock) @ (Minirisc.Jump (Label (access_node x))) :: blk_mapped_reversed, 
                LabelMap.union (fun _ k1 k2 -> if (k1 <> k2) then failwith "The same has been labeled twice" else Some(k1)) 
                mapping (second xblock),
                third xblock)
              end
            | Cond(x1, x2) -> 
              begin
                let x1block = rec_helper cfg mapping x1 (curr_pos + blk_len + 1) in
                let x2block = rec_helper cfg ( LabelMap.union (fun _ k1 k2 -> if (k1 <> k2) then failwith "The same has been labeled twice" else Some(k1)) 
                mapping (second x1block)) x2 (third x1block+1) in
                match Data_flow_analysis.Utils.extract_written_register (List.hd blk_reversed) with
                | None -> failwith "Ill-formed Control Flow Graph, the last instruction before a fork should be a write instruction" 
                | Some wr -> ((first x2block) @ (first x1block) @ (Minirisc.Cjump(wr, Label (access_node x1), Label (access_node x2))) :: blk_mapped_reversed, (second x2block), (third x2block))
              end
            | None -> (Jump(exit_label)::blk_mapped_reversed, mapping, curr_pos+ blk_len)
          end
      end
      in
      let (optimized_cfg, input_location, output_location) = (chaitin_briggs_algorithm cfg k cost_metric) in
      let code_injected = 
        let updated_entry = NodeMap.add optimized_cfg.entry ((generate_input_instructions input_location first_free_register)@(NodeMap.find optimized_cfg.entry optimized_cfg.code)) optimized_cfg.code in
        NodeMap.add optimized_cfg.exit ((NodeMap.find optimized_cfg.exit updated_entry)@(generate_output_instructions output_location first_free_register)) updated_entry
      in
      let (reversed_code, label_map, _) = rec_helper ({optimized_cfg with code = code_injected}) LabelMap.empty optimized_cfg.entry 0 
      in
      let number_of_instructions = List.length reversed_code in
      (List.rev (Simple(Nop)::reversed_code), (LabelMap.add exit_label number_of_instructions label_map))


let generate_target_code_string cfg k = 
  let (target_code, label_map) = translate_cfg_to_target cfg k in
  (* Labels have an instruction number associated, so we sort them and while
  we convert each instruction to a string we check whether the current instruction has 
  a correspondent label attached *)
  let labels = (Array.of_list (LabelMap.bindings label_map)) in
  Array.sort (fun pos1 pos2 -> compare (snd pos1) (snd pos2)) labels;
  first (List.fold_left (fun (str, curr_pos, curr_label) instruction -> 
    let to_append = (minirisc_command_to_string instruction) ^ "\n" in
    let label = Array.get labels curr_label in 
    if curr_pos = (snd label) then 
      begin
      let label_str = Printf.sprintf "%s:" 
      (if ((fst label) = main_label) then "main" else "l" ^ (string_of_int (get_label_val ((fst label))))) in
      (str ^ (Printf.sprintf "%s\t" label_str) ^ to_append, curr_pos+1, ((curr_label + 1) mod (Array.length labels)))
      end
    else
      ((str ^ to_append), curr_pos+1, curr_label)) ("", 0, 0) target_code)

let eval_risc_cfg cfg ~registers:k ~value:input =
  (* Function to generate initialization code for the input variable, *)
  let (instructions, label_mapping) = translate_cfg_to_target cfg k in
  let array_instructions = Array.of_list instructions in
  (* Helper function to fetch a value from memory *)
  let fetch_memory memory address =
    try MemoryMap.find address memory with Not_found -> 0
  in
  (* Helper function to update a value in memory *)
  let update_memory memory address value =
    MemoryMap.add address value memory
  in

  let fetch_register registers regid = 
    try RegisterMap.find regid registers with Not_found -> 0
  in
  
  let update_register registers regid value = 
    RegisterMap.add regid registers value
  in

  (* Recursive function to execute a single instruction *)
  let execute_instruction instruction memory registers pc =
    match instruction with
    | Simple scomm ->
        begin
          match scomm with
          | Nop -> (memory, registers, pc + 1)
          | Rtor (op, r1, r2, r3) ->
              let v1 = fetch_register registers r1 in
              let v2 = fetch_register registers r2 in
              let result = 
                match op with
                | Add -> v1 + v2
                | Sub -> v1 - v2
                | Mult -> v1 * v2
                | And -> if v1 <> 0 && v2 <> 0 then 1 else 0
                | Less -> if v1 < v2 then 1 else 0
              in
              (memory, update_register result r3 registers, pc + 1)
          | Rtoi (op, r1, imm, r2) ->
              let v1 = fetch_register registers r1 in
              let result =
                match op with
                | AddI -> v1 + imm
                | SubI -> v1 - imm
                | MultI -> v1 * imm
                | AndI -> if v1 <> 0 && imm <> 0 then 1 else 0
              in
              (memory, update_register result r2 registers, pc + 1)
          | Rury (op, r1, r2) ->
              let v1 = fetch_register registers r1 in
              let result =
                match op with
                | Not -> if v1 = 0 then 1 else 0
                | Copy -> v1
              in
              (memory, update_register result r2 registers, pc + 1)
          | Load (r1, r2) ->
              let address = fetch_register registers r1 in
              let value = fetch_memory memory (Address address) in
              (memory, update_register value r2 registers, pc + 1)
          | LoadI (imm, r1) ->
              (memory, update_register imm r1 registers, pc + 1)
          | Store (r1, r2) ->
              let address = fetch_register registers r2 in
              let value = fetch_register registers r1 in
              (update_memory memory (Address address) value, registers, pc + 1)
        end
    | Jump label ->
        let label_val = label in
        (memory, registers, (LabelMap.find label_val label_mapping)) (* Assuming PC is in Id 0 *)
    | Cjump (r, l1, l2) ->
        let v = RegisterMap.find r registers in
        let next_label = if v <> 0 then l1 else l2 in
        (memory, registers, (LabelMap.find next_label label_mapping) )
  in

  (* Function to iterate over instructions *)
  let rec run pc memory registers =
    if (pc <> (Array.length array_instructions)) then
      let instruction = array_instructions.(pc) in
      let (memory, registers, pc) = execute_instruction instruction memory registers pc in
      let next_pc = pc in
        run next_pc memory registers
    else
      (memory, registers)
  in
  let (_, registers) = run 0 MemoryMap.empty (RegisterMap.singleton in_register input)
  in
  RegisterMap.find out_register registers
let generate_target_code_file
  ?(register_number = 4)
  miniimp_file_path
  ~check_undefinedness
  ~target_file_path : unit = 
  if register_number < 4 then failwith "Register number must be >= 4";
  let in_channel = open_in miniimp_file_path in
  Fun.protect ~finally: (fun _ -> close_in in_channel)
  (fun _ ->
    match Miniimp.parse_with_errors (Lexing.from_channel (open_in miniimp_file_path)) with
    Some imp_prog -> 
      begin
        let risc_cfg = miniimp_cfg_to_minirisc (translate_miniimp imp_prog) in
        if check_undefinedness && (Data_flow_analysis.check_for_undefinedness risc_cfg) then
          failwith "Undefined variable found"  
        else
          let out_channel = open_out target_file_path in
          Fun.protect ~finally: (fun _ -> close_out out_channel) 
          (fun _ -> 
            let oc = open_out target_file_path in
            Printf.fprintf oc "%s" (generate_target_code_string risc_cfg register_number))
      end
  | None -> failwith (Printf.sprintf "Error while parsing file %s\n" miniimp_file_path))

let compile_and_run_imp_from_file
  ?(register_number = 4)
  miniimp_file_path
  ~input
  ~check_undefinedness : int = 
  if register_number < 4 then failwith "Register number must be >= 4";
  let in_channel = open_in miniimp_file_path in
  Fun.protect ~finally: (fun _ -> close_in in_channel)
  (fun _ -> match Miniimp.parse_with_errors (Lexing.from_channel in_channel) with
  | Some imp_prog -> 
      let risc_cfg = miniimp_cfg_to_minirisc (translate_miniimp imp_prog) in
      if check_undefinedness && (Data_flow_analysis.check_for_undefinedness risc_cfg) then
          failwith "Undefined variable found"  
      else
        eval_risc_cfg risc_cfg ~registers:register_number ~value:input
  | None -> failwith (Printf.sprintf "Error while parsing file %s\n" miniimp_file_path))
