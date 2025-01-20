exception NotEnoughRegisters of string
exception IllFormedCfg of string

open Printf
open Minirisc
open Cfg

module Nodemap = Cfg.NodeMap
module Nodeset = Cfg.NodeSet
module RegSet = Minirisc.RegisterSet
module RegMap = Minirisc.RegisterMap

type mriscfg = Minirisc.scomm Cfg.control_flow_graph
type node = Cfg.node
type riscomm = Minirisc.scomm
type reg = Minirisc.register

type vertex = {id: reg; degree: int; cost: int; color: int}

let third (_, _, t) = t
let first (f, _, _) = f
let second (_, s, _) = s

(** Such register ID is assigned to registers
which need to be spilled. *)
let uncolored_register = Id 10000

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
    let degree1 = v1.degree in 
    let degree2 = v2.degree in
    if (degree1 = 0) && (degree2 = 0) then
      compare v1.id v2.id
    else if (degree1 = 0) then
      -1
    else if (degree2 = 0) then 
      1
    else
      let cost1 = (v1.cost / degree1) in
      let cost2 = (v2.cost / degree2) in
      if cost1 = cost2 then 
        compare v1.id v2.id
      else
        compare cost1 cost2 
end)


let print_vertexsetbydegree set = 
  VertexSetByDegree.iter (fun x -> printf "vertex %d has degree %d\n" (get_reg_id x.id) x.degree) set;
  print_string ("Number of registers in degree set: " ^ (string_of_int (VertexSetByDegree.cardinal set)))

let print_vertexsetbymetric set = 
  VertexSetBySpillMetric.iter (fun x -> printf "vertex %d has metric value %d\n" (get_reg_id x.id) x.cost) set;
  print_string ("Number of registers in metric set: " ^ (string_of_int (VertexSetBySpillMetric.cardinal set)))

let print_vertexsetbycolor set = 
  VertexSetByColor.iter (fun x -> printf "vertex %d has color %d\n" (get_reg_id x.id) x.color) set;
  print_string ("Number of registers in color set: " ^ (string_of_int (VertexSetByColor.cardinal set)))

(** The cost of a register is the amount of read operations done on it *)
let compute_cost_register (cfg : mriscfg) (reg: reg): int = 
  Nodemap.fold (fun _ codeblk curr -> curr + 
  (List.fold_left 
  (fun acc instruction -> 
    acc + 
    (match (RegSet.find_opt reg (Data_flow_analysis.Utils.extract_read_registers instruction)) with
          | Some _ -> 1
          | _ -> 0)) 0 codeblk)) cfg.code 0

(** Removes a vertex from a vertex set ordered by degree. Such set is used
for determining the current state of interference, i.e. the interference graph
passed as input tells us the interference, the vertex set keeps track for each node
of the remaining nodes in the interference graph. *)
let remove_vertex (interference_graph: RegSet.t RegMap.t) (gi: VertexSetByDegree.t)
  (regId: reg) = 
    VertexSetByDegree.filter_map (fun v -> 
    if v.id = regId then 
      None
  else 
    match RegSet.find_opt regId (RegMap.find v.id interference_graph) with
    | Some _ -> if v.degree > 0 then Some({v with degree = v.degree - 1}) else None
    | None -> Some v) gi

(** Computes the live ranges of the registers present
in the control flow graph in input. The registers in the graph are
retrieved using {! Data_flow_analysis.Utils.get_top} *)
let compute_live_ranges (cfg: mriscfg) =
  let mapState = Data_flow_analysis.liveness_analysis cfg in
  let graph = RegSet.fold (fun reg liveRangeSet ->
    RegMap.add reg RegSet.empty liveRangeSet
  ) (Data_flow_analysis.Utils.get_top cfg) RegMap.empty 
  in
  let add_edge_to_graph from_register to_register graph nodeId =
    if to_register = from_register then
      graph
    else
      begin
      printf "in node %d adding edge from reg %d to reg %d\n" 
      (access_node nodeId) (get_reg_id from_register) (get_reg_id to_register);
      RegMap.update from_register
      (fun edges -> match edges with
      | Some set -> Some (RegSet.add to_register set)
      | None -> Some (RegSet.singleton to_register)) graph
    end
  in
  Nodeset.fold (fun nodeId liveRangeSet ->
    let live_now = (Nodemap.find nodeId mapState).out_set in
    let res = List.fold_right (fun instruction (live_now, graph) ->
      let written = Data_flow_analysis.Utils.extract_written_register instruction in
      match written with
      | Some written -> 
        (RegSet.remove written (RegSet.union live_now (Data_flow_analysis.Utils.extract_read_registers instruction)), (RegSet.fold 
        (fun reg graph -> 
        (add_edge_to_graph reg written (add_edge_to_graph written reg graph nodeId) nodeId)) live_now graph))
      | None -> (live_now, graph)) (Nodemap.find nodeId cfg.code) (live_now, liveRangeSet)
      in (snd res)
  ) cfg.nodes graph

(** Given a miniRISC instruction it replaces each reference to 
register {b x} in the instruction passed as first argument with its mapping according to
the second argument. *)
let replace_register
  (instruction : riscomm)
  (mapping : reg -> reg) =
  match instruction with
  | Minirisc.Rtor (op, reg1, reg2, reg3) ->
      Minirisc.Rtor (op, mapping reg1, mapping reg2, mapping reg3)
  | Minirisc.Rtoi (op, reg1, imm, reg2) ->
      Minirisc.Rtoi (op, mapping reg1, imm, mapping reg2)
  | Minirisc.Rury (op, reg1, reg2) ->
      Minirisc.Rury (op, mapping reg1, mapping reg2)
  | Minirisc.Load (reg1, reg2) ->
      Minirisc.Load (mapping reg1, mapping reg2)
  | Minirisc.LoadI (imm, reg1) ->
      Minirisc.LoadI (imm, mapping reg1)
  | Minirisc.Store (reg1, reg2) ->
      Minirisc.Store (mapping reg1, mapping reg2)
  | Minirisc.Nop -> Minirisc.Nop

let rec chaitin_briggs_step1 
  (interference_graph : RegSet.t RegMap.t) 
  (registers_left : RegSet.t)
  (gi: VertexSetByDegree.t) 
  (gh : VertexSetBySpillMetric.t)
  (stack : VertexStack.t) 
  (k : int)
  (current_coloring : VertexSetByColor.t)
  (spilled_registers : RegSet.t) :
  (VertexSetByColor.t * RegSet.t) =
    let lowest = VertexSetByDegree.min_elt_opt gi in
    match lowest with
    | Some v -> 
      if (v.degree < k) then
        let () = VertexStack.push v stack in
        (chaitin_briggs_step1 interference_graph (RegSet.remove v.id registers_left) (remove_vertex interference_graph gi v.id) gh stack k current_coloring spilled_registers)
      else 
        (chaitin_briggs_step2 interference_graph registers_left gi gh stack k current_coloring spilled_registers)
    | None -> (chaitin_briggs_step3 interference_graph registers_left gi gh stack k current_coloring spilled_registers)
and chaitin_briggs_step2
  (interference_graph : RegSet.t RegMap.t) 
  (registers_left: RegSet.t)
  (gi: VertexSetByDegree.t) 
  (gh: VertexSetBySpillMetric.t)
  (stack : VertexStack.t) 
  (k : int)
  (current_coloring : VertexSetByColor.t) 
  (spilled_registers : RegSet.t) :
  (VertexSetByColor.t * RegSet.t) = 
  let rec find_max_by_metric setmetric = 
    match VertexSetBySpillMetric.min_elt_opt setmetric with
    | Some x -> (match RegSet.mem x.id registers_left with
                | true -> (x, setmetric)
                | false -> find_max_by_metric (VertexSetBySpillMetric.remove x setmetric))
    | None -> failwith "There are no nodes in the graph, thus step2 should have not been invoked"
  in
  let (best, gh) = find_max_by_metric gh
  in
  let () = VertexStack.push best stack 
  in
  chaitin_briggs_step1 interference_graph (RegSet.remove best.id registers_left) (remove_vertex interference_graph gi best.id) gh stack k current_coloring spilled_registers
and chaitin_briggs_step3 
  (interference_graph : RegSet.t RegMap.t) 
  (registers_left: RegSet.t)
  (gi: VertexSetByDegree.t) 
  (gh: VertexSetBySpillMetric.t)
  (stack : VertexStack.t) 
  (k : int)
  (current_coloring : VertexSetByColor.t) 
  (spilled_registers : RegSet.t) :
  (VertexSetByColor.t * RegSet.t) = 
  match (VertexStack.is_empty stack) with
  | true -> (current_coloring, spilled_registers)
  | false -> 
    (let v = (VertexStack.pop stack) in
    let v_set = (RegMap.find v.id interference_graph) in
    let (color, _) = (VertexSetByColor.fold (fun x (acc, b) ->
      match b with
      | false -> (match (RegSet.mem x.id v_set) with 
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
      chaitin_briggs_step3 interference_graph registers_left gi gh stack k (VertexSetByColor.add {v with color = color } (VertexSetByColor.remove v current_coloring)) spilled_registers
    else
      let spilled_id = (- get_reg_id v.id) in 
      chaitin_briggs_step1 interference_graph (RegSet.remove v.id registers_left)
      gi gh stack k (VertexSetByColor.add {v with color = spilled_id }
      (VertexSetByColor.remove v current_coloring)) (RegSet.add (Id spilled_id) spilled_registers))


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
let apply_spilling_to_cfg  (cfg : mriscfg) (spilled_registers : RegSet.t) (register_number : int) =
  let liveness_result = Data_flow_analysis.liveness_analysis cfg in
  let spill_register_local
  (register: reg) (blk: node) (cfg : mriscfg) (memory_location: int): riscomm list =
  match (Nodemap.find_opt blk cfg.code) with
  | Some blk_code -> 
      let reversed_instructions = List.rev blk_code in
      let available_registers = RegSet.diff (RegSet.remove register (RegSet.add_seq (Seq.init register_number (fun x -> Id x)) RegSet.empty)) (Nodemap.find blk liveness_result).out_set in
      let _ = match blk with Label t -> (printf "num of available registers %d in block %d\n" (RegSet.cardinal available_registers) t) in
      fst (List.fold_left (fun (new_list, available_registers) instruction -> 
        let read = Data_flow_analysis.Utils.extract_read_registers instruction in 
        let available_registers = RegSet.diff available_registers read in
        let (prefix_list, reading_register) = 
          if RegSet.mem register read then 
            begin
            match RegSet.choose_opt available_registers with
            | Some r -> 
              ([Minirisc.LoadI(memory_location, r); 
                Minirisc.Load(r, r); 
                replace_register instruction (fun x -> if x = register then r else x)] @ new_list, Some r)
            | None -> raise (NotEnoughRegisters "There are not enough registers to read spilled register")
            end
          else
            (new_list, None)
        in
        let written = Data_flow_analysis.Utils.extract_written_register instruction in
        let (complete_list, available_registers) =
          match written with
          | Some r when r = register -> 
              begin 
                match reading_register with
              | Some rr -> 
                  (match RegSet.choose_opt (RegSet.remove rr available_registers) with
                  | Some address -> 
                      ([Minirisc.LoadI(memory_location,address); Minirisc.Store(rr, address)] @ prefix_list,
                       available_registers)
                  | None -> raise (NotEnoughRegisters "Not enough registers to write spilled register"))
              | None -> 
                  let write_in_register = 
                    match RegSet.choose_opt available_registers with
                    | Some r -> r
                    | None -> raise (NotEnoughRegisters "Not enough registers for writing spilled register")
                  in
                  let address_register = 
                    match RegSet.choose_opt (RegSet.remove write_in_register available_registers) with
                    | Some r -> r
                    | None -> raise (NotEnoughRegisters "Not enough registers for writing address for storing")
                  in
                  ( [replace_register instruction (fun x -> if x = register then write_in_register else x); 
                    Minirisc.LoadI(memory_location, address_register);
                    Minirisc.Store(write_in_register, address_register)] @ prefix_list, 
                   available_registers)
              end
          | Some r -> (prefix_list, RegSet.add r available_registers)
          | None -> (prefix_list, available_registers)
        in
        if List.length new_list = List.length complete_list then
          (instruction :: complete_list, available_registers)
        else
          (complete_list, available_registers)
      ) ([], available_registers) reversed_instructions)
  | None -> raise (IllFormedCfg "The Cfg passed as input does not contain the needed block of code")
  in
  snd 
    (RegSet.fold 
    (fun register (memory, blocks) -> 
      (memory + 1, 
      Nodemap.fold 
      (fun node _ map -> (Nodemap.add node (spill_register_local register node cfg memory) map))
      blocks 
      Nodemap.empty)) spilled_registers (0, cfg.code))

(** Returns the CFG in input with each register changed to the register it is mapped in {! color_map},

- If the register is not present in {! color_map} the register is left unchanged.
*)
let apply_color_map_to_cfg
  (cfg : mriscfg)
  (color_map : reg RegMap.t) : mriscfg = 
  let replace_register_with_map reg =
    match RegMap.find_opt reg color_map with
    | Some new_reg -> new_reg
    | None -> uncolored_register  (* If no mapping is found, then it is considered spilled *)
  in
  let replace_block blk_code =
    List.map (Fun.flip replace_register (fun x -> replace_register_with_map x)) blk_code
  in
  let new_code = Nodemap.fold (fun blk blk_code acc ->
    Nodemap.add blk (replace_block blk_code) acc
  ) cfg.code Nodemap.empty in
  { cfg with code = new_code }


(** Returns a mapping for each register to a new register(the one it is substituted with).
Spilled registers are given as the fourth element in the result. *)
let chaitin_briggs_algorithm (cfg : mriscfg) (k : int) : mriscfg = 
    let g = compute_live_ranges cfg in
    let (vertexesByDegree, vertexesByMetric, vertexesByColor) = RegMap.fold (fun reg neighbors (setByDegree, setByMetric, setByColor) -> 
      let vertex = {id = reg; degree = (RegSet.cardinal neighbors); cost = (compute_cost_register cfg reg); color = (get_reg_id uncolored_register)} in
      (VertexSetByDegree.add vertex setByDegree, VertexSetBySpillMetric.add vertex setByMetric, VertexSetByColor.add vertex setByColor))
    g (VertexSetByDegree.empty, VertexSetBySpillMetric.empty, VertexSetByColor.empty) in
    print_vertexsetbycolor vertexesByColor;
    print_vertexsetbydegree vertexesByDegree;
    print_vertexsetbymetric vertexesByMetric;
    let (colors, spilled) = chaitin_briggs_step1 g (Data_flow_analysis.Utils.get_top cfg) vertexesByDegree vertexesByMetric (VertexStack.create ()) k vertexesByColor RegSet.empty in
    let color_map = VertexSetByColor.fold (fun v map -> RegMap.add v.id (Id v.color) map) colors RegMap.empty in
    let merged_cfg = apply_color_map_to_cfg cfg color_map in
    printf "Spilling size %d " (RegSet.cardinal spilled) ;
    {merged_cfg with code = apply_spilling_to_cfg merged_cfg spilled k}
  
let get_live_ranges_dot_format (cfg : mriscfg) : string = 
  let g = compute_live_ranges cfg in
  let other_registers start set =
    RegSet.fold (fun x acc -> acc ^ (Printf.sprintf "%d -> %d\n" (get_reg_id start) (get_reg_id x))) set "" 
  in
  let nodes_str =
    RegSet.fold
      (fun reg acc ->
        acc
        ^ Printf.sprintf "  %d [label=\"%d\"];\n" (get_reg_id reg) (get_reg_id reg))
      (Data_flow_analysis.Utils.get_top cfg) ""
  in

  let edges_str =
    RegMap.fold
      (fun src interf acc ->
        acc ^ other_registers src interf)
        g ""
  in

  let entry_str = (string_of_int (get_reg_id Cfg.in_register)) in
  let exit_str = (string_of_int (get_reg_id Cfg.out_register)) in

  (* Return the full graph's DOT string *)
  Printf.sprintf
    "digraph G {\n\
    \  // Entry node\n\
    \  %s [shape=ellipse, color=green];\n\
    \  // Exit node\n\
    \  %s [shape=ellipse, color=red];\n\
    %s%s}\n"
    entry_str exit_str nodes_str edges_str

let generate_target_code (cfg : mriscfg) (k : int): (Minirisc.comm list * int Minirisc.LabelMap.t)  =
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
            let blk_reversed = List.rev (Nodemap.find node cfg.code) in (* reversing for efficiency *)
            (* removing garbage instructions and promoting simple instructions to instructions *)
            let blk_mapped_reversed = (List.filter_map (fun x -> match (remove_garbage x) with
            | Some i -> Some(Minirisc.Simple(i))
            | None -> None) blk_reversed)
            in
            let blk_len = List.length blk_mapped_reversed in (* number of instructions in the filtered block *)
            Printf.printf "Current pos in node %d is %d blk filtered len is %d block unfiltered %d \n" (access_node node) curr_pos blk_len (List.length blk_reversed);
            (* flag the current block with a label, corresponding to its node identifier*)
            let mapping = LabelMap.add (Minirisc.Label l) curr_pos mapping in 
            match (Nodemap.find node cfg.edges) with
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
                mapping (second x1block)) x2 (third x1block + 1) in
                match Data_flow_analysis.Utils.extract_written_register (List.hd blk_reversed) with
                | None -> failwith "Ill-formed Control Flow Graph, the last instruction before a fork should be a write instruction" 
                | Some wr -> ((first x2block) @ (first x1block) @ (Minirisc.Cjump(wr, Label (access_node x1), Label (access_node x2))) :: blk_mapped_reversed, (second x2block), (third x2block))
              end
            | None -> (Jump(Label 1000)::blk_mapped_reversed, LabelMap.add (Label l) curr_pos mapping, curr_pos)
          end
      end
      in
      let res = rec_helper (chaitin_briggs_algorithm cfg k) (Minirisc.LabelMap.empty) (cfg.entry) 0
      in
      ((List.rev (first res)), second res)

let generate_target_code_string cfg k = 
  let target_code_output = generate_target_code cfg k in
  let labels = (Array.of_list (LabelMap.bindings (snd target_code_output))) in
  Array.sort (fun pos1 pos2 -> compare (snd pos1) (snd pos2)) labels;
  Array.iter (fun x -> (Printf.printf "is %d\n" (snd x))) labels;
  first (List.fold_left (fun (str, curr_pos, curr_label) instruction -> 
    let to_append = (minirisc_command_to_string instruction) ^ "\n" in
    let label = Array.get labels curr_label in 
    Printf.printf "Curr instruction is %d curr label is at position: %d\n" curr_pos (snd label);
    if curr_pos = (snd label) then 
      begin
      (str ^ (Printf.sprintf "L %d\t" (get_label_val (fst label))) ^ to_append, curr_pos+1, ((curr_label + 1) mod (Array.length labels)))
      end
    else
      ((str ^ to_append), curr_pos+1, curr_label)) ("", 0, 0) (fst target_code_output))