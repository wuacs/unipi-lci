module Nodemap = Cfg.NodeMap
module Nodeset = Cfg.NodeSet
module RegSet = Minirisc.RegisterSet
module RegMap = Minirisc.RegisterMap

type mriscfg = Minirisc.scomm Cfg.control_flow_graph
type node = Cfg.node
type riscomm = Minirisc.scomm
type reg = Minirisc.register
type interference_graph = (int * RegSet.t) RegMap.t
type heuristic = int RegMap.t

type vertex = {id: reg; degree: int; cost: int; color: int}

module VertexStack = (struct
  type t = vertex Stack.t
  let create = Stack.create
  let push = Stack.push
  let pop = Stack.pop
  let is_empty = Stack.is_empty
end)

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

module VertexSetBySpillMetric = Set.Make(struct
  type t = vertex
  let compare v1 v2 = 
    let degree1 = v1.degree in 
    let degree2 = v2.degree in
    if (degree1 == 0) && (degree2 == 0) then
      0
    else if (degree1 == 0) then
      -1
    else if (degree2 == 0) then 
      1
    else
      compare (v1.cost / degree1) (v2.cost / degree2)
end)

let compute_live_ranges (cfg: mriscfg) =
  let mapState = Data_flow_analysis.liveness_analysis cfg in
  let graph = RegSet.fold (fun reg liveRangeSet ->
    RegMap.add reg RegSet.empty liveRangeSet
  ) (Data_flow_analysis.Utils.get_top cfg) RegMap.empty 
  in
  Nodeset.fold (fun nodeId liveRangeSet ->
    let live_now = (Nodemap.find nodeId mapState).out_set in
    let res = List.fold_right (fun instruction (live_now, graph) ->
      let written = Data_flow_analysis.Utils.extract_written_register instruction in
      match written with
      | Some written -> 
        (RegSet.remove written (RegSet.union live_now (Data_flow_analysis.Utils.extract_read_registers instruction)), (RegSet.fold 
        (fun reg graph -> (RegMap.update written (fun edges -> match edges with
        | Some set -> Some(RegSet.add reg set) 
        | None -> Some(RegSet.singleton reg)) graph)) live_now graph))
      | None -> (live_now, graph)) (Nodemap.find nodeId cfg.code) (live_now, liveRangeSet)
      in (snd res)
  ) cfg.nodes graph


(** The cost of a register is the amount of read operations done on it *)
let compute_cost_register (cfg : mriscfg) (reg: reg): int = 
  Nodemap.fold (fun _ codeblk curr -> curr + 
  (List.fold_left 
  (fun acc instruction -> 
    acc + 
    (match (RegSet.find_opt reg (Data_flow_analysis.Utils.extract_read_registers instruction)) with
          | Some _ -> 1
          | _ -> 0)) 0 codeblk)) cfg.code 0

let remove_vertex 
  (interference_graph: RegSet.t RegMap.t) 
  (gi: VertexSetByDegree.t) 
  (regId: reg) = 
  VertexSetByDegree.filter_map (fun v -> match RegSet.find_opt regId (RegMap.find v.id interference_graph) with
  | Some _ -> if v.degree != 1 then Some({v with degree = v.degree - 1}) else None
  | None -> Some v) gi

(** Mutable operation *)
let push_on_stack 
  (stack : VertexStack.t) (vert : vertex) =
  VertexStack.push vert stack

let replace_register
  (instruction : riscomm)
  (register_replaced : reg)
  (register_replacing : reg) =
  match instruction with
  | Minirisc.Rtor (op, reg1, reg2, reg3) ->
      let reg1' = if reg1 = register_replaced then register_replacing else reg1 in
      let reg2' = if reg2 = register_replaced then register_replacing else reg2 in
      let reg3' = if reg3 = register_replaced then register_replacing else reg3 in
      Minirisc.Rtor (op, reg1', reg2', reg3')
  | Minirisc.Rtoi (op, reg1, imm, reg2) ->
      let reg1' = if reg1 = register_replaced then register_replacing else reg1 in
      let reg2' = if reg2 = register_replaced then register_replacing else reg2 in
      Minirisc.Rtoi (op, reg1', imm, reg2')
  | Minirisc.Rury (op, reg1, reg2) ->
      let reg1' = if reg1 = register_replaced then register_replacing else reg1 in
      let reg2' = if reg2 = register_replaced then register_replacing else reg2 in
      Minirisc.Rury (op, reg1', reg2')
  | Minirisc.Load (reg1, reg2) ->
      let reg1' = if reg1 = register_replaced then register_replacing else reg1 in
      let reg2' = if reg2 = register_replaced then register_replacing else reg2 in
      Minirisc.Load (reg1', reg2')
  | Minirisc.LoadI (imm, reg1) ->
      let reg1' = if reg1 = register_replaced then register_replacing else reg1 in
      Minirisc.LoadI (imm, reg1')
  | Minirisc.Store (reg1, reg2) ->
      let reg1' = if reg1 = register_replaced then register_replacing else reg1 in
      let reg2' = if reg2 = register_replaced then register_replacing else reg2 in
      Minirisc.Store (reg1', reg2')
  | Minirisc.Nop -> Minirisc.Nop

let spill_register_local
  (register: reg)
  (blk: node)
  (cfg : mriscfg)   
  (memory_location: int)
  : riscomm list =
  match (Nodemap.find_opt blk cfg.code) with
  | Some blk_code -> 
      let reversed_instructions = List.rev blk_code in
      let available_registers = Data_flow_analysis.Utils.get_top cfg in
      fst (List.fold_left (fun (new_list, available_registers) instruction -> 
        let read = Data_flow_analysis.Utils.extract_read_registers instruction in 
        let available_registers = RegSet.diff available_registers read in
        let (prefix_list, reading_register) = 
          if RegSet.mem register read then 
            match RegSet.choose_opt available_registers with
            | Some r -> 
              (new_list @ 
               [Minirisc.LoadI(memory_location, r); 
                Minirisc.Load(r, r); 
                replace_register instruction register r], Some r)
            | None -> failwith "There are not enough registers to read spilled register"
          else
            (new_list, None)
        in
        let written = Data_flow_analysis.Utils.extract_written_register instruction in
        let (complete_list, available_registers) =
          match written with
          | Some r when r = register -> 
              begin match reading_register with
              | Some rr -> 
                  (match RegSet.choose_opt (RegSet.remove rr available_registers) with
                  | Some address -> 
                      (prefix_list @ 
                       [Minirisc.LoadI(memory_location,address); Minirisc.Store(rr, address)], 
                       available_registers)
                  | None -> failwith "Not enough registers to write spilled register")
              | None -> 
                  let write_in_register = 
                    match RegSet.choose_opt available_registers with
                    | Some r -> r
                    | None -> failwith "Not enough registers for writing spilled register"
                  in
                  let address_register = 
                    match RegSet.choose_opt (RegSet.remove write_in_register available_registers) with
                    | Some r -> r
                    | None -> failwith "Not enough registers for writing address for storing"
                  in
                  (prefix_list @ 
                   [replace_register instruction register write_in_register; 
                    Minirisc.LoadI(memory_location, address_register);
                    Minirisc.Store(write_in_register, address_register)], 
                   available_registers)
              end
          | Some r -> (prefix_list, RegSet.add r available_registers)
          | None -> (prefix_list, available_registers)
        in
        (complete_list, available_registers)
      ) ([], available_registers) reversed_instructions)
  | None -> failwith "The Cfg passed as input does not contain the needed block of code"

let rec chaitin_briggs_step1 
  (interference_graph : RegSet.t RegMap.t) 
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
        let () = push_on_stack stack v in
        (chaitin_briggs_step1 interference_graph (remove_vertex interference_graph gi v.id) gh stack k current_coloring spilled_registers)
      else 
        (chaitin_briggs_step2 interference_graph gi gh stack k current_coloring spilled_registers)
    | None -> (chaitin_briggs_step3 interference_graph gi gh stack k current_coloring spilled_registers)
and chaitin_briggs_step2
  (interference_graph : RegSet.t RegMap.t) 
  (gi: VertexSetByDegree.t) 
  (gh: VertexSetBySpillMetric.t)
  (stack : VertexStack.t) 
  (k : int)
  (current_coloring : VertexSetByColor.t) 
  (spilled_registers : RegSet.t) :
  (VertexSetByColor.t * RegSet.t) = 
  let rec find_max_by_metric setmetric = 
    match VertexSetBySpillMetric.max_elt_opt setmetric with
    | Some x -> (match VertexSetByDegree.mem x gi with
                | true -> (x, setmetric)
                | false -> find_max_by_metric (VertexSetBySpillMetric.remove x setmetric))
    | None -> failwith "There are no nodes in the graphs, thus step2 should have not been invoked, step1 should have returned"
  in
  let (best, gh) = find_max_by_metric gh
  in
  let () = VertexStack.push best stack 
  in
  chaitin_briggs_step1 interference_graph (remove_vertex interference_graph gi best.id) gh stack k current_coloring spilled_registers
and chaitin_briggs_step3 
  (interference_graph : RegSet.t RegMap.t) 
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
                | true -> if (x.color > acc + 1) then (acc + 1, true) else (x.color, false)
                | false -> (acc, b))
      | true -> (acc, b)) current_coloring (1, false)) in
    if color < k then
      chaitin_briggs_step3 interference_graph gi gh stack k (VertexSetByColor.add {v with color = color + 1 } (VertexSetByColor.remove v current_coloring)) spilled_registers
    else
      chaitin_briggs_step1 interference_graph gi gh stack k (VertexSetByColor.remove v current_coloring) (RegSet.add v.id spilled_registers))
      
let apply_spilling_to_cfg 
  (cfg : mriscfg)
  (spilled_registers : RegSet.t) =
  snd (RegSet.fold (fun register (memory, blocks) -> (memory + 1, Nodemap.fold (fun node _ map -> 
    (Nodemap.add node (spill_register_local register node cfg memory) map)) blocks Nodemap.empty)) spilled_registers (0, cfg.code))

let apply_color_map_to_cfg
  (cfg : mriscfg)
  (color_map : int RegMap.t) : mriscfg = 
  let replace_register_with_map reg =
    match RegMap.find_opt reg color_map with
    | Some new_reg -> new_reg
    | None -> reg  (* If no mapping is found, return the original register *)
  in      
  let replace_instruction instruction =
    match instruction with
    | Minirisc.Rtor (op, reg1, reg2, reg3) ->
        Minirisc.Rtor (op, replace_register_with_map reg1, replace_register_with_map reg2, replace_register_with_map reg3)
    | Minirisc.Rtoi (op, reg1, imm, reg2) ->
        Minirisc.Rtoi (op, replace_register_with_map reg1, imm, replace_register_with_map reg2)
    | Minirisc.Rury (op, reg1, reg2) ->
        Minirisc.Rury (op, replace_register_with_map reg1, replace_register_with_map reg2)
    | Minirisc.Load (reg1, reg2) ->
        Minirisc.Load (replace_register_with_map reg1, replace_register_with_map reg2)
    | Minirisc.LoadI (imm, reg1) ->
        Minirisc.LoadI (imm, replace_register_with_map reg1)
    | Minirisc.Store (reg1, reg2) ->
        Minirisc.Store (replace_register_with_map reg1, replace_register_with_map reg2)
    | Minirisc.Nop -> Minirisc.Nop
  in
  let replace_block blk_code =
    List.map replace_instruction blk_code
  in
  let new_code = Nodemap.fold (fun blk blk_code acc ->
    Nodemap.add blk (replace_block blk_code) acc
  ) cfg.code Nodemap.empty in
  { cfg with code = new_code }

(** Returns a mapping for each register to a new register(the one it is substituted with).
Spilled registers are given as the fourth element in the result. *)
let chaitin_briggs_algorithm (cfg : mriscfg) (k : int) = 
    let g = compute_live_ranges cfg in
    let (vertexesByDegree, vertexesByMetric, vertexesByColor) = RegMap.fold (fun reg neighbors (setByDegree, setByMetric, setByColor) -> 
      let vertex = {id = reg; degree = (RegSet.cardinal neighbors); cost = (compute_cost_register cfg reg); color = 10000} in
      (VertexSetByDegree.add vertex setByDegree, VertexSetBySpillMetric.add vertex setByMetric, VertexSetByColor.add vertex setByColor))
    g (VertexSetByDegree.empty, VertexSetBySpillMetric.empty, VertexSetByColor.empty) in
    let (colors, spilled) = chaitin_briggs_step1 g vertexesByDegree vertexesByMetric (VertexStack.create ()) k vertexesByColor RegSet.empty in
    let color_map = VertexSetByColor.fold (fun v map -> RegMap.add v.id v.color map) colors RegMap.empty in
    let merged_cfg = apply_color_map_to_cfg cfg color_map in
    {merged_cfg with code = apply_spilling_to_cfg merged_cfg spilled}


  (* Function to extract and sort by live range size *)
  let sort_by_live_range_size (mapEdges : RegSet.t RegMap.t) =
    RegMap.bindings mapEdges  (* Extract key-value pairs *)
    |> List.sort (fun (_, e1) (_, e2) -> compare (RegSet.cardinal e1) (RegSet.cardinal e2))

  (** let cost_degree_metric (graph : interference_graph) heuristic = *)
