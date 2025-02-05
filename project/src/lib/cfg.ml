open Minirisc

type node = Label of int [@@unboxed]

module NodeSet = Set.Make (struct
  type t = node

  let compare x y = compare x y
end)

module NodeMap = Map.Make (struct
  type t = node

  let compare x y = compare x y
end)

module ImpAst = Miniimp.ImpAst
module StringMap = Map.Make (String)

type next_codomain = Uncond of node | Cond of (node * node) | None

type 'a control_flow_graph = {
  nodes : NodeSet.t;
  edges : next_codomain NodeMap.t;
  entry : node;
  exit : node;
  code : 'a list NodeMap.t;
}

type coercable = Boolean of ImpAst.b_exp | Integer of ImpAst.a_exp

let empty_node_set = NodeSet.empty
let empty_edge_map = NodeMap.empty
let empty_code_map = NodeMap.empty
let dummy_node_val = Label (-1)
let third (_, _, t) = t
let first (f, _, _) = f
let second (_, s, _) = s

let compute_reversed_map (cfg : 'a control_flow_graph) : node list NodeMap.t =
  NodeMap.fold
    (fun src_node type_edge reversed_map ->
      match type_edge with
      | Uncond n ->
          let updated_list =
            src_node
            :: (NodeMap.find_opt n reversed_map |> Option.value ~default:[])
          in
          NodeMap.add n updated_list reversed_map
      | Cond (n1, n2) ->
          let updated_list1 =
            src_node
            :: (NodeMap.find_opt n1 reversed_map |> Option.value ~default:[])
          in
          let updated_list2 =
            src_node
            :: (NodeMap.find_opt n2 reversed_map |> Option.value ~default:[])
          in
          NodeMap.add n1 updated_list1
            (NodeMap.add n2 updated_list2 reversed_map)
      | None -> reversed_map)
    cfg.edges NodeMap.empty

(** Pushes an instruction to the front of a particular node.*)
let push_instruction node command blk_map =
  NodeMap.update node
    (fun x ->
      match x with Some y -> Some (command :: y) | None -> Some [ command ])
    blk_map

(** Labels of nodes are assigned incrementally *)
let get_next_uid_node : node -> node =
 fun x -> match x with Label v -> Label (v + 1)

(** Registers are assigned incrementally *)
let get_new_register (register : register) : register =
  Id (get_reg_id register + 1)

let add_edge_mono (source : node) (sink : node)
    (edges : next_codomain NodeMap.t) : next_codomain NodeMap.t =
  NodeMap.add source (Uncond sink) edges

let add_edge_bi (source : node) (sink_true : node) (sink_false : node)
    (edges : next_codomain NodeMap.t) : next_codomain NodeMap.t =
  NodeMap.add source (Cond (sink_true, sink_false)) edges

let add_node_set (node : node) (set : NodeSet.t) : NodeSet.t =
  NodeSet.add node set

let rec add_list_node_to_set (node : node list) (set : NodeSet.t) : NodeSet.t =
  match node with
  | x :: xs -> add_list_node_to_set xs (add_node_set x set)
  | [] -> set

let rec add_list_edges_mono (ss : (node * node) list)
    (edges : next_codomain NodeMap.t) : next_codomain NodeMap.t =
  match ss with
  | (src, sink) :: oth -> add_list_edges_mono oth (add_edge_mono src sink edges)
  | _ -> edges

let create_cfg nodes edges entry exit code = { nodes; edges; entry; exit; code }

let reverse_code_set (cfg : 'a control_flow_graph) : 'a control_flow_graph =
  let code = cfg.code in
  let newcode =
    NodeMap.fold
      (fun nodeId list newMap -> NodeMap.add nodeId (List.rev list) newMap)
      code NodeMap.empty
  in
  { cfg with code = newcode }

let translate_miniimp (program : ImpAst.program) :
    ImpAst.miniimp_simple control_flow_graph =
  let rec translate_command (cfg : ImpAst.miniimp_simple control_flow_graph)
      (command : ImpAst.command) : ImpAst.miniimp_simple control_flow_graph =
    match command with
    | Assign (x, y) ->
        create_cfg (add_node_set cfg.entry cfg.nodes) cfg.edges cfg.entry cfg.entry
          (push_instruction cfg.entry (ImpAst.Assignment (x, y)) cfg.code)
    | Sequence (c1, c2) ->
        let cfg_c1 =
          translate_command
            (create_cfg cfg.nodes cfg.edges cfg.entry dummy_node_val cfg.code)
            c1
        in
        let cfg_c2 =
          translate_command
            (create_cfg cfg_c1.nodes cfg_c1.edges cfg_c1.exit dummy_node_val
               cfg_c1.code)
            c2
          (* Note the start block of the next CFG is the same as the end block of the previous *)
        in
        create_cfg
          (add_list_node_to_set
             [ cfg_c2.exit; cfg_c1.entry; cfg_c1.exit ]
             cfg_c2.nodes)
          cfg_c2.edges cfg.entry cfg_c2.exit cfg_c2.code
    | Cond (bexpr, c1, c2) ->
        let guard_blk =
          push_instruction
            (* The guard represents creating branches so we cannot put any more things here *)
            cfg.entry (ImpAst.Guard bexpr) cfg.code
        in
        let cfg_c1 =
          translate_command
            (create_cfg cfg.nodes cfg.edges
               (get_next_uid_node cfg.entry) (* Get next uid for start *)
               dummy_node_val guard_blk) (* The code map is the last one defined *)
            c1
        in
        let cfg_c2 =
          translate_command
            (create_cfg cfg_c1.nodes cfg_c1.edges
               (get_next_uid_node cfg_c1.exit) (* This is a separate CFG *)
               dummy_node_val cfg_c1.code)
            c2
        in
        let ifs_exit = get_next_uid_node cfg_c2.exit in
        let after_skipping =
          push_instruction ifs_exit ImpAst.Skip cfg_c2.code
        in
        create_cfg
          (add_list_node_to_set
             [ ifs_exit; cfg_c1.entry; cfg_c1.exit; cfg_c2.entry; cfg_c2.exit; cfg.entry ]
             cfg_c2.nodes)
          (add_edge_bi cfg.entry cfg_c1.entry cfg_c2.entry
             (add_list_edges_mono
                [ (cfg_c1.exit, ifs_exit); (cfg_c2.exit, ifs_exit) ]
                cfg_c2.edges))
          cfg.entry ifs_exit after_skipping
    | While (bexpr, c1) ->
        (* Since guard has an entry point we cannot use our starting block *)
        let guard_block = get_next_uid_node cfg.entry in
        let with_grd =
          push_instruction guard_block (ImpAst.Guard bexpr) cfg.code
        in
        let cfg_c1 =
          translate_command
            (create_cfg cfg.nodes cfg.edges
               (get_next_uid_node guard_block)
               dummy_node_val with_grd)
            c1
        in
        (* Since while's body final block should jump back to beginning we need an additional block for
           ending *)
        let whiles_exit = get_next_uid_node cfg_c1.exit in
        let after_adding_skips =
          push_instruction whiles_exit ImpAst.Skip cfg_c1.code
        in
        create_cfg
          (add_list_node_to_set
             [ whiles_exit; cfg_c1.exit; cfg_c1.entry; guard_block; cfg.entry ]
             cfg_c1.nodes)
          (add_edge_bi guard_block cfg_c1.entry whiles_exit
             (add_list_edges_mono
                [ (cfg_c1.exit, guard_block); (cfg.entry, guard_block) ]
                cfg_c1.edges))
          cfg.entry whiles_exit after_adding_skips
    | Skip ->
        create_cfg cfg.nodes cfg.edges cfg.entry cfg.entry
          (push_instruction cfg.entry ImpAst.Skip cfg.code)
  in
  let cfg =
    reverse_code_set
      (translate_command
         (create_cfg empty_node_set empty_edge_map (Label 0) dummy_node_val
            empty_code_map)
         program.command)
  in
  { cfg with edges = NodeMap.add cfg.exit None cfg.edges }

let node_to_string (node : node) : string =
  match node with Label n -> string_of_int n

let access_node n = match n with Label l -> l

let next_codomain_to_string (src : node) (edge : next_codomain) =
  match edge with
  | Uncond sink ->
      Printf.sprintf "%s -> %s;\n" (node_to_string src) (node_to_string sink)
  | Cond (sink_true, sink_false) ->
      Printf.sprintf "%s -> %s [label=\"true\"];\n%s -> %s [label=\"false\"];"
        (node_to_string src) (node_to_string sink_true) (node_to_string src)
        (node_to_string sink_false)
  | None -> ""

(** This function takes a node of a CFG and it maps each instruction to a new
    value and accumulates using the fold_fun function parameter. Each code's
    block is traversed from beginning to end.

    fold_fun's first parameter is the accumulated value and the new instruction
    parsed is the second.*)
let fold_left_code_blocks (node : node) (map : 'a list NodeMap.t)
    (map_statement : 'a -> 'b) (on_empty_block : 'b) (base_value : 'b)
    (fold_fun : 'b -> 'b -> 'b) : 'b =
  let x = NodeMap.find_opt node map in
  match x with
  | Some l ->
      List.fold_left (fun acc x -> fold_fun acc (map_statement x)) base_value l
  | None -> on_empty_block

let miniimp_cfg_to_dot (cfg : ImpAst.miniimp_simple control_flow_graph) : string
    =
  let miniimp_simple_to_string (stmt : ImpAst.miniimp_simple) : string =
    match stmt with
    | Skip -> "SKIP"
    | Assignment (x, _) -> "Assignment to " ^ x
    | Guard _ -> "GUARD"
  in
  let nodes_str =
    NodeSet.fold
      (fun node acc ->
        acc
        ^ Printf.sprintf "  %s [label=\"%s\"];\n" (node_to_string node)
            (fold_left_code_blocks node cfg.code miniimp_simple_to_string "SKIP"
               "" (fun ni acc -> ni ^ " \\n " ^ acc)))
      cfg.nodes ""
  in

  let edges_str =
    NodeMap.fold
      (fun src next_codomain acc ->
        acc ^ next_codomain_to_string src next_codomain)
      cfg.edges ""
  in

  let entry_str = node_to_string cfg.entry in
  let exit_str = node_to_string cfg.exit in

  (* Return the full graph's DOT string *)
  Printf.sprintf
    "digraph G {\n\
    \  // Entry node\n\
    \  %s [shape=ellipse, color=green];\n\
    \  // Exit node\n\
    \  %s [shape=ellipse, color=red];\n\
     %s%s}\n"
    entry_str exit_str nodes_str edges_str

let minirisc_cfg_to_dot (cfg : Minirisc.scomm control_flow_graph) : string =
  let nodes_str =
    NodeSet.fold
      (fun node acc ->
        acc
        ^ Printf.sprintf "  %s [label=\"%s\"];\n" (node_to_string node)
            (fold_left_code_blocks node cfg.code
               (fun x -> Minirisc.minirisc_command_to_string (Simple x))
               "Nop" ""
               (fun ni acc -> ni ^ " \\n " ^ acc)))
      cfg.nodes ""
  in

  let edges_str =
    NodeMap.fold
      (fun src next_codomain acc ->
        acc ^ next_codomain_to_string src next_codomain)
      cfg.edges ""
  in

  let entry_str = node_to_string cfg.entry in
  let exit_str = node_to_string cfg.exit in

  (* Return the full graph's DOT string *)
  Printf.sprintf
    "digraph G {\n\
    \  // Entry node\n\
    \  %s [shape=ellipse, color=green];\n\
    \  // Exit node\n\
    \  %s [shape=ellipse, color=red];\n\
     %s%s}\n"
    entry_str exit_str nodes_str edges_str

let coerce (x : coercable) : int option =
  match x with
  | Boolean b -> (
      match b with Bval false -> Some 0 | Bval true -> Some 1 | _ -> None)
  | Integer aritm -> ( match aritm with Aval i -> Some i | _ -> None)

let coerce_or_fail x =
  match coerce x with Some v -> v | None -> failwith "Coercion failed"

let reserve_opt_variable_register (map : register StringMap.t) (var : string)
    (reserved_register : register) : register StringMap.t =
  match StringMap.find_opt var map with
  | Some _ -> map
  | None -> StringMap.add var reserved_register map

let convert_minirisc_bop (register : Minirisc.register) (input : 'a * 'a)
    (vars : register StringMap.t)
    (helper_fun :
      Minirisc.register ->
      'a ->
      register StringMap.t ->
      Minirisc.scomm list * Minirisc.register * register StringMap.t)
    (ir : int -> Minirisc.register -> Minirisc.register -> Minirisc.scomm list)
    (ri : Minirisc.register -> int -> Minirisc.register -> Minirisc.scomm list)
    (rr :
      Minirisc.register ->
      Minirisc.register ->
      Minirisc.register ->
      Minirisc.scomm list) :
    Minirisc.scomm list * Minirisc.register * register StringMap.t =
  match input with
  | x, y when Option.is_some (coerce x) ->
      let t = coerce_or_fail x in
      let right_register = get_new_register register in
      let eval_right = helper_fun right_register y vars in
      ( first eval_right @ ir t right_register register,
        second eval_right,
        third eval_right )
  | x, y when Option.is_some (coerce y) ->
      let t = coerce_or_fail y in
      let left_register = get_new_register register in
      let eval_left = helper_fun left_register x vars in
      ( first eval_left @ ri left_register t register,
        second eval_left,
        third eval_left )
  | x, y ->
      let left_register = get_new_register register in
      let eval_left = helper_fun left_register x vars in
      let right_register = get_new_register (second eval_left) in
      let eval_right = helper_fun right_register y (third eval_left) in
      ( first eval_left @ first eval_right
        @ rr left_register right_register register,
        second eval_right,
        third eval_right )

let convert_miniimp_arithmetic_to_minirisc (available : Minirisc.register)
    (expr : ImpAst.a_exp) (vars : register StringMap.t) :
    Minirisc.scomm list * Minirisc.register * register StringMap.t =
  let rec helper_arithemtic (register : Minirisc.register) (expr : coercable)
      (vars : register StringMap.t) :
      Minirisc.scomm list * Minirisc.register * register StringMap.t =
    match expr with
    | Integer expr -> (
        match expr with
        | ImpAst.Minus (x, y) ->
            convert_minirisc_bop register (Integer x, Integer y) vars
              helper_arithemtic
              (fun x y z -> [ Minirisc.Rtoi (Minirisc.SubI, y, x, z) ])
              (fun x y z -> [ Minirisc.Rtoi (Minirisc.SubI, x, y, z) ])
              (fun x y z -> [ Minirisc.Rtor (Minirisc.Sub, x, y, z) ])
        | ImpAst.Times (x, y) ->
            convert_minirisc_bop register (Integer x, Integer y) vars
              helper_arithemtic
              (fun x y z -> [ Minirisc.Rtoi (Minirisc.MultI, y, x, z) ])
              (fun x y z -> [ Minirisc.Rtoi (Minirisc.MultI, x, y, z) ])
              (fun x y z -> [ Minirisc.Rtor (Minirisc.Mult, x, y, z) ])
        | ImpAst.Plus (x, y) ->
            convert_minirisc_bop register (Integer x, Integer y) vars
              helper_arithemtic
              (fun x y z -> [ Minirisc.Rtoi (Minirisc.AddI, y, x, z) ])
              (fun x y z -> [ Minirisc.Rtoi (Minirisc.AddI, x, y, z) ])
              (fun x y z -> [ Minirisc.Rtor (Minirisc.Add, x, y, z) ])
        | ImpAst.Substitue s ->
            let vars = reserve_opt_variable_register vars s register in
            ( [
                Minirisc.Rury (Minirisc.Copy, StringMap.find s vars, register);
              ],
              register,
              vars )
        | ImpAst.Aval n -> ([ Minirisc.LoadI (n, register) ], register, vars))
    | _ -> failwith "Called arithmetic helper function passing a boolean"
  in
  helper_arithemtic available (Integer expr) vars

let convert_miniimp_boolean_to_minirisc (available : Minirisc.register)
    (expr : ImpAst.b_exp) (vars : register StringMap.t) :
    Minirisc.scomm list * Minirisc.register * register StringMap.t =
  let rec helper_boolean (register : Minirisc.register) (expr : coercable)
      (vars : register StringMap.t) :
      Minirisc.scomm list * Minirisc.register * register StringMap.t =
    match expr with
    | Boolean expr -> (
        match expr with
        | ImpAst.And (x, y) ->
            convert_minirisc_bop register (Boolean x, Boolean y) vars
              helper_boolean
              (fun x y z -> [ Minirisc.Rtoi (Minirisc.AndI, y, x, z) ])
              (fun x y z -> [ Minirisc.Rtoi (Minirisc.AndI, x, y, z) ])
              (fun x y z -> [ Minirisc.Rtor (Minirisc.And, x, y, z) ])
        | ImpAst.Minor (x, y) ->
            let left_register = get_new_register register in
            let left_expr =
              convert_miniimp_arithmetic_to_minirisc left_register x vars
            in
            let right_register = get_new_register (second left_expr) in
            let right_expr =
              convert_miniimp_arithmetic_to_minirisc right_register y
                (third left_expr)
            in
            ( first left_expr @ first right_expr
              @ [
                  Minirisc.Rtor
                    (Minirisc.Less, left_register, right_register, register);
                ],
              second right_expr,
              third right_expr )
        | ImpAst.Not x ->
            let eval_reg = get_new_register available in
            let eval_expr = helper_boolean eval_reg (Boolean x) vars in
            ( first eval_expr
              @ [ Minirisc.Rury (Minirisc.Not, eval_reg, available) ],
              second eval_expr,
              third eval_expr )
        | _ ->
            failwith
              "Only Add, Less and Not of miniimp's boolean instructions \
               supported")
    | _ -> failwith "Called boolean helper function passing a boolean"
  in
  helper_boolean available (Boolean expr) vars

let miniimp_cfg_to_minirisc 
  (imp_cfg : ImpAst.miniimp_simple control_flow_graph)
  ~input_variable:input_var
  ~output_variable:output_var
    : Minirisc.scomm control_flow_graph =
  let map_simple_imp_to_simple_risc (stmt : ImpAst.miniimp_simple)
      (available : Minirisc.register) (vars : register StringMap.t) :
      Minirisc.scomm list * Minirisc.register * register StringMap.t =
    match stmt with
    | ImpAst.Skip -> ([ Minirisc.Nop ], available, vars)
    | ImpAst.Assignment (str, aexp) ->
        let string_to_ram =
          reserve_opt_variable_register vars str available
        in
        let register = StringMap.find str string_to_ram in
        let aexp_register = get_new_register available in
        let eval_expr =
          convert_miniimp_arithmetic_to_minirisc aexp_register aexp
            string_to_ram
        in
        ( first eval_expr
          @ [ Minirisc.Rury (Minirisc.Copy, aexp_register, register) ],
          second eval_expr,
          third eval_expr )
    | ImpAst.Guard bexp ->
        convert_miniimp_boolean_to_minirisc available bexp vars
  in
  let code_translator (available : Minirisc.register)
      (ram : register StringMap.t) : Minirisc.scomm list NodeMap.t =
    first
      (NodeMap.fold
         (fun nodeId stmts (map, available, ram) ->
           let res =
             List.fold_left
               (fun (accumulated_stmts, current_register, ram) stmt ->
                 let blk_res =
                   map_simple_imp_to_simple_risc stmt current_register ram
                 in
                 ( accumulated_stmts @ first blk_res,
                   get_new_register (second blk_res),
                   third blk_res ))
               ([], available, ram) stmts
           in
           ( NodeMap.add nodeId (first res) map,
             get_new_register (second res),
             third res ))
         imp_cfg.code
         (NodeMap.empty, available, ram))
  in
  create_cfg imp_cfg.nodes imp_cfg.edges imp_cfg.entry imp_cfg.exit
    (code_translator first_free_register
       (StringMap.add output_var out_register (StringMap.singleton input_var in_register)))
