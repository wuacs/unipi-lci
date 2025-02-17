type register = Id of int [@@unboxed]
type label = Label of int [@@unboxed]
type memory_address = Address of int [@@unboxed]
type memory_loc = Register of register | Memory of memory_address

let in_register = Id 0
let out_register = Id 1
let first_free_register = Id 2
let main_label = Label 0
let exit_label = Label (-1)
let get_reg_id r = match r with Id l -> l
let get_label_val l = match l with Label v -> v
let get_memory_address m = match m with Address v -> v

module RegisterSet = Set.Make (struct
  type t = register

  let compare x y = compare x y
end)

module RegisterMap = Map.Make (struct
  type t = register

  let compare x y = compare x y
end)

module LabelMap = Map.Make (struct
  type t = label

  let compare x y =
    if x = exit_label && y = exit_label then 0 else
    if x = exit_label then 1 else
    if y = exit_label then -1 else
    compare (get_label_val x) (get_label_val y)
end)

module MemoryMap = Map.Make (struct
  type t = memory_address

  let compare x y = compare (get_memory_address x) (get_memory_address y)
end)

type brop = Add | Sub | Mult | And | Less
type biop = AddI | SubI | MultI | AndI
type urop = Not | Copy

type scomm =
  | Nop
  | Rtor of brop * register * register * register (* register to register *)
  | Rtoi of biop * register * int * register (* Register to integer *)
  | Rury of urop * register * register (* register unary *)
  | Load of register * register
  | LoadI of int * register
  | Store of register * register

type comm =
  | Simple of scomm
  | Jump of label
  | Cjump of register * label * label

type block = comm List.t
type program = Program of (block Array.t * int LabelMap.t) [@@unboxed]
  
let minirisc_command_to_string (stmt : comm) : string =
  let right_hand_side_separator = " => " in
  let operand_separator = " " in
  let register_to_string (reg : register) = string_of_int (get_reg_id reg) in
  let brop_code_to_string (stmt : brop) : string =
    match stmt with
    | Add -> "add"
    | Mult -> "mult"
    | Less -> "less"
    | And -> "and"
    | Sub -> "sub"
  in
  let biop_code_to_string (stmt : biop) : string =
    match stmt with
    | AddI -> "addI"
    | MultI -> "multI"
    | AndI -> "andI"
    | SubI -> "subI"
  in
  let unaryop_code_to_string (stmt : urop) : string =
    match stmt with Copy -> "copy" | Not -> "not"
  in
  let minirisc_simple_to_string (stmt : scomm) : string =
    match stmt with
    | Rtor (opcode, r1, r2, r3) ->
        brop_code_to_string opcode ^ operand_separator ^ register_to_string r1
        ^ operand_separator ^ register_to_string r2 ^ right_hand_side_separator
        ^ register_to_string r3
    | Rtoi (opcode, r1, n, r2) ->
        biop_code_to_string opcode ^ operand_separator ^ register_to_string r1
        ^ operand_separator ^ string_of_int n ^ right_hand_side_separator
        ^ register_to_string r2
    | Rury (opcode, r1, r2) ->
        unaryop_code_to_string opcode
        ^ operand_separator ^ register_to_string r1 ^ right_hand_side_separator
        ^ register_to_string r2
    | Load (r1, r2) ->
        "load" ^ operand_separator ^ register_to_string r1
        ^ right_hand_side_separator ^ register_to_string r2
    | LoadI (n, r) ->
        "loadI " ^ operand_separator ^ string_of_int n
        ^ right_hand_side_separator ^ register_to_string r
    | Store (r1, r2) ->
        "store" ^ operand_separator ^ register_to_string r1
        ^ right_hand_side_separator ^ register_to_string r2
    | Nop -> "nop"
  in
  match stmt with
  | Simple scomm -> minirisc_simple_to_string scomm
  | Jump label -> Printf.sprintf "jump l%d" (get_label_val label)
  | Cjump (reg, l1, l2) ->
      Printf.sprintf "cjump %d l%d l%d" (get_reg_id reg) (get_label_val l1)
        (get_label_val l2)

let get_program prg = 
  match prg with
  | Program p -> p