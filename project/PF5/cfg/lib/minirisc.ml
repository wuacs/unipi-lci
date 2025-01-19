type register = int

module RegisterSet = Set.Make (struct
  type t = register

  let compare x y = compare x y
end)

module RegisterMap = Map.Make (struct
  type t = register

  let compare x y = compare x y
end)

type label = string
type mem_ram = int -> int
type mem_reg = register -> int
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

let minirisc_command_to_string (stmt : comm) : string =
  let register_to_string (reg : register) = 
    "R" ^ (string_of_int reg) 
  in
  let brop_code_to_string (stmt : brop) : string = 
    match stmt with 
    | Add -> "Add"
    | Mult -> "Mult"
    | Less -> "Less"
    | And -> "And"
    | Sub -> "Sub"
  in
  let biop_code_to_string (stmt : biop) : string = 
    match stmt with
    | AddI -> "AddI"
    | MultI -> "MultI"
    | AndI -> "AndI"
    | SubI -> "SubI"
  in
  let unaryop_code_to_string (stmt : urop) : string = 
    match stmt with
    | Copy -> "Copy"
    | Not -> "Not"
  in
  let minirisc_simple_to_string (stmt : scomm) : string =
    match stmt with
    | Rtor(opcode, r1, r2, r3) -> brop_code_to_string opcode ^ (register_to_string r1) ^ " => " ^ (register_to_string r2) ^ " => " ^ (register_to_string r3)
    | Rtoi(opcode, r1, n, r3) -> biop_code_to_string opcode ^ (register_to_string r1) ^ " => " ^ (string_of_int n) ^ " => " ^ (register_to_string r3)
    | Rury(opcode, r1, r2) -> unaryop_code_to_string opcode ^ (register_to_string r1) ^ " => " ^ (register_to_string r2)
    | Load(r1, r2) -> "Load " ^ (register_to_string r1) ^ " => " ^ (register_to_string r2)
    | LoadI(n, r) -> "LoadI " ^ (string_of_int n) ^ " => " ^ (register_to_string r)
    | Store(r1, r2) -> "Store " ^ (register_to_string r1) ^ " => " ^ (register_to_string r2)
    | Nop -> "Nop"
  in
    match stmt with
    | Simple(scomm) -> minirisc_simple_to_string scomm
    | Jump(label) -> Printf.sprintf "jump l%s" label
    | Cjump(reg, l1, l2) -> Printf.sprintf "cjump r%d l%s l%s" reg l1 l2