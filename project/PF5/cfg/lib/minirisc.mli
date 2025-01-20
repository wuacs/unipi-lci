type register = Id of int [@@unboxed]
type label = Label of int [@@unboxed]

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

module RegisterSet : Set.S with type elt = register
module RegisterMap : Map.S with type key = register
module LabelMap : Map.S with type key = label

val get_reg_id : register -> int 

val get_label_val : label -> int

val minirisc_command_to_string : comm -> string