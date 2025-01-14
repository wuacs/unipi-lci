type register = int

module RegisterSet = Set.Make (struct
  type t = register

  let compare x y = compare x y
end)

module RegisterMap = Map.Make (struct
  type t = register

  let compare x y = compare x y
end)

type label = String
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
