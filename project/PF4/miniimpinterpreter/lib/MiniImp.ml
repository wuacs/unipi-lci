type a_exp = 
  Aval of int 
  | Plus of a_exp * a_exp 
  | Minus of a_exp * a_exp
  | Times of a_exp * a_exp
  | Substitue of string

type b_exp = Bval of bool
  | And of b_exp * b_exp
  | Or of b_exp * b_exp
  | Not of b_exp
  | Minor of a_exp * a_exp

type command = Skip 
| Assign of (string * a_exp)
| Sequence of (command * command)
| Cond of (b_exp * command * command)
| While of (b_exp * command)

type program = {input: string; output: string; command: command} 