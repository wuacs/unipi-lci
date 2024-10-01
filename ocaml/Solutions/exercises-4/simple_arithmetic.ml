  type a_exp = 
    Aval of int 
    | Plus of a_exp * a_exp 
    | Minus of a_exp * a_exp
    | Times of a_exp * a_exp
    | Of_bool of b_exp
  and b_exp = Bval of bool
      | And of b_exp * b_exp
      | Or of b_exp * b_exp
      | Not of b_exp
      | Minor of a_exp * a_exp

exception NotValidExpr

let rec eval_arithmetic_without_coercion = function
  | Plus(x, y) -> eval_arithmetic_without_coercion(x) + eval_arithmetic_without_coercion(y)
  | Minus(x, y) -> eval_arithmetic_without_coercion(x) - eval_arithmetic_without_coercion(y)
  | Times(x, y) -> eval_arithmetic_without_coercion(x) * eval_arithmetic_without_coercion(y)
  | Aval(x) -> x
  | Of_bool(x) -> raise NotValidExpr

let rec eval_bool = function
  | And(x, y) -> eval_bool(x) && eval_bool(y)
  | Or(x, y) -> eval_bool(x) || eval_bool(y)
  | Not(x) -> eval_bool(x)
  | Minor(x, y) -> eval_arithmetic_without_coercion(x) < eval_arithmetic_without_coercion(y)
  | Bval(x) -> x
  
let rec eval_arithmetic_c = function
  | Plus(x, y) -> eval_arithmetic_c(x) + eval_arithmetic_c(y)
  | Minus(x, y) -> eval_arithmetic_c(x) - eval_arithmetic_c(y)
  | Times(x, y) -> eval_arithmetic_c(x) * eval_arithmetic_c(y)
  | Aval(x) -> x
  | Of_bool(x) -> (match eval_bool(x) with 
                    | false -> 0
                    | true -> 1)