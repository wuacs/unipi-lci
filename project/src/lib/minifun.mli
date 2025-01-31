module EnvMap : Map.S with type key = string

type var = string
type operator = Plus | Minus | And | Mul | Less | Equal

type ast =
  | Fun of var * ast
  | App of ast * ast
  | Op of ast * operator * ast
  | If of ast * ast * ast
  | Let of var * ast * ast
  | LetFun of var * var * ast * ast
  | Value of value
  | Var of var

and value =
  | Integer of int
  | Boolean of bool
  | RecClosure of var * var * ast * env
  | Closure of var * ast * env

and env = value EnvMap.t

val string_of_value : value -> string
(** Converts a {!Minifun.value} into his string counterpart *)

val eval_fun_ast : ast -> value
(** Evaluates a minifun program, in the form of an {!Minifun.ast}*)
