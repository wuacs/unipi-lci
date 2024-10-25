(** Type representing a variable, i.e. the domain of the {!value_env} type *)
type var = string

(** Type representing one of the allowed {b binary} operators. In future it might be wise to rename this. *)
type op = Plus | Minus | And | Mul | Less | Equal

(** 
Type representing the abstract syntax tree of the language. Notice how closure definitions {b Fun} and
{b LetFun} have typed(see {!tau}) parameters.

Notice that a node of the AST, which has type {!ast} {i can} be a variable by variant `Var` but
a variable is not a {!value} thus the height of the AST rooted at a node of type `Var` is 1, i.e. 
variables are not first-class values.
*)
type ast = 
  | LetFun of var * var * tau * ast * ast
  | Fun of var * tau * ast
  | Let of var * ast * ast
  | Op of ast * op * ast
  | If of ast * ast * ast
  | App of ast * ast
  | Val of value
  | Var of var

(** Type representing the type of a term. {b Not sure about naming conventions?} *)
and tau = 
  | Not_Defined
  | Integer_t
  | Boolean_t
  | Closure_t of tau * tau 

(** Storable first class values. Integers, Booleans, Recursive closures, Non-Recursive closures *)
and value =
  | Integer of int
  | Boolean of bool
  | RecClosure of var * var * ast * value_env
  | Closure of var * ast * value_env
  
(** The value environment which can be seen as a store of mappings  {!var} -> {!value} *)
and value_env

(** The type environment which can be seen as a mapping from {!var} -> {!tau} *)
and type_env

(** 
This function checks if the given AST is type safe:
- returns {i Some(t)} if t is the type of the AST
- returns {i None} if the the AST contains a type error 
*)
val type_check: ast -> tau option