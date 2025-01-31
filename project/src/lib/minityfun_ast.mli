module Env_map : Map.S with type key = string
(** The environment map, mapping {!var} to {!value} *)

type var = string
(** Type representing a variable, i.e. the domain of the {!value_env} type *)

(** Type representing one of the allowed {b binary} operators. In future it
    might be wise to rename this. *)
type op = Plus | Minus | And | Mul | Less

(** Type representing the abstract syntax tree of the language. Notice how
    closure definitions {b Fun} and {b LetFun} have typed(see {!tau})
    parameters.

    Notice that a node of the AST, which has type {!ast} {i can} be a variable
    by variant `Var` but a variable is not a {!value} thus the height of the AST
    rooted at a node of type `Var` is 1, i.e. variables are not first-class
    values. *)
type ast =
  | LetFun of var * var * tau * ast * ast
  | Fun of var * tau * ast
  | Let of var * ast * ast
  | Op of ast * op * ast
  | If of ast * ast * ast
  | App of ast * ast
  | Val of value
  | Var of var

(** Type representing the type of a term. {b Not sure about naming conventions?}
*)
and tau =
  | Integer_t
  | Boolean_t
  | Closure_t of tau * tau
      (** Storable first class values. Integers, Booleans, Recursive closures,
          Non-Recursive closures *)

and value =
  | Integer of int
  | Boolean of bool
  | RecClosure of var * var * ast * value_env
  | Closure of var * ast * value_env

and value_env = value Env_map.t
(** The value environment which can be seen as a store of mappings {!var} ->
    {!value} *)

and type_env = tau Env_map.t
(** The type environment which can be seen as a mapping from {!var} -> {!tau} *)

val compatible : tau -> tau -> bool
(** That is this function represents the "compatible" function:
    + an Integer is compatible only with another Integer
    + a Boolean is compatible only with another Boolean
    + a Closure type is compatible with another Closure only if they domains are
      compatible and their codomains
    + a Hole type is compatible with any other type *)

val get_string_of_value : value -> string
val get_string_of_type : tau -> string
