module Env_map = Map.Make (struct
  type t = string

  let compare x y = compare x y
end)

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

let get_string_of_value = function
  | Integer i -> "Integer " ^ string_of_int i
  | Boolean b -> "Boolean " ^ string_of_bool b
  | _ -> "Closure"

let rec get_string_of_type = function
  | Integer_t -> "int"
  | Boolean_t -> "boolean"
  | Closure_t (domain, codomain) ->
      Printf.sprintf " ( %s -> %s ) "
        (get_string_of_type domain)
        (get_string_of_type codomain)

let compatible x y =
  let rec helper x y =
    match (x, y) with
    | Integer_t, Integer_t -> true
    | Boolean_t, Boolean_t -> true
    | Closure_t (t1, t2), Closure_t (t3, t4) -> helper t1 t3 && helper t2 t4
    | _ -> false
  in
  helper x y
