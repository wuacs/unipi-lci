exception TypeError of string
(** The exception which is thrown by function {!type_check} *)

module StringCompare = struct
  type t = string
  let compare = String.compare
end

module Env_map = Map.Make (StringCompare)

type var = string
type op = Plus | Minus | And | Mul | Less | Equal

type ast =
  | LetFun of var * var * tau * ast * ast
  | Fun of var * tau * ast
  | Let of var * ast * ast
  | Op of ast * op * ast
  | If of ast * ast * ast
  | App of ast * ast
  | Val of value
  | Var of var

and tau = Integer_t | 
          Boolean_t |
          Closure_t of tau * tau |
          Variable_t of var * context

and context = Domain 
            | Codomain
            | Value

and value =
  | Integer of int
  | Boolean of bool
  | RecClosure of var * var * ast * value_env
  | Closure of var * ast * value_env

and value_env = value Env_map.t
and type_env = tau Env_map.t

(* Given an identifier, it updates its type. *)
let update_type_bind (var : var) (tau : tau) (type_env : tau Env_map.t) : tau Env_map.t = 
  Env_map.add var tau type_env

let remove_type_bind (var : var) (type_env : tau Env_map.t) : tau Env_map.t =
  Env_map.remove var type_env

let get_type_bind (var : var) (type_env : tau Env_map.t) : tau option = 
  Env_map.find_opt var type_env

let unify (env1 : tau Env_map.t) (env2 : tau Env_map.t) : tau Env_map.t = 
  Env_map.merge
    (fun _ k1 k2 ->
      match (k1, k2) with
      | Some v1, Some v2 -> (
          match (v1, v2) with
          | (Closure_t (par_t, out_t), Closure_t (par_t1, out_t1)) -> (
              if par_t <> par_t1 then
                raise (TypeError "Unification failed: parameter types do not match in closure.")
              else
                match out_t1 with
                | Variable_t(_, _) -> Some v1
                | actual_out_t1 -> (
                    match out_t with 
                    | Variable_t(_, _) -> Some v2
                    | actual_out_t -> 
                        if actual_out_t <> actual_out_t1 then 
                          raise (TypeError "Unification failed: output types in closure do not match.")
                        else Some v1))
          | _ ->
              if v1 <> v2 then raise (TypeError "Unification failed: types do not match.")
              else Some v1)
      | Some v1, None -> Some v1
      | None, Some v2 -> Some v2
      | None, None -> None)
    env1 env2

let get_codomain (operation : op) = 
  match operation with
  | Plus | Minus | Mul  -> Integer_t
  | Less | Equal | And -> Boolean_t

let get_type_variable (type_var : tau) (type_env : tau Env_map.t) : tau = 
  match type_var with 
    | Variable_t(var_name, context) -> (
        let actual_type = get_type_bind var_name type_env in
        match actual_type with
        | None -> raise (TypeError ("Type variable " ^ var_name ^ " is unbound and its type cannot be determined."))
        | Some t -> (match context with
                    | Domain -> (
                        match t with 
                        | Closure_t(x, _) -> x
                        | _ -> raise (TypeError "Type variable expected in domain context does not match closure structure."))
                    | Codomain -> (
                        match t with
                        | Closure_t(_, y) -> y
                        | _ -> raise (TypeError "Type variable expected in codomain context does not match closure structure."))
                    | Value -> t))
    | _ -> raise (TypeError ("Provided a non-type variable to the function"))

let get_domain (operation : op) (operand : tau) (type_env : tau Env_map.t) : tau  = 
  let operand =   
  match operand with 
    | Variable_t(var_name, _) -> (
        let actual_type = get_type_bind var_name type_env in
        match actual_type with
        | None -> raise (TypeError ("Type variable " ^ var_name ^ " is unbound and its type cannot be determined."))
        | Some t -> t)
    | _ -> operand in
  match operation with
  | Plus | Minus | Mul | Less -> (
      match operand with
      | Integer_t -> Integer_t
      | _ -> raise (TypeError "Invalid operand type: expected Integer_t for arithmetic or comparison operations."))
  | And -> (
      match operand with
      | Boolean_t -> Boolean_t
      | _ -> raise (TypeError "Invalid operand type: expected Boolean_t for logical 'And' operation."))
  | Equal -> operand

let type_of_literal (value : value) : tau = 
  match value with
  | Integer _ -> Integer_t
  | Boolean _ -> Boolean_t
  | _ -> failwith "Only integer and boolean literals are allowed in this context."

let update_type_variable (type_var : tau) (type_env : tau Env_map.t) (actual_type : tau) : tau Env_map.t = 
  match type_var with
  | Variable_t(var_id, context) -> (
    let context_type = get_type_bind var_id type_env in
    match context with
    | Domain -> (
        match context_type with 
        | Some Closure_t(_, y) -> update_type_bind var_id (Closure_t(actual_type, y)) type_env
        | _ -> raise (TypeError "Type variable expected in domain context does not match closure structure."))
    | Codomain -> (
        match context_type with
        | Some Closure_t(x, _) -> update_type_bind var_id (Closure_t(x, actual_type)) type_env
        | _ -> raise (TypeError "Type variable expected in codomain context does not match closure structure."))
    | Value -> (
        match context_type with 
        | Some Integer_t when actual_type = Integer_t -> type_env
        | Some Boolean_t when actual_type = Boolean_t -> type_env
        | _ -> raise (TypeError "Type mismatch in value context: expected integer or boolean type.")))
  | _ -> raise (TypeError "Attempt to update non-variable type encountered.")

let type_check (ast : ast) : tau option =
  let rec rec_type_check (ast : ast) (env : tau Env_map.t) : tau * tau Env_map.t =
    match ast with
    | Fun (pname, ptype, a1) ->
        let t, re = rec_type_check a1 (update_type_bind pname ptype env) in
        (t, remove_type_bind pname re)
    | LetFun (fname, pname, ptype, a1, a2) ->
        let t, re =
          rec_type_check a1
            (update_type_bind pname ptype
               (update_type_bind fname (Closure_t (ptype, Variable_t(fname, Codomain))) env))
        in
        let t1, re1 = rec_type_check a2 (update_type_bind fname (Closure_t (ptype, t)) re) in
        (t1, remove_type_bind fname re1)
    | Let (pname, a1, a2) ->
        let t, re = rec_type_check a1 env in
        rec_type_check a2 (update_type_bind pname t (unify re env))
    | App (a1, a2) -> (
        let t, re = rec_type_check a2 env in
        let t1, re1 = rec_type_check a1 (unify re env) in
        match t1 with
        | Closure_t (_, Variable_t(x, y)) -> (Variable_t(x, y), re1)
        | Closure_t (dt, ot) ->
            if dt <> t then raise (TypeError "Function application failed: argument type does not match function domain.")
            else (ot, re1)
        | _ -> raise (TypeError "Application error: expected closure type for function application."))
    | Op (a1, op, a2) ->
        let t, re = rec_type_check a1 env in
        let t1, re1 = rec_type_check a2 (unify re env) in
        let (re1, t) = 
          (match t with 
           | Variable_t(_, _) -> (
              let new1 = update_type_variable t re1 (get_domain op t1 re1) in
              (new1, (get_type_variable t new1)))
           | _ -> (re1, t)) in
        let (re1, t1) = 
          (match t1 with 
           | Variable_t(_, _) -> (
              let new1 = update_type_variable t1 re1 (get_domain op t re1) in
              (new1, (get_type_variable t1 new1)))
           | _ -> (re1, t)) in
        let operand_type1 = get_domain op t re1 in
        if operand_type1 <> (get_domain op t1 re1) then
          raise (TypeError "Operand types do not match in binary operation.")
        else (get_codomain op, re1)
    | If (a1, a2, a3) ->
        let t, re = rec_type_check a1 env in
        if t <> Boolean_t then raise (TypeError "If condition must be of Boolean_t type.")
        else
          let t1, re1 = rec_type_check a2 (unify re env) in
          let t2, re2 = rec_type_check a3 (unify re1 env) in
          if t1 <> t2 then raise (TypeError "If branches must have the same type.")
          else (t1, unify re2 env)
    | Val value -> (type_of_literal value, env)
    | Var var -> (
        match get_type_bind var env with
        | Some t -> (t, env)
        | None -> raise (TypeError ("Variable " ^ var ^ " is unbound in the current environment.")))
  in
  try Some (fst (rec_type_check ast Env_map.empty))
  with TypeError msg -> (print_endline msg; None)
