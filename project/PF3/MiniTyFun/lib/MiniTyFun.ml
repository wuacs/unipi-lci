(** The exception which is thrown by function {!type_check} *)
exception TypeError of string

module StringCompare = struct
  type t = string

  let compare = String.compare
end

module EnvMap = Map.Make (StringCompare)

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

and tau = 
  | Not_Defined
  | Integer_t
  | Boolean_t
  | Closure_t of tau * tau 

and value =
  | Integer of int
  | Boolean of bool
  | RecClosure of var * var * ast * value_env
  | Closure of var * ast * value_env

and value_env = value EnvMap.t

and type_env = tau EnvMap.t

let type_check ast = 
  let updateEnv var tau typing_env = EnvMap.add var tau typing_env in
  let rec rec_type_check ast env =
  match ast with
    | Fun (var, tau, ast1) -> Closure_t(tau, (rec_type_check ast1 (updateEnv var tau env)))
    | App (f, par) -> (
        match rec_type_check f env with
        | Closure_t (t1, t2) -> (let partype = (rec_type_check par env) in 
                                if partype = t1 then t2 else raise (TypeError "Type checking failed"))
        | _ -> raise (TypeError "Type checking failed"))
    | Op (a1, op, a2) -> (
        let a1 = rec_type_check a1 env in
        let a2 = rec_type_check a2 env in
        match op with
        | Plus | Minus | Mul | Less -> (
            match (a1, a2) with
            | Integer_t, Integer_t -> Integer_t
            | _ -> raise (TypeError "operands do not match binary integer operator"))
        | And -> (
            match (a1, a2) with
            | Boolean_t, Boolean_t -> Boolean_t
            | _ -> raise (TypeError "operands do not match binary boolean operator"))
        | Equal -> (
            match (a1, a2) with
            | Boolean_t, Boolean_t | Integer_t, Integer_t-> Boolean_t
            | _ -> raise (TypeError "operands do not match == operator")))
    | If (a1, a2, a3) -> (
        let cond = rec_type_check a1 env in
        match cond with
        | Boolean_t ->  let t1 = rec_type_check a2 env in 
                        let t2 = rec_type_check a3 env in
                        if t1 <> t2 then raise  (TypeError "Type checking failed") else t1
        | _ -> raise ( TypeError"Type checking failed"))
    | Let (var, a1, a2) -> rec_type_check a2 (updateEnv var (rec_type_check a1 env) env)
    | LetFun (f, var, t, a1, a2) ->
        rec_type_check a2 (updateEnv f (Closure_t(t, rec_type_check a1 (updateEnv var t (updateEnv f (Closure_t(t, Not_Defined)) env)))) env)
    | Var x -> (try EnvMap.find x env with _ -> failwith ("non ho trovato " ^ x))
    | Val x -> (match x with 
                  | Integer _ -> Integer_t
                  | Boolean _ -> Boolean_t
                  | Closure(v, a, _) | RecClosure(_, v, a, _) -> 
                    let tau_par = EnvMap.find v env in
                    let body_par = rec_type_check a (updateEnv v tau_par env) in
                    Closure_t(tau_par, body_par))
  in
  try let x = (rec_type_check ast EnvMap.empty) in Some(x) with TypeError _ -> None
