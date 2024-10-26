exception TypeError of string
(** The exception which is thrown by function {!type_check} *)

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

and tau = Not_Defined of var | Integer_t | Boolean_t | Closure_t of tau * tau

and value =
  | Integer of int
  | Boolean of bool
  | RecClosure of var * var * ast * value_env
  | Closure of var * ast * value_env

and value_env = value EnvMap.t
and type_env = tau EnvMap.t

let type_check ast =
  let mergeEnv env1 env2 =
    EnvMap.merge
      (fun _x k1 k2 ->
        match (k1, k2) with
        | Some v1, Some v2 -> (
            match v2 with
            | Closure_t (_, Not_Defined _) -> Some v1
            | _ ->
                if v1 <> v2 then raise (TypeError "Type checking failed")
                else Some v1)
        | Some v1, None -> Some v1
        | None, Some v2 -> Some v2
        | None, None -> None)
      env1 env2
  in
  let getOperationType operation =
    match operation with
    | Plus | Minus | Mul -> Integer_t
    | Less | Equal | And -> Boolean_t
  in
  let getOperandType operation operand =
    match operation with
    | Plus | Minus | Mul | Less -> (
        match operand with
        | Integer_t | Not_Defined _ -> Integer_t
        | _ -> raise (TypeError "Type checking failed"))
    | And -> (
        match operand with
        | Boolean_t | Not_Defined _ -> Boolean_t
        | _ -> raise (TypeError "Type checking failed"))
    | Equal -> (
        match operand with
        (* If you do Equal(Undefined, Undefined) it doesnt pass the tc *)
        | Not_Defined _ -> raise (TypeError "Type checking failed")
        | x -> x)
  in
  let updateEnv var tau typing_env = EnvMap.add var tau typing_env in
  let removeBinding var typing_env = EnvMap.remove var typing_env in
  let getBinding var typing_env = EnvMap.find_opt var typing_env in
  let getValueType value =
    match value with
    | Integer _ -> Integer_t
    | Boolean _ -> Boolean_t
    | _ -> failwith "Only integer and boolean literals allowed"
  in
  let rec rec_type_check : ast -> tau EnvMap.t -> tau * tau EnvMap.t =
   fun ast env ->
    match ast with
    | Fun (pname, ptype, a1) ->
        let t, re = rec_type_check a1 (updateEnv pname ptype env) in
        (t, removeBinding pname re)
    | LetFun (fname, pname, ptype, a1, a2) ->
        let _, re =
          rec_type_check a1
            (updateEnv pname ptype
               (updateEnv fname (Closure_t (ptype, Not_Defined fname)) env))
        in
        let t1, re1 = rec_type_check a2 (removeBinding pname re) in
        (t1, removeBinding fname re1)
    | Let (pname, a1, a2) ->
        let t, re = rec_type_check a1 env in
        rec_type_check a2 (updateEnv pname t (mergeEnv re env))
    | App (a1, a2) -> (
        let t, re = rec_type_check a2 env in
        let t1, re1 = rec_type_check a1 (mergeEnv re env) in
        match t1 with
        | Closure_t (dt, Not_Defined x) ->
            if dt <> t then raise (TypeError "Type checking failed")
            else (Not_Defined x, re1)
        | Closure_t (dt, ot) ->
            if dt <> t then raise (TypeError "Type checking failed")
            else (ot, re1)
        | _ -> raise (TypeError "Type checking failed"))
    | Op (a1, op, a2) ->
        let helper : tau -> tau -> tau EnvMap.t -> op -> tau * tau EnvMap.t =
         fun t t1 e o ->
          match t with
          | Not_Defined x -> (
              let t2 = getOperandType o t1 in
              match getBinding x e with
              | Some (Closure_t (dt, _)) ->
                  (t2, updateEnv x (Closure_t (dt, t2)) e)
              | _ -> raise (TypeError "Type checking failed"))
          | _ -> (getOperandType o t, e)
        in
        let t, re = rec_type_check a1 env in
        let t1, re1 = rec_type_check a2 (mergeEnv re env) in
        let _, re2 = helper t t1 (mergeEnv re1 env) op in
        let _, re3 = helper t1 t re2 op in
        (getOperationType op, re3)
    | If (a1, a2, a3) ->
        let t, re = rec_type_check a1 env in
        if t <> Boolean_t then raise (TypeError "Type checking failed");
        let t1, re1 = rec_type_check a2 (mergeEnv re env) in
        let t2, re2 = rec_type_check a3 (mergeEnv re1 env) in
        if t1 <> t2 then raise (TypeError "Type checking failed")
        else (t1, mergeEnv re2 env)
    | Val value -> (getValueType value, env)
    | Var var -> (
        match getBinding var env with
        | Some t -> (t, env)
        | None -> raise (TypeError "Type checking failed"))
  in
  try
    let x = rec_type_check ast EnvMap.empty in
    Some (fst x)
  with TypeError _ -> None
