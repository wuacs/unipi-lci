exception TypeError of string
(** The exception which is thrown by function {!type_check} *)

module StringCompare = struct
  type t = string

  let compare = String.compare
end

module EnvMap = Map.Make (StringCompare)

type var = string
type infering = 
  | Dom of var 
  | Cod of var 
  | Nor of var

module InferingCompare = struct
  type t = infering
  
  let compare x y = let x = match x with | Dom(t) -> t | Cod(t) -> t | Nor(t) -> t in 
                    let y = match y with | Dom(t) -> t | Cod(t) -> t | Nor(t) -> t in
                    String.compare x y
end

module InferingMap = Map.Make (InferingCompare)

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

and tau = Integer_t | Boolean_t | Closure_t of tau * tau | Not_Defined of var

and value =
  | Integer of int
  | Boolean of bool
  | RecClosure of var * var * ast * value_env
  | Closure of var * ast * value_env

and value_env = value EnvMap.t
and type_env = tau EnvMap.t
and infering_env = infering EnvMap.t

let type_check ast =
  (* You need to use this function in the code, only problem, when you find an equal they can be both int and boolean... *)
  let helper_force_cast: tau -> tau -> tau InferingMap.t -> tau InferingMap.t = fun source guard map ->
    match source with 
    | Not_Defined(infering) -> (match (InferingMap.find_opt (Nor infering) map) with
                                  | Some(x) ->( match x with 
                                                | Not_Defined(_) -> InferingMap.add (Nor infering) guard map
                                                | t -> if t<>guard then raise (TypeError "Type checking failed") else map)
                                  | None -> (InferingMap.add (Nor infering) guard map))
    | _ -> map
    
  in
  let getOperationType operation =
    match operation with
    | Plus | Minus | Mul -> Integer_t
    | Less | And -> Boolean_t
    | _ -> failwith "Equal is defined on multiple types"
  in
  let helper_merge_bindings map1 map2 = 
    InferingMap.merge
    (fun _x k1 k2 ->
      match (k1, k2) with
      | Some v1, Some v2 -> (
          if v1 <> v2 then raise (TypeError "Type checking failed")
          else Some v1)
      | Some v1, None -> Some v1
      | None, Some v2 -> Some v2
      | None, None -> None) map1 map2
  in
  let helper_remove_binding infering map = 
    InferingMap.remove infering map
  in 
  let helper_check_consistency infering map guard = 
    match (InferingMap.find_opt infering map) with
    | Some(x) -> if x <> guard then raise (TypeError "Type checking failed")
    | None -> () 
  in
  let getValueType value =
    match value with
    | Integer _ -> Integer_t
    | Boolean _ -> Boolean_t
    | _ -> failwith "Only integer and boolean literals allowed"
  in
  let rec rec_type_check : ast -> tau * tau InferingMap.t =
   fun ast ->
    match ast with
    | Fun (pname, ptype, a1) ->
        let (b, map) = rec_type_check a1 in
        helper_check_consistency (Nor pname) map ptype;
        (Closure_t(ptype, b), map)
    | LetFun (fname, pname, ptype, a1, a2) -> 
        (let (b, map) = rec_type_check a1 in
        helper_check_consistency (Dom fname) map ptype;
        helper_check_consistency (Nor pname) map ptype;
        let (b1, map1) = rec_type_check a2 in 
        helper_check_consistency (Cod fname) map b;
        helper_check_consistency (Dom fname) map ptype;
        (b1, helper_remove_binding (Dom fname) (helper_remove_binding (Cod fname) map1)))
    | Let (vname, a1, a2) ->
      ( let (b, map) = rec_type_check a1 in 
        let (b1, map1) = rec_type_check a2 in
        let map1 = helper_merge_bindings map1 map in
        (match b with 
        | Closure_t(t1, t2) -> begin 
                                  (helper_check_consistency (Dom vname) map1 t1);
                                  (helper_check_consistency (Cod vname) map1 t2); 
                                  (b1, helper_remove_binding (Dom vname) (helper_remove_binding (Cod vname) map1))
                                end
        | x ->  begin 
                  helper_check_consistency (Nor vname) map1 x;
                  (b1, helper_remove_binding (Nor vname) map1)
                end)
      )
    | App (a1, a2) -> (let (b, map) = rec_type_check a2 in
                      let (b1, map1) = rec_type_check a1 in
                      (match b1 with 
                      | Closure_t(t1, _) -> if t1 <> b then raise (TypeError "Type checking failed");
                      | _ -> raise (TypeError "Type checking failed")); 
                      (b1, helper_merge_bindings map map1))
    | Op(a1, op, a2) -> (let (b, map) = rec_type_check a1 in 
                        let (b1, map1) = rec_type_check a2 in 
                        match op with
                        | Equal -> (match (b, b1) with 
                                  | Not_Defined _, Not_Defined _ -> (Boolean_t, helper_merge_bindings map (helper_force_cast b1 b map1))
                                  | Not_Defined _, typ -> (Boolean_t, helper_merge_bindings (helper_force_cast b typ map) map1)
                                  | typ, Not_Defined _ -> (Boolean_t, helper_merge_bindings map (helper_force_cast b1 typ map1))
                                  | typ1, typ2 -> if typ1 <> typ2 then raise (TypeError "Type checking failed") else (Boolean_t, helper_merge_bindings map map1))
                        | x -> let typ = getOperationType(x) in (typ, helper_merge_bindings (helper_force_cast b typ map) (helper_force_cast b1 typ map1)))
    | If(a1, a2, a3) -> (let (b, map) = rec_type_check a1 in
                        if b<>Boolean_t then raise (TypeError "Type checking failed");
                        let (b1, map1) = rec_type_check a2 in
                        let (b2, map2) = rec_type_check a3 in
                        if b1<>b2 then raise (TypeError "Type checking failed");
                        (b2, (helper_merge_bindings map (helper_merge_bindings map1 map2))))
    | Val(v) -> (getValueType(v), InferingMap.empty)
    | Var(x) -> (Not_Defined x, InferingMap.add (Nor x) (Not_Defined x) InferingMap.empty) 
              in
    try
      let (x, map) = rec_type_check ast in
      if InferingMap.is_empty map then Some(x) else raise (TypeError "Type checking failed")
    with TypeError _ -> None         
