  open Minityfun_ast

  (** The exception which is thrown by function {!type_check} *)
  exception TypeError of string

  (* Given an identifier, it updates its type. *)
  let update_type_bind (var : var) (tau : tau) (type_env : tau Env_map.t) : tau Env_map.t = 
    Env_map.add var tau type_env

  let remove_type_bind (var : var) (type_env : tau Env_map.t) : tau Env_map.t =
    Env_map.remove var type_env

  let get_type_bind (var : var) (type_env : tau Env_map.t) : tau option = 
    Env_map.find_opt var type_env

  (** This function merges two type environmnets which {b need} to be
  coherent, i.e. if {b t} has type {b t'} in {b env1} then if {b env2} has
  a binding for {b t} with {b t''} then {b t'} = {b t''}, and returns
  the merged environment.

  If the two environments are not coherent the function raises 
  a {b TypeError} exception. *)
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

  (** Returns for an operation {b op} in the AST its codomain type *)
  let get_codomain (operation : op) = 
    match operation with
    | Plus | Minus | Mul  -> Integer_t
    | Less | And -> Boolean_t

  (** This function returns for a given type variable {b type_var} 
  its type in the type enviornment {b type_env}, thus 
  this function is to be used only in moments where the type infering
  algorithm needs the actual type of a type variable, because otherwise
  can not proceed.

  {b Note}: raises a {b TypeError} exception if:
    
  1) the function was invoked without a type variable.
  2) {b type_env} does not contain the type of {b type_var}
  3) In the case of type variables referring to a function context(i.e. a 
  domain or codomain) if the variable refers to something that happens
    not to be a function in the provided {b type_env}.
  *)
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

  (** Returns the domain of the parameter {b operation} given that one
  operand has type {b operand} in the type environment {b type_env}.

  {b Note 1}: Other than doing type-checking with the one's operand type 
  this information can be used to enrich the language with an {b ==} operator.

  {b Note 2}: raises a {b TypeError} exception if the operand is inferred not
  to be valid for the given operation, given the type environment {b type_env}.
  *)
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

  (** Given a first-class value {b value} returns its type iff 
  it is a literal character, i.e. an integer or a boolean. *)
  let type_of_literal (value : value) : tau = 
    match value with
    | Integer _ -> Integer_t
    | Boolean _ -> Boolean_t
    | _ -> failwith "Only integer and boolean literals are allowed in this context."

  (** This function {i tries} to udpate the type variable referenced by 
  {b type_var} in the typing environmnent {b type_env} with the type 
  {b actual_type}.

  The meaning behind this function is that of the type infering algorithm 
  "committing" a type for a type variable.

  {b Note}: this function raises a {b TypeError} exception if


  - the actual variable {b type_var} refers to is already bound in 
  {b type_env} to a different type than the one we are trying to insert.


  - In the {b context} of the type variable {b type_var} we already have
  a binding in {b type_env} but its type differs from the one we are trying to add. *)
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

  (** let rec give_type x = match x with 
  | Integer_t -> print_string "int\n"
  | Boolean_t  ->  print_string "bool\n"
  | Closure_t(x, y) -> print_string "closure\n"; give_type x;  give_type y; print_string "end closure\n" 
  | Variable_t(x, _) -> print_string "variable type " ; print_string x;;*)

  (** The type checking algorithm*)
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

  let parse_with_errors lexbuf =
    try 
      let x = Minityfun_parser.prog Minityfun_lexer.read lexbuf in Some(x)
    with
    | Parsing.Parse_error ->
        (Printf.eprintf "Syntax error at line %d, position %d\n"
        (lexbuf.lex_curr_p.pos_lnum)
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol));
        None
    | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      None


  let eval (ast : ast) =
    let update_env (var : var) value (env : value_env) = Env_map.add var value env in
    let rec rec_eval ast env =
      match ast with
      | Fun (var, _, ast) -> Closure (var, ast, env)
      | App (f, par) -> (
          match rec_eval f env with
          | Closure (v, a, e) -> rec_eval a (update_env v (rec_eval par env) e)
          | RecClosure (f, v, a, e) ->
              rec_eval a
                (update_env f
                    (RecClosure (f, v, a, e))
                    (update_env v (rec_eval par env) e))
          | _ -> failwith "Application failed")
      | Op (a1, op, a2) -> (
          let a1 = rec_eval a1 env in
          let a2 = rec_eval a2 env in
          match op with
          | Plus -> (
              match (a1, a2) with
              | Integer i, Integer j -> Integer (i + j)
              | _ -> failwith "operands do not match Plus operator")
          | Minus -> (
              match (a1, a2) with
              | Integer i, Integer j -> Integer (i - j)
              | _ -> failwith "operands do not match Minus operator")
          | Mul -> (
              match (a1, a2) with
              | Integer i, Integer j -> Integer (i * j)
              | _ -> failwith "operands do not match Mul operator")
          | Less -> (
              match (a1, a2) with
              | Integer i, Integer j -> Boolean (i < j)
              | _ -> failwith "operands do not match Less operator")
          | And -> (
              match (a1, a2) with
              | Boolean i, Boolean j -> Boolean (i && j)
              | _ -> failwith "operands do not match And operator"))
      | If (a1, a2, a3) -> (
          let cond = rec_eval a1 env in
          match cond with
          | Boolean true -> rec_eval a2 env
          | Boolean false -> rec_eval a3 env
          | _ -> failwith "Condition of If is not a Boolean value")
      | Let (var, a1, a2) -> rec_eval a2 (update_env var (rec_eval a1 env) env)
      | LetFun (f, p, _, a1, a2) ->
          rec_eval a2 (update_env f (RecClosure (f, p, a1, env)) env)
      | Var x -> Env_map.find x env
      | Val x -> x
    in
    rec_eval ast Env_map.empty
      
  let interpret (lexbuf : Lexing.lexbuf) : Minityfun_ast.value =
    match parse_with_errors lexbuf with
    | None -> failwith "parsing failed"
    | Some(x) -> (match type_check x with 
                  | None -> failwith "type checking failed"
                  | Some(_) -> (eval x))