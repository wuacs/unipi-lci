open Minityfun_ast

exception TypeError of string
(** The exception which is thrown by function {!type_check} *)

module TyFunAst = Minityfun_ast

(* Given an identifier, it updates its type. *)
let update_type_bind (var : var) (tau : tau) (type_env : tau Env_map.t) :
    tau Env_map.t =
  Env_map.add var tau type_env

let get_type_bind (var : var) (type_env : tau Env_map.t) : tau option =
  Env_map.find_opt var type_env

let get_string_of_operator (operator : op) =
  match operator with
  | Plus -> "+"
  | Minus -> "-"
  | And -> "and"
  | Mul -> "*"
  | Less -> "<="

let get_codomain (operation : op) =
  match operation with
  | Plus | Minus | Mul -> Integer_t
  | Less | And -> Boolean_t

let get_domain (operation : op) (operand : tau) : tau =
  match operation with
  | Plus | Minus | Mul | Less ->
      if compatible operand Integer_t then Integer_t
      else
        raise
          (TypeError
             (Printf.sprintf
                "Invalid operand type: expected integer for arithmetic or \
                 comparison operations, instead got %s"
                (get_string_of_type operand)))
  | And ->
      if compatible operand Boolean_t then Boolean_t
      else
        raise
          (TypeError
             (Printf.sprintf
                "Invalid operand type: expected boolean for and operation, \
                 instead got %s"
                (get_string_of_type operand)))

let type_of_literal (value : value) : tau =
  match value with
  | Integer _ -> Integer_t
  | Boolean _ -> Boolean_t
  | _ ->
      failwith "Only integer and boolean literals are allowed in this context."

let type_check (ast : ast) : tau option =
  let rec rec_type_check (ast : ast) (env : tau Env_map.t) : tau
      =
    match ast with
    | Fun (pname, ptype, a1) ->
        let t = rec_type_check a1 (update_type_bind pname ptype env) in
        Closure_t (ptype, t)
    | LetFun (fname, pname, ftype, a1, a2) -> (
        match ftype with
        | Closure_t (domain, range) ->
            let t =
              rec_type_check a1
                (update_type_bind pname domain
                   (update_type_bind fname ftype env))
            in
            if compatible range t <> true then
              raise
                (TypeError
                   (Printf.sprintf
                      "Function %s was declared to return type %s but returns \
                       %s"
                      fname (get_string_of_type range) (get_string_of_type t)))
            else
              rec_type_check a2 (update_type_bind fname ftype env)
        | _ ->
            raise
              (TypeError
                 "With a LetFun syntax you need to specify the type of a \
                  function"))
    | Let (var, a1, a2) ->
        let t = rec_type_check a1 env in
        rec_type_check a2 (update_type_bind var t env)
    | App (a1, a2) -> (
        let t = rec_type_check a2 env in
        let t1 = rec_type_check a1 env in
        match t1 with
        | Closure_t (d, c) ->
            if compatible d t <> true then
              raise
                (TypeError
                   (Printf.sprintf
                      "Domain does not correspond, expected a %s instead got a \
                       %s"
                      (get_string_of_type d) (get_string_of_type t)))
            else c
        | t ->
            raise
              (TypeError
                 (Printf.sprintf "Tried to apply to type %s"
                    (get_string_of_type t))))
    | Op (a1, op, a2) ->
        let t = rec_type_check a1 env in
        let t1 = rec_type_check a2 env in
        let operand_type = get_domain op t in
        if compatible t operand_type && compatible t1 operand_type <> true then
          raise
            (TypeError
               (Printf.sprintf
                  "Operand types do not match in binary operation %s, left \
                   hand side has type %s, right hand side %s"
                  (get_string_of_operator op)
                  (get_string_of_type t) (get_string_of_type t1)))
        else (get_codomain op)
    | If (a1, a2, a3) ->
        let t = rec_type_check a1 env in
        if compatible t Boolean_t <> true then
          raise
            (TypeError
               (Printf.sprintf
                  "If condition must be of Boolean_t type, instead has type %s"
                  (get_string_of_type t)))
        else
          let t1 = rec_type_check a2 env in
          let t2 = rec_type_check a3 env in
          if compatible t1 t2 <> true then
            raise
              (TypeError
                 (Printf.sprintf
                    "If branches must have the same type. `True` branch has \
                     type %s and `False` branch has type %s"
                    (get_string_of_type t1) (get_string_of_type t2)))
          else t1
    | Val value -> type_of_literal value
    | Var var -> (
        match get_type_bind var env with
        | Some t -> t
        | None ->
            raise
              (TypeError
                 ("Variable " ^ var ^ " is unbound in the current environment."))
        )
  in
  try Some (rec_type_check ast Env_map.empty)
  with TypeError msg ->
    print_endline msg;
    None

let eval (ast : ast) =
  let update_env (var : var) value (env : value_env) =
    Env_map.add var value env
  in
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

let parse_with_errors lexbuf =
  try
    let x = Minityfun_parser.prog Minityfun_lexer.read lexbuf in
    Some x
  with
  | Parsing.Parse_error ->
      Printf.eprintf "Syntax error at line %d, position %d\n"
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol);
      None
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      None

let interpret_from_file (filename : string) : value * tau =
  let ic = open_in filename in
  Fun.protect
    ~finally:(fun _ -> close_in ic)
    (fun _ ->
      match parse_with_errors (Lexing.from_channel ic) with
      | Some ast -> (
          match type_check ast with
          | None -> failwith "Type checking failed"
          | Some t -> (eval ast, t))
      | None -> failwith "Error while parsing")
