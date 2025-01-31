open Miniimp_ast
module ImpAst = Miniimp_ast

exception IllFormedAST of string

let parse_with_errors lexbuf =
  try
    let x = Miniimp_parser.prg Miniimp_lexer.read lexbuf in
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

let eval p input =
  let memory = Hashtbl.create 10000 in
  let rec eval_arithmetic expr =
    match expr with
    | Plus (x, y) -> eval_arithmetic x + eval_arithmetic y
    | Minus (x, y) -> eval_arithmetic x - eval_arithmetic y
    | Times (x, y) -> eval_arithmetic x * eval_arithmetic y
    | Aval x -> x
    | Substitue x -> (
        match Hashtbl.find_opt memory x with
        | None ->
            raise
              (IllFormedAST
                 (Printf.sprintf
                    "Accessed variable %s, which was not present in memory" x))
        | Some y -> y)
  in
  let rec eval_bool = function
    | And (x, y) -> eval_bool x && eval_bool y
    | Or (x, y) -> eval_bool x || eval_bool y
    | Not x -> not (eval_bool x)
    | Minor (x, y) -> eval_arithmetic x < eval_arithmetic y
    | Bval x -> x
  in
  let assign loc x = Hashtbl.replace memory loc (eval_arithmetic x) in
  let rec eval_command = function
    | Assign (loc, expr) -> assign loc expr
    | Sequence (c0, c1) ->
        eval_command c0;
        eval_command c1
    | Cond (b, t, e) -> if eval_bool b then eval_command t else eval_command e
    | While (b, c) ->
        if eval_bool b then (
          eval_command c;
          eval_command (While (b, c)))
    | Skip -> ()
  in
  try
    assign p.input (Aval input);
    assign p.output (Aval 0);
    eval_command p.command;
    Hashtbl.find_opt memory p.output
  with IllFormedAST _ -> None
