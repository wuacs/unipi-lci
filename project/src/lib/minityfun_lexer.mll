{
    open Minityfun_parser
}

let character = ['a'-'z']
let natural = ['0'-'9']+
let boolean = "false"|"true"
let variable = character(natural|character)*
let white = 
    (' ' | '\t')*   |
    '\r'            | 
    '\n'            | 
    "\r\n"
rule read = parse
    | white {read lexbuf}
    | "(" {LEFT_PAR}
    | ")" {RIGHT_PAR}
    | "->" {TYPE_ARROW}
    | "=>" {FUN_ARROW}
    | "in" {IN}
    | ":" {TYPE_SEP}
    | "letfun" {LETFUN}
    | "fun" {FUN}
    | "else" {ELSE}
    | "then" {THEN}
    | "if" {IF}
    | "let" {LET}
    | "=" {EQUAL}
    | "and" {AND}
    | "<" {LESS}
    | "*" {MUL}
    | "-" {MINUS}
    | "+" {PLUS}
    | "int" {INT_TYPE}
    | "bool" {BOOL_TYPE}
    | boolean {match (Lexing.lexeme lexbuf) with | "false"-> BOOL(false) | _ -> BOOL(true)}
    | natural {INT(int_of_string (Lexing.lexeme lexbuf))}
    | variable {VAR(Lexing.lexeme lexbuf)} 
    | eof {EOF}
    | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }