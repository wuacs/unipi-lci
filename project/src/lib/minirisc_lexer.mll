{
    open Minirisc_parser
}

let character = ['a'-'z']|':'
let integer = '-'?['0'-'9']+
let boolean = "false"|"true"
let label = character(integer|character)*
let white = 
    (' ' | '\t')*   |
    '\r'            | 
    '\n'            | 
    "\r\n"
rule read = parse
    | white {read lexbuf}
    | "nop" {NOP}
    | "copy" {COPY}
    | "not" {NOT}
    | "add" {ADD}
    | "addI" {ADDI}
    | "sub" {SUB}
    | "subI" {SUBI}
    | "mult" {MULT}
    | "multI" {MULTI}
    | "and" {AND}
    | "less" {LESS}
    | "andI" {ANDI}
    | "=>" {RHS}
    | "store" {STORE}
    | "load" {LOAD}
    | "loadI" {LOADI}
    | "jump" {JUMP}
    | "cjump" {CJUMP}
    | integer {INT(int_of_string (Lexing.lexeme lexbuf))}
    | label {LABEL (Lexing.lexeme lexbuf ) } 
    | eof {EOF}
    | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }