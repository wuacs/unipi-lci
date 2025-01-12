{
    open Miniimp_parser
}

let character = ['a'-'z']
let natural = ['0'-'9']+
let boolean = "false"|"true" (* Booleans b *)
let variable = character(natural | character)* (* Any variable *)
let white = (' ' | '\t')* | '\r' | '\n' | "\r\n" (* whitespace characters *)

rule read = parse
    | white {read lexbuf}
    | "def main with input "(variable as v1)" output "(variable as v2)" as" {MAIN(v1, v2)}
    | "not" {NOT}
    | "while" {WHILE}
    | "do" {DO}
    | "skip" {SKIP}
    | "(" {LEFTPAR}
    | ")" {RIGHTPAR}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | ";" {SEQ}
    | "or" {OR}
    | ":=" {ASSIGN}
    | "*" {MUL}
    | "+" {PLUS}
    | "-" {MINUS}
    | "and" {AND}
    | "<" {LESS}
    | natural {INT(Stdlib.int_of_string (Lexing.lexeme lexbuf))}
    | boolean {match (Lexing.lexeme lexbuf) with | "false"-> BOOL(false) | _ -> BOOL(true)}
    | variable {VAR((Lexing.lexeme lexbuf))}
    | eof {EOF}
    | _ as c { failwith (Printf.sprintf "unexpected character: %C" c) }
    
