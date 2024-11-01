{
    open Miniimpparser
}

let character = ['a'-'Z']
let integer = ['0'-'9']+ (* Integer literals n*)
let boolean = "false"|"true" (* Booleans b *)
let variable = character(integer | character)* (* Any variable *)
let white = (' '|'\t')+|'\r'|'\n'|"\r\n" (* white spaces characters *)

rule read = parse
    | integer {INT(Stdlib.int_of_string integer)}
    | boolean as b {match b with | "false"-> BOOL(false) | _ -> BOOL(true)}
    | variable {VAR(variable)}
    | white {read lexbuf}
    | "if" {IF}
    | "then" {THEN}
    | "else" {ELSE}
    | "while" {WHILE}
    | "do" {DO}
    | ";" {SEQ}
    | "or" {OR}
    | "skip" {SKIP}
    | ":=" {ASSIGN}
    | "*" {MUL}
    | "+" {PLUS}
    | "-" {MINUS}
    | "and" {AND}
    | "not" {NOT}
    | "<" {LESS}
    | "def main with input "variable as v1" output "variable as v2" as" {MAIN(v1, v2)}
