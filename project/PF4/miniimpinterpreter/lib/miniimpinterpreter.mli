open MiniImp

val parse_with_errors: Lexing.lexbuf -> program option

val eval: program -> int -> int option