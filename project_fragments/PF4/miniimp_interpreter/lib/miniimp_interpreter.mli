val parse_with_errors: Lexing.lexbuf -> Miniimp_ast.program option

val eval: Miniimp_ast.program -> int -> int option