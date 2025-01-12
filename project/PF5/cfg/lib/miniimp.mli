module ImpAst = Miniimp_ast

val parse_with_errors: Lexing.lexbuf -> ImpAst.program option

val eval: ImpAst.program -> int -> int option